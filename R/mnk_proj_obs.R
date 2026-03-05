#' @title mnk_proj_obs
#' Download observations from a Minka project
#' @description This function downloads observation data for a Minka project,in a given year y,
#' optionally, a specific month. It automatically manages if you download it
#' must be done month by month or for a single month.
#' @details
#' Before using this function, you need to know the \code{project_id}.
#' If the ID is unknown, you can first use the function \code{\link{mnk_proj_byname}}
#' to find it based on the project's name.
#' @param project_id A single id for a Minka project.
#' @param year Return all observations of project only in that year (can only be
#'  one year, not a range of years).
#' @param month A number from 1 to 12 to download only that month.
#' If it is NULL, the entire year will be downloaded.
#' @return A data frame with all the year/month records with full details.
#' @examples \dontrun{
#'   #First of all it is necessary to obtain the project_id of the project.
#'    projects <- mnk_proj_byname(query="Biomarato 2025")
#'
#'   #Select the id_project
#'   obs_june_2025 <- mnk_proj_obs( projects$project_id[1], year=2025, month=6)
#'   obs_2025 <- mnk_proj_obs( projects$project_id[1], year=2025, month=NULL)
#' }
#' @importFrom utils read.csv
#' @export

#Main function; download only a month or a full year calling function download_month_data()

mnk_proj_obs <- function(project_id, year, month = NULL) {

  if (is.null(project_id) || is.null(year)) stop("You must provide 'project_id' and 'year'")
  if (!is.null(month) && (is.na(as.numeric(month)) || as.numeric(month) < 1 || as.numeric(month) > 12)) {
    stop("'month' must be a number between 1 and 12.")
  }

  if (is.null(month)) {
    # --- INICIO DE LA MEJORA ---
    # 1. Hacemos un "ping" para TODO el año
    message(paste0("--- STARTING ANNUAL PROJECT ID= ", project_id, " DOWNLOAD FOR THE YEAR ", year, " ---"))
    message("--- Performing initial check for yearly data... ---")

    yearly_ping_params <- list(project_id = project_id, year = year, per_page = 1)
    ping_response <- httr::GET("https://api.minka-sdg.org/v1/observations", query = yearly_ping_params)

    if (httr::http_error(ping_response)) {
      message("Yearly PING query failed. Cannot proceed.")
      return(tibble::tibble())
    }

    yearly_total <- httr::content(ping_response, as = "parsed")$total_results

    # 2. Si no hay datos en todo el año, nos detenemos aquí.
    if (is.null(yearly_total) || yearly_total == 0) {
      message("No records found for the entire year.")
      final_data <- NULL # Opcional: inicializamos la variable
    } else {
      message(paste("Found", yearly_total, "total records for the year. Proceeding month by month..."))
      # 3. Solo si hay datos, procedemos con el bucle de 12 meses.
      final_data <- purrr::map_dfr(1:12, ~download_month_data(
        project_id = project_id,
        year = year,
        current_month =.x
      ))
    }
    # --- FIN DE LA MEJORA ---

  } else {
    # El modo mensual se queda exactamente igual.
    message(paste0("--- STARTING MONTHLY PROJECT ID =", project_id, " DOWNLOAD FOR MONTH ", month.name[month], " ---"))
    final_data <- download_month_data(project_id = project_id, year = year, current_month = month)
  }

  message("\n--- FINISHING... ---")

  if (is.null(final_data) || nrow(final_data) == 0) {
    message("No data could be downloaded for the specified criteria.")
    return(tibble::tibble())
  }

  message(paste0("Download complete! A total of ", nrow(final_data), " records were obtained."))
  return(final_data)
}




#' Internal function only to download the records of a month.
#'
#' @description Monthly register download function. Download a month of data either day by day
#' or all at once (>10.000 rec).This functions calls the function get_minka_obs.
#' @param project_id id given for main function.
#' @param year year given by main function.
#' @param current_month month given by main function or internally for the
#' calculations of the loops.
#' @return A data frame with a monthly records.
#' @noRd
download_month_data <- function(project_id, year, current_month) {
  base_url <- "https://api.minka-sdg.org"
  api_path <- "v1/observations"

  month_name <- month.name[current_month]
  message(paste0("\n--- Evaluating month: ", current_month, " (", month_name, " ", year, ") ---"))

  base_params <- list(project_id = project_id, year = year, month = current_month)
  ping_params <- c(base_params, list(per_page = 1, page = 1))
  ping_response <- httr::GET(base_url, path = api_path, query = ping_params)

  if (httr::http_error(ping_response)) { message(paste(" The PING query failed for month ", month_name)); return(tibble::tibble()) }

  monthly_total <- httr::content(ping_response, as = "parsed")$total_results
  if (is.null(monthly_total) || monthly_total == 0) { message(paste(" No records were found for ", month_name," ", year)); return(tibble::tibble()) }

  message(paste(" Month of ", month_name, "has ", monthly_total, "records."))

  if (monthly_total <= 10000) {
    message(" -> Total records less than 10.000. Downloading full month...")
    return(get_minka_obs(params = base_params, total_res = monthly_total))
  } else {
    message(paste(" -> Total records greater than 10.00. SUBDIVIDING BY DAY..."))

    days_in_month <- as.numeric(format(as.Date(paste0(year, "-", current_month + 1, "-01")) - 1, "%d"))

    purrr::map_dfr(1:days_in_month, function(current_day) {
      message(paste(" - Downloadind day:", current_day))
      daily_params <- list(project_id = project_id, year = year, month = current_month, day = current_day)
      get_minka_obs(params = daily_params)
    })
  }
}

#' Internal function to process the raw list of results into a tibble
#' @description Internal function to process the raw list of results into a tibble
#' @param all_results list of parameters given for get_minka_obs function.
#' @return the final tibble
#' @noRd
process_minka_results <- function(all_results) {
  if (length(all_results) == 0) {
    return(tibble::tibble())
  }
  purrr::map_dfr(all_results, ~tibble::tibble(
    id =.x$id %||% NA,
    observed_on =.x$observed_on %||% NA,
    year=.x$observed_on_details$year %||% NA,
    month=.x$observed_on_details$month %||% NA,
    week=.x$observed_on_details$week %||% NA,
    day=.x$observed_on_details$day %||% NA,
    hour=.x$observed_on_details$hour %||% NA,
    created_at =.x$created_at %||% NA,
    updated_at=.x$updated_at %||% NA,
    latitude =.x$geojson$coordinates[[2]] %||% NA_real_,
    longitude =.x$geojson$coordinates[[1]] %||% NA_real_,
    positional_accuracy=.x$positional_accuracy %||% NA,
    geoprivacy =.x$taxon_geoprivacy %||% NA,
    obscured =.x$obscured %||% NA,
    uri =.x$uri %||% NA,
    uri.photo.square=.x$taxon$default_photo$square_url %||% NA, # Añadido %||% NA por seguridad
    uri.photo.medium=.x$taxon$default_photo$medium_url %||% NA, # Añadido %||% NA por seguridad
    quality_grade =.x$quality_grade %||% NA,
    species_guess =.x$species_guess %||% NA,
    taxon_id =.x$taxon$id %||% NA,
    taxon_name =.x$taxon$name %||% NA,
    taxon_rank =.x$taxon$rank %||% NA,
    taxon_min_ancestry =.x$taxon$min_species_ancestry %||% NA,
    taxon_endemic=.x$taxon$endemic %||% NA,
    taxon_threatened=.x$taxon$threatened %||% NA,
    taxon_introduced=.x$taxon$introduced %||% NA,
    taxon_native =.x$taxon$native %||% NA,
    user_id =.x$user$id %||% NA_integer_,
    user_login =.x$user$login %||% NA
  ))
}

#' Internal function check de number of records and pagination and then download
#' the records.
#' @description Check the total of  month records and decide whether to download the
#' entire month or divide it by days.Then download the records.
#' @param params Parameters given for main function.
#' @param meta Download metadata.
#' @return A data frame with part of the year/month records.
#' @noRd
#'

get_minka_obs <- function(params, meta = FALSE, total_res = NULL) {
  API_MAX_RESULTS <- 10000
  base_url <- "https://api.minka-sdg.org"
  api_path <- "v1/observations"

  if (is.null(total_res)) {
    ping_params <- c(params, list(per_page = 1, page = 1))
    ping_response <- httr::GET(base_url, path = api_path, query = ping_params)
    if (httr::http_error(ping_response)) {
      message(paste("The PING query failed with the code:", httr::status_code(ping_response)))
      return(tibble::tibble())
    }
    ping_content <- httr::content(ping_response, as = "parsed", type = "application/json")
    total_res <- ping_content$total_results
  }

  if (is.null(total_res) || total_res == 0) return(tibble::tibble())

  max_to_fetch <- min(total_res, API_MAX_RESULTS)
  if (total_res > API_MAX_RESULTS) {
    message(paste("WARNING: The query found", total_res, "records. The API limits the download to the first few", API_MAX_RESULTS, "."))
  }

  all_results <- list()
  if (max_to_fetch > 0) {
    for (i in 1:ceiling(max_to_fetch / 200)) {
      page_params <- c(params, list(per_page = 200, page = i))
      data_response <- httr::GET(base_url, path = api_path, query = page_params)
      if (httr::http_error(data_response)) { message(paste("Error in page", i)); next }
      data_content <- httr::content(data_response, as = "parsed", type = "application/json")
      if (length(data_content$results) > 0) { all_results <- c(all_results, data_content$results) } else { break }
    }
  }

  # --- CAMBIO CLAVE: Llamamos a la nueva función para procesar los datos ---
  data_out <- process_minka_results(all_results)

  if (meta) {
    return(list(meta = list(found = total_res, returned = nrow(data_out)), data = data_out))
  } else {
    return(data_out)
  }
}

