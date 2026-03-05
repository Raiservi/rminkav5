#' @title mnk_proj_info
#' Download complete info from a Minka project
#' @description This function can be used to get either information from a project
#' by complete project name or ID.
#' @details Before using this function, you need to know the \code{project_id} or the project slug name.
#' If the ID or the slug name is unknown, you can first use the function \code{\link{mnk_proj_byname}}
#' to find it based on the project's name.
#' A Minka slug is usually the project name as a single string with words separated by hyphens.
#' For instance, the project "biomarato-2022-girona" has a slug of "world-oceans-week-2022",
#' which you can find by searching projects on Minka and looking at the \href{https://minka-sdg.org/}{project's page's URL}.
#' @param project_id A single id for a Minka project.
#' @param grpid Name of the group as an Minka slug or group ID.
#' returns the raw output of parsed JSON for that project. Otherwise just some basic information is returned as a list.
#' @return A list containing project details similar to what you can find on a project's page.
#' @examples \dontrun{
#'  mnk_proj_byname("Biomarato Barcelona 2025")
#'  mnk_minka_proj_info("420")
#' }
#' @importFrom utils read.csv
#' @export
#'
mnk_proj_info <- function(project_id = NULL, grpid = NULL) {
  if (is.null(project_id) && is.null(grpid)) {
    stop("You must provide either 'project_id' or 'grpid'. Both cannot be NULL.")
  }

  # Nueva validación, más clara
  if (!is.null(project_id)) {
    if (!is.atomic(project_id) || length(project_id) != 1) {
      stop("'project_id' must be a single character string or number.")
    }
  }
  if (!is.null(grpid)) {
    if (!is.atomic(grpid) || length(grpid) != 1) {
      stop("'grpid' must be a single character string or number.")
    }
  }

  id_for_msg <- if (!is.null(project_id)) as.character(project_id) else as.character(grpid)

  base_url <- "https://api.minka-sdg.org"
  api_path <- "v1/projects"

  query_params <- list()
  if (!is.null(project_id)) {
    query_params$id <- as.character(project_id)
  }
  if (!is.null(grpid)) {
    query_params$q <- as.character(grpid)
  }

  # Usamos tryCatch para capturar errores de conexión a la red
  response <- tryCatch({
    httr::GET(url = base_url, path = api_path, query = query_params, as = "text")
  }, error = function(e) {
    message("Network error: Minka API is unavailable or unreachable. ", e$message)
    return(NULL)
  })
  if (is.null(response)) return(invisible(NULL))

  if (httr::http_error(response)) {
    status <- httr::status_code(response)
    message("Minka API request failed. Status code: ", status)
    return(invisible(NULL))
  }

  response_content <- httr::content(response, as = "text", encoding = "UTF-8")

  if (nchar(response_content) == 0 || response_content == "null") {
    message("API returned an empty or null response for project: ", id_for_msg, ".")
    return(invisible(NULL))
  }

  xx <- jsonlite::fromJSON(response_content, simplifyVector = FALSE)

  # --- CORRECCIÓN CRUCIAL AQUÍ ---
  # Comprobamos si 'results' existe y si tiene algún elemento
  if (is.null(xx$results) || length(xx$results) == 0) {
    message("No project details found for id_project = ", id_for_msg, ".")
    return(invisible(NULL))
  }

  # Si hay resultados, la API devuelve un array (lista) de proyectos. Tomamos el primero.
  project_data <- xx$results[[1]]

  output <- list()

  get_safe_value <- function(obj, field, default = NA) {
    val <- obj[[field]]
    if (is.null(val)) default else val
  }

  output[["title"]] <- get_safe_value(project_data, "title", NA_character_)
  output[["description"]] <- get_safe_value(project_data, "description", NA_character_)
  output[["slug"]] <- get_safe_value(project_data, "slug", NA_character_)
  output[["created_at"]] <- get_safe_value(project_data, "created_at", NA_character_)
  output[["id"]] <- get_safe_value(project_data, "id")

  output[["user_info"]] <- tibble::tibble(
    id = get_safe_value(project_data$user, "id", NA_integer_),
    login = get_safe_value(project_data$user, "login", NA_character_),
    name = get_safe_value(project_data$user, "name", NA_character_)
  )

  if (!is.null(project_data$admins) && length(project_data$admins) > 0) {
    output[["admins_info"]] <- dplyr::bind_rows(project_data$admins)
  } else {
    output[["admins_info"]] <- tibble::tibble(id = integer(), login = character(), name = character())
  }

  user_ids_val <- get_safe_value(project_data, "user_ids", list())
  user_ids_vec <- unlist(user_ids_val)

  output[["user_ids_list"]] <- if (is.null(user_ids_vec)) integer(0) else user_ids_vec
  output[["subscrib_users"]] <- length(output[["user_ids_list"]])


  return(output)
}

#
#
# library(tibble)
# s <- get_minka_proj_info(420)
# View(s)
