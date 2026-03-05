#'
#' @title mnk_obs_id
#' Get information on a specific observation by observation id
#' @description
#' Get information on a specific observation by observation id
#' @param id A single id for a Minka observation record
#' @param meta Downloand metadata
#' @return A dataframe with all details on a given record
#' @examples \dontrun{
#' m_obs <- mnk_obs(query="Boops boops")
#' get_minka_obs_id(m_obs$id[1])
#' }
#' @importFrom utils read.csv
#' @export
#'
#'
#'
mnk_obs_id <- function(id, meta = FALSE) {

  # --- VALIDACIÓN ROBUSTA DEL ID ---
  if (is.null(id) || length(id) == 0 || is.na(id[1]) ||!is.atomic(id) || length(id) > 1) {
    stop("You must provide a single, non-empty, non-NA ID for the observation.")
  }
  id_char <- as.character(id[1])
  if (nchar(trimws(id_char)) == 0) {
    stop("You must provide a single, non-empty, non-NA ID for the observation.")
  }

  base_url <- "https://api.minka-sdg.org"
  api_path <- "/v1/observations/"
  q_path <- paste0(api_path, id_char)

  response <- httr::GET(base_url, path = q_path, as = "text")

  if (httr::http_error(response)) {
    status <- httr::status_code(response)
    message("Minka API request failed for observation ID ", id_char, ". Status code: ", status)
    return(invisible(NULL))
  }

  response_content <- httr::content(response, as = "text", encoding = "UTF-8")

  if (nchar(response_content) == 0 || response_content == "null") {
    message("API returned an empty or null response for observation ID ", id_char, ".")
    return(invisible(NULL))
  }

  parsed_json <- jsonlite::fromJSON(response_content)

  if (is.list(parsed_json) && length(parsed_json) == 0) {
    message("No data found for observation ID ", id_char, ".")
    return(invisible(NULL))
  }

  if (is.list(parsed_json)) {
    # --- LÓGICA CORREGIDA SIN purrr::map ---
    # Convertir todos los NULLs en NA para que tibble::as_tibble no falle
    # Esto lo hacemos iterando con lapply de base R
    parsed_json_cleaned <- lapply(parsed_json, function(x) {
      if (is.null(x)) NA else x
    })

    # Asegurarse de que todos los elementos sean atómicos para as_tibble
    # y de longitud 1 para formar una fila
    parsed_json_final <- lapply(parsed_json_cleaned, function(x) {
      if (length(x) == 0) NA # Si después de NULLs quedan elementos de longitud 0
      else if (length(x) > 1) list(x) # Si es una lista o vector de >1, lo ponemos en una lista para que tibble lo maneje como una columna de lista
      else x
    })

    df_result <- tibble::as_tibble(parsed_json_final)
    return(df_result)
  } else {
    warning("Unexpected JSON structure received for observation ID ", id_char, ". Returning NULL.")
    return(invisible(NULL))
  }
}
