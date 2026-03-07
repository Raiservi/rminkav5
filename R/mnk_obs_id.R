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

  # Intenta parsear el JSON. Si falla, genera un error.
  # El id_char se pasa directamente al stop en el entorno de la función.
  parsed_json_full <- tryCatch({
    jsonlite::fromJSON(response_content, simplifyVector = TRUE, flatten = TRUE)
  }, error = function(e) {
    stop("Failed to parse JSON response for observation ID ", id_char, ": ", e$message)
  })

  # Comprobación inicial si parsed_json_full NO es una lista o data.frame
  # Esto maneja el caso de un string simple como respuesta JSON, donde fromJSON no devuelve un data.frame/list
  if (!is.list(parsed_json_full) &&!is.data.frame(parsed_json_full)) {
    message("No data found or unexpected JSON structure (atomic type) for observation ID ", id_char, ".")
    return(invisible(NULL))
  }

  # La API real devuelve { "total_results":..., "results": [{...}] }
  # Extraer el objeto de observación de 'results'
  if (!is.null(parsed_json_full$results) && is.data.frame(parsed_json_full$results) && nrow(parsed_json_full$results) > 0) {
    df_result <- parsed_json_full$results # Esto ya es un data frame
    if (nrow(df_result) > 1) {
      warning("Multiple observations found for ID ", id_char, ". Returning only the first one.")
      df_result <- df_result[1, ]
    }
  } else if (is.data.frame(parsed_json_full) && nrow(parsed_json_full) > 0) {
    # Si la API devuelve directamente el objeto de observación como un data frame sin "results"
    df_result <- parsed_json_full
  } else {
    # Si no se encuentra data o la estructura es inesperada después del fromJSON
    message("No data found or unexpected JSON structure for observation ID ", id_char, ".")
    return(invisible(NULL))
  }

  # Si en este punto df_result no es un data.frame o está vacío (muy improbable con las comprobaciones anteriores, pero para seguridad)
  if (!is.data.frame(df_result) || nrow(df_result) == 0) {
    message("No data found or unexpected JSON structure for observation ID ", id_char, ".")
    return(invisible(NULL))
  }

  return(tibble::as_tibble(df_result)) # Asegurarse de que el resultado sea un tibble
}
