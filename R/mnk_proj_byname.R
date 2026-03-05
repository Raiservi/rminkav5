#' @title mnk_proj_byname
#' Get information on a specific project selected by  a string contained in the project name
#' @description Get information on a specific project selected by  a string contained in the project name
#' @param query A string that is contained in the project name
#' @return A data frame with all the projects that contain the string with some details of those projects
#' @examples \dontrun{
#' mnk_obs <- mnk_proj_byname(query="Biomarato 2025")
#' mnk_obs_id(m_obs$id[1])
#' }
#' @importFrom utils read.csv
#' @export
#'
mnk_proj_byname <- function(query) {

  if (is.null(query) || length(query) == 0 || is.na(query[1]) ||!is.character(query) || nchar(trimws(query[1])) == 0) {
    stop("You must provide a single, non-empty, non-NA character 'query' for the project search.")
  }

  if (length(query) > 1) {
    # CAMBIO: stop() en lugar de warning()
    stop("You must provide a single query string. Only one query is accepted.")
  }

  base_url <- "https://api.minka-sdg.org"
  query_parsed <- stringr::str_replace_all(query, " ", "%20")
  q_path <- paste0("/v1/projects/autocomplete?q=", query_parsed)
  response <- httr::GET(base_url, path = q_path, as = "text")

  if (httr::http_error(response)) {
    status <- httr::status_code(response)
    message("Minka API request failed for query '", query, "'. Status code: ", status)
    return(invisible(NULL))
  }
  response_content <- httr::content(response, as = "text", encoding = "UTF-8")
  if (nchar(response_content) == 0 || response_content == "null") {
    message("API returned an empty or null response for query '", query, "'.")
    return(invisible(NULL))
  }
  result <- jsonlite::fromJSON(response_content)

  if (is.null(result$results) || length(result$results) == 0) {
    message("No projects found for query '", query, "'.")
    return(data.frame())
  }
  df <- as.data.frame(result$results)
  return(df)
}

