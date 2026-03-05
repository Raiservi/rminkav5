#' @title mnk_user_byname
#' Get information on a specific Minka user selected by  a string contained in the user login name
#' @description Get information on a specific Minka user selected by  a string contained in the user login name
#' @details This fucntion is mainly used for obtain the user id and use this id for other functions.
#' @param query A string that is contained in the the user login name
#' @return A data frame with all some details of the differents users  that contains the string
#' @examples \dontrun{
#' m_obs <- mnk_user_byname(query="xavier")
#' }
#' @importFrom utils read.csv
#' @export

mnk_user_byname <- function(query) {

  base_url <- "https://api.minka-sdg.org"
  api_path <- "v1/users/autocomplete"

  response <- httr::GET(base_url, path = api_path, query = list(q = query))

  if (httr::http_error(response)) {
    message("Minka API request failed. Status: ", httr::status_code(response))
    return(tibble::tibble())
  }

  content <- httr::content(response, as = "parsed")

  if (is.list(content) && !is.null(content$results)) {


    purrr::map_dfr(content$results, ~tibble::tibble(
      id = .x$id %||% NA_integer_,
      login = .x$login %||% NA_character_,
      name = .x$name %||% NA_character_,
      observations_count = .x$observations_count %||% NA_integer_,
      created_at = .x$created_at %||% NA_character_
    ))

  } else {
    message("API response was not in the expected format (missing a 'results' list).")
    return(tibble::tibble())
  }

}
