#' @title mnk_user_proj
#' @description Retrieves the projects to which a user is subscribed from the Minka-SDG API
#'     and returns them as a tibble.
#' @param id_user (numeric): The user ID
#' @return A tibble where each row is a project and the columns
#' contain relevant information about the project.
#' Returns an empty tibble if there are no projects or if an error occurs.
#' @export
#'
#' @examples \dontrun{
#' # projects_for_user_6 <- mnk_user_proj(6)
#' # print(projects_for_user_6)
#'}
#'

mnk_user_proj <- function(id_user) {
  # Validación básica del input (manteniendo la robustez de la última versión)
  if (is.null(id_user) || length(id_user)!= 1 ||!is.numeric(id_user) || is.na(id_user)) {
    stop("You must provide a single, non-NA numeric 'id_user'.")
  }

  api_url <- paste0("https://api.minka-sdg.org/v1/users/", id_user, "/projects")

  response <- httr::GET(api_url)


  if (httr::http_error(response)) {
    message("Minka API request failed. Status: ", httr::status_code(response))
    return(tibble::tibble())
  }


  content <- httr::content(response, as = "parsed")

  if (is.list(content) && !is.null(content$results)) {


    purrr::map_dfr(content$results, ~tibble::tibble(
      id = .x$id %||% NA_integer_,
      title = .x$title %||% NA_character_,
      description = .x$description %||% NA_character_,
      slug = .x$slug %||% NA_character_,
      icon = .x$icon %||% NA_character_,
      place_id = .x$place_id %||% NA_integer_,
      created_at = .x$created_at %||% NA_character_
    ))

  } else {
    message("API response was not in the expected format (missing a 'results' list).")
    return(tibble::tibble())
  }

}


