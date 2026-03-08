#' @title Get the sf geometry of a Minka place.
#' Get the geometry of a Minka place from its id_place.
#' @description Gets the geometry in sf format of a Minka
#' place given its id_place.The CRS of the resulting geometry is
#' CRS = 4326 (WGS84).
#' @param id A single integer number id for a Minka place.
#' This id_place number is unique for each Minka place.
#' @return A tibble with the sf geometry place.
#' @examples \dontrun{
#' sant_feliu_id <-mnk_place_byname ("area marina sant feliu")
#' sf_sant_feliu <- mnk_place_sf(id= sf_sant_feliu$place_id)
#'
#' If the id_place is known, the function can be used directly.
#' sf_sant_feliu <- mnk_place_sf(id= 265)
#' }
#' @importFrom utils read.csv
#' @export


mnk_place_sf <- function(id) {

  if (!is.numeric(id) || length(id) != 1 || is.na(id)) {
    stop("You must provide a single non-empty numerical 'id'.")
  }
  base_url <- "https://api.minka-sdg.org"
  q_path <- paste0("/v1/places/", as.character(id), sep = "")
  response <- httr::GET(base_url, path = q_path, as = "text")

  if (httr::http_error(response)) {
    message("Minka API request failed. Status code: ", httr::status_code(response))
    return(invisible(NULL))
  }
  response_content <- httr::content(response, as = "text", encoding = "UTF-8")

  if (nchar(response_content) == 0) {
    message("API returned an empty response.")
    return(invisible(NULL))
  }


  parsed_json <- jsonlite::fromJSON(response_content, simplifyVector = FALSE)


  if (is.null(parsed_json$results) || length(parsed_json$results) == 0) {
    message("No places found for your query.") # Mantener el mensaje consistente
    return(invisible(NULL))}

  final_tibble <- purrr::map_dfr(parsed_json$results, function(x) {


    location_parts <- as.numeric(strsplit(x$location, ",")[[1]])

    tibble::tibble(
      geojson_string = as.character(jsonlite::toJSON(x$geometry_geojson, auto_unbox = TRUE))
    )
  })

  sf_object <- tibble::as_tibble(final_tibble) %>%
    dplyr::mutate(
      sf_geometry = purrr::map(geojson_string, function(raw_string) {

        cleaned_string <- stringr::str_replace_all(raw_string, "\\\\", "")
        cleaned_string <- sub('^"', '', cleaned_string)
        cleaned_string <- sub('"$', '', cleaned_string)

        sf_geom <- tryCatch({
        suppressWarnings(sf::st_geometry(sf::st_read(cleaned_string, quiet = TRUE))[[1]])
        }, error = function(e) {
          sf::st_point()
        })

        return(sf_geom)
      })
    ) %>%
    dplyr::select(-geojson_string) %>%
    sf::st_as_sf(sf_column_name = "sf_geometry", crs = 4326)

  return(sf_object)
}
