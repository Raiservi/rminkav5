#' @title Get information about Minka place by name
#' Get information about Minka place by name
#' @description Get information about Minka place by part of the place name
#' @param query A string that is contained in the project name
#' @return A tibble with all the Minka places that contain the string with
#' some details of those projects
#' @examples \dontrun{
#' sant_feliu <- mnk_places_byname(query="Sant Feliu")
#' }
#' @importFrom utils read.csv
#' @export

mnk_places_byname <- function(query) {

  if (is.null(query) ||!is.character(query) || nchar(trimws(query)) == 0) {
    stop("You must provide a non-empty 'query' string.")
  }
  base_url <- "https://api.minka-sdg.org"
  query_parsed <- stringr::str_replace_all(query, " ", "%20")
  q_path <- paste0("/v1/places/autocomplete?q=", as.character(query_parsed), sep = "")
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

  # --- MODIFICACIÓN AQUÍ: Usar jsonlite directamente ---
  parsed_json <- jsonlite::fromJSON(response_content, simplifyVector = FALSE)

  if (is.null(parsed_json$results) || length(parsed_json$results) == 0) {
    message("No places found for your query.")
    return(invisible(NULL))
  }

  # Extraer la información relevante y estructurarla en un tibble
  final_tibble <- purrr::map_dfr(parsed_json$results, function(x) {
    # Dividir la ubicación y convertir a numérico
    location_parts <- as.numeric(strsplit(x$location, ",")[[1]])

    tibble::tibble(
      place_id = x$id,
      slug = x$slug,
      name = x$name,
      display_name = x$display_name,
      location_latitud = location_parts[1],
      location_longitud = location_parts[2]

    )
  })

  if (is.null(final_tibble) || nrow(final_tibble) == 0) {
    message("No places found for your query.")
    return(invisible(NULL))
  }
  # --- FIN DE LA MODIFICACIÓN ---

  return(final_tibble)
}

#  library(httr)
#  library(jsonlite)
#  library(dplyr)
#  library(leaflet)
#  library(tibble)
#  library(sf)
# #
#
#
#  e <-mnk_places_byname("sant feliu")
#
#
# #
# sf::st_polygon(geom)
#
# cat(geojson)
#
# js <- stringr::str_replace_all(geojson, "'\'", "")

# cat(js)
#
#   mapa_leaflet <- leaflet(geom) %>%
#      addTiles() %>%
#     addPolygons()
# #
#   print (mapa_leaflet)
