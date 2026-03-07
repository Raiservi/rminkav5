library(testthat)
library(mockery) # Para with_mocked_bindings
library(httr)
library(jsonlite)
library(sf)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)

# Asegúrate de que tu función mnk_place_sf esté cargada.
# source("R/mnk_place_sf.R") # ¡Descomenta si tu paquete no está instalado o cargado!

# --- DEFINICIONES DE MOCKS JSON GLOBALES (TODAS AL PRINCIPIO DEL ARCHIVO) ---
# ESTOS SON SOLO STRINGS, PUEDEN ESTAR AQUÍ.

mock_response_json_success_point <- '{
  "total_results": 1,
  "page": 1,
  "per_page": 1,
  "results": [
    {
      "ancestor_place_ids": null,
      "bounding_box_geojson": { "type": "Polygon", "coordinates": [[[2.9828202716,41.7263143087],[2.9828202716,41.7923486579],[3.0631033079,41.7923486579],[3.0631033079,41.7263143087],[2.9828202716,41.7263143087]]] },
      "bbox_area": 0.0053,
      "admin_level": null,
      "place_type": null,
      "name": "Area marina Sant Feliu",
      "location": "41.7597741479,3.0226481505",
      "id": 265,
      "display_name": "Area marina Sant Feliu",
      "uuid": "ee1ecbbf-2ae7-4551-b053-35eb5bb9f31e",
      "slug": "area-marina-sant-feliu",
      "geometry_geojson": {
        "coordinates": [2.923, 41.777],
        "type": "Point"
      }
    }
  ]
}'

mock_response_json_success_polygon <- '{
  "total_results": 1,
  "page": 1,
  "per_page": 1,
  "results": [
    {
      "ancestor_place_ids": null,
      "bounding_box_geojson": { "type": "Polygon", "coordinates": [[[0,0], [0,1], [1,1], [1,0], [0,0]]] },
      "bbox_area": 0.0053,
      "admin_level": null,
      "place_type": null,
      "name": "Parque Natural XYZ",
      "location": "10.0,10.0",
      "id": 456,
      "display_name": "Parque Natural XYZ",
      "uuid": "some-uuid",
      "slug": "parque-natural-xyz",
      "geometry_geojson": {
        "type": "Polygon",
        "coordinates": [[[0,0], [0,1], [1,1], [1,0], [0,0]]]
      }
    }
  ]
}'

mock_response_json_empty_results <- '{
  "total_results": 0,
  "page": 1,
  "per_page": 1,
  "results": []
}'

mock_response_json_no_results_key <- '{"some_other_key": "some_value", "total_results": 0}'

mock_response_json_results_no_geom <- '{
  "total_results": 1,
  "page": 1,
  "per_page": 1,
  "results": [
    {
      "id": 888,
      "slug": "place-no-geom",
      "name": "Place without geometry in results",
      "location": "10.0,10.0"
      // geometry_geojson falta intencionadamente
    }
  ]
}'

mock_response_malformed_geojson_coords <- '{
  "total_results": 1,
  "page": 1,
  "per_page": 1,
  "results": [
    {
      "id": 666,
      "slug": "malformed-geom-coords",
      "name": "Malformed Geometry Place Coords",
      "location": "1.0,1.0",
      "geometry_geojson": {
        "type": "Point",
        "coordinates": ["invalid", "data"]
      }
    }
  ]
}'

mock_response_completely_malformed_json <- '{"total_results": 1, "results": [ {"id": 777, "geometry_geojson": {"type": "Point", "coordinates": [0,0]}'

# --- Test para IDs no válidos en la entrada de la función (Correcto) ---
test_that("mnk_place_sf handles invalid input 'id'", {
  expect_error(mnk_place_sf(NULL), "You must provide a single non-empty numerical 'id'.")
  expect_error(mnk_place_sf("abc"), "You must provide a single non-empty numerical 'id'.")
  expect_error(mnk_place_sf(c(1, 2)), "You must provide a single non-empty numerical 'id'.")
  expect_error(mnk_place_sf(NA_real_), "You must provide a single non-empty numerical 'id'.")
})

# --- Test de éxito: ID válido y respuesta correcta ---
test_that("mnk_place_sf returns an sf object for a valid ID", {
  # Definición de la función mock_httr_GET para ESTE test_that
  mock_httr_GET <- function(url = NULL,..., path = NULL, as) {
    id_from_path <- as.numeric(stringr::str_extract(path, "[0-9]+$"))

    response_content <- ""
    status_code <- 200L

    if (id_from_path == 265) {
      response_content <- mock_response_json_success_point
    } else if (id_from_path == 456) {
      response_content <- mock_response_json_success_polygon
    } else {
      status_code <- 404L
      response_content <- '{"error": "Not Found"}'
    }

    response_obj <- structure(list(
      url = paste0("https://api.minka-sdg.org", path),
      status_code = status_code,
      headers = list("Content-Type" = "application/json"),
      content = charToRaw(response_content)
    ), class = c("response", "handle"))
    return(response_obj)
  }

  with_mocked_bindings(
    GET = mock_httr_GET,
    .package = "httr",
    {
      # Test con un Point (ID 265, de tu ejemplo)
      result_point <- mnk_place_sf(265)

      expect_s3_class(result_point, "sf")
      expect_equal(nrow(result_point), 1)
      # CORRECCIÓN: Tu función ELIMINA 'geojson_string' al final.
      # expect_true("geojson_string" %in% names(result_point)) # Esta línea se comenta o elimina.

      expect_equal(sf::st_crs(result_point)$epsg, 4326)
      expect_true(sf::st_is(result_point$sf_geometry, "POINT"))
      expect_equal(as.numeric(sf::st_coordinates(result_point$sf_geometry)), c(2.923, 41.777))

      # Test con un Polygon
      result_polygon <- mnk_place_sf(456)
      expect_s3_class(result_polygon, "sf")
      expect_equal(nrow(result_polygon), 1)
      expect_true(sf::st_is(result_polygon$sf_geometry, "POLYGON"))
    }
  )
})

# --- Test para no resultados o respuesta vacía de la API ---
test_that("mnk_place_sf handles no results or empty response from API", {
  # Definición de la función mock_httr_GET para ESTE test_that
  mock_httr_GET_empty <- function(url = NULL,..., path = NULL, as) {
    id_from_path <- as.numeric(stringr::str_extract(path, "[0-9]+$"))

    response_content <- ""
    status_code <- 200L
    if (id_from_path == 999) { # ID que devuelve string vacío (ej. 200 OK pero sin contenido)
      response_content <- ""
    } else if (id_from_path == 888) { # ID que devuelve JSON con 'results': []
      response_content <- mock_response_json_empty_results
    } else if (id_from_path == 777) { # ID que devuelve JSON sin la clave 'results'
      response_content <- mock_response_json_no_results_key
    } else if (id_from_path == 666) { # ID que devuelve JSON con 'results' pero un elemento sin geometry_geojson
      response_content <- mock_response_json_results_no_geom
    } else {
      stop("Mock no configurado para esta URL en el test de no resultados: ", path)
    }

    response_obj <- structure(list(
      url = paste0("https://api.minka-sdg.org", path),
      status_code = status_code,
      headers = list("Content-Type" = "application/json"),
      content = charToRaw(response_content)
    ), class = c("response", "handle"))
    return(response_obj)
  }

  with_mocked_bindings(
    GET = mock_httr_GET_empty,
    .package = "httr",
    {
      # Test con respuesta HTTP 200 pero contenido vacío
      expect_message(result_empty_content <- mnk_place_sf(999), "API returned an empty response.")
      expect_null(result_empty_content)

      # Test con JSON '{"results": []}'
      expect_message(result_empty_results <- mnk_place_sf(888), "No places found for your query.")
      expect_null(result_empty_results)

      # Test con JSON sin la clave 'results'
      expect_message(result_no_results_key <- mnk_place_sf(777), "No places found for your query.")
      expect_null(result_no_results_key)

      # Test con JSON con 'results' pero un elemento sin 'geometry_geojson'
      result_no_geom_in_results <- mnk_place_sf(666)
      expect_s3_class(result_no_geom_in_results, "sf")
      expect_equal(nrow(result_no_geom_in_results), 1)
      expect_true(sf::st_is(result_no_geom_in_results$sf_geometry, "POINT"))
      expect_true(sf::st_is_empty(result_no_geom_in_results$sf_geometry))
    }
  )
})

# --- Test para error HTTP de la API (Correcto) ---
test_that("mnk_place_sf handles API HTTP error", {
  # Definición de la función mock_httr_GET para ESTE test_that
  mock_httr_GET_error <- function(url = NULL,..., path = NULL, as) {
    id_from_path <- as.numeric(stringr::str_extract(path, "[0-9]+$"))

    response_content <- ""
    status_code <- 200L # Default

    if (id_from_path == 500) { # Simula un error 500
      status_code <- 500L
      response_content <- '{"error": "internal server error"}'
    } else if (id_from_path == 404) { # Simula un error 404
      status_code <- 404L
      response_content <- '{"error": "Not Found"}'
    } else {
      stop("Mock no configurado para esta URL en el test de error HTTP: ", path)
    }

    response_obj <- structure(list(
      url = paste0("https://api.minka-sdg.org", path),
      status_code = status_code,
      headers = list("Content-Type" = "application/json"),
      content = charToRaw(response_content)
    ), class = c("response", "handle"))
    return(response_obj)
  }

  with_mocked_bindings(
    GET = mock_httr_GET_error,
    .package = "httr",
    {
      # Test con error 500
      expect_message(result_500 <- mnk_place_sf(500), regexp = "Minka API request failed. Status code: 500")
      expect_null(result_500)

      # Test con error 404
      expect_message(result_404 <- mnk_place_sf(404), regexp = "Minka API request failed. Status code: 404")
      expect_null(result_404)
    }
  )
})

# --- Test para GeoJSON malformado ---
test_that("mnk_place_sf handles malformed GeoJSON or JSON", {
  # Definición de la función mock_httr_GET para ESTE test_that
  mock_httr_GET_malformed <- function(url = NULL,..., path = NULL, as) {
    id_from_path <- as.numeric(stringr::str_extract(path, "[0-9]+$"))
    response_content <- ""
    status_code <- 200L

    if (id_from_path == 666) {
      response_content <- mock_response_malformed_geojson_coords
    } else if (id_from_path == 777) {
      response_content <- mock_response_completely_malformed_json
    } else {
      stop("Mock no configurado para esta URL en el test de GeoJSON malformado: ", path)
    }

    response_obj <- structure(list(
      url = paste0("https://api.minka-sdg.org", path),
      status_code = status_code,
      headers = list("Content-Type" = "application/json"),
      content = charToRaw(response_content)
    ), class = c("response", "handle"))
    return(response_obj)
  }

  with_mocked_bindings(
    GET = mock_httr_GET_malformed,
    .package = "httr",
    {
      # Test con GeoJSON con coordenadas mal formadas (sf::st_read fallará)
      result_malformed_geom <- suppressWarnings(mnk_place_sf(666)) # Añadido suppressWarnings
      expect_s3_class(result_malformed_geom, "sf")
      expect_equal(nrow(result_malformed_geom), 1)
      expect_true(sf::st_is(result_malformed_geom$sf_geometry, "POINT"))
      expect_true(sf::st_is_empty(result_malformed_geom$sf_geometry))

      # Test con JSON completamente malformado (jsonlite::fromJSON fallará)
      expect_error(mnk_place_sf(777), regexp = "lexical error|syntax error|unallowed token|premature EOF", class = "error")
    }
  )
})
