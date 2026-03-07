library(testthat)
library(mockery) # Para with_mocked_bindings
library(httr)
library(jsonlite)
library(purrr)
library(dplyr)
library(tibble)
library(stringr)

# Asegúrate de que tu función mnk_places_byname esté cargada.
# source("R/mnk_places_byname.R") # ¡Descomenta si tu paquete no está instalado o cargado!

# --- DEFINICIONES DE MOCKS JSON GLOBALES PARA mnk_places_byname ---
# Estas definiciones están fuera de cualquier 'test_that' para asegurar su alcance.

mock_response_json_success_multiple <- '{
  "results": [
    {
      "id": "123",
      "slug": "area-marina-sant-feliu",
      "name": "Area marina Sant Feliu",
      "display_name": "Area marina Sant Feliu de Guíxols",
      "location": "41.7597741479,3.0226481505",
      "bbox_area": 0.005301438053870476
    },
    {
      "id": "456",
      "slug": "otra-area",
      "name": "Otra Area",
      "display_name": "Otra Area en alguna parte",
      "location": "40.00,2.00",
      "bbox_area": 0.01
    }
  ]
}'

mock_response_json_success_single <- '{
  "results": [
    {
      "id": "100",
      "slug": "single-place",
      "name": "Single Place",
      "display_name": "Just one place here",
      "location": "30.00,1.00",
      "bbox_area": 0.0075
    }
  ]
}'

# JSON para malformado que producirá un error real de jsonlite::fromJSON
mock_response_json_malformed_for_parsing <- '{"error": "bad json", "results": [' # Corchete sin cerrar

mock_response_json_no_results_key <- '{"some_other_key": "some_value"}' # JSON válido pero sin la clave 'results'
mock_response_json_empty_results <- '{"results": []}' # JSON con 'results' vacío

# --- Test para argumentos inválidos ---
test_that("mnk_places_byname throws error for invalid query", {
  expect_error(mnk_places_byname(NULL), "You must provide a non-empty 'query' string.")
  expect_error(mnk_places_byname(""), "You must provide a non-empty 'query' string.")
  # El error debe coincidir con el que lanza mnk_places_byname para una entrada numérica
  expect_error(mnk_places_byname(123), "You must provide a non-empty 'query' string.")
  expect_error(mnk_places_byname(" "), "You must provide a non-empty 'query' string.")
})

# --- Test de éxito: Query válida y respuesta correcta ---
test_that("mnk_places_byname returns a tibble for a valid query", {
  mock_httr_GET <- function(url = NULL,..., path = NULL, as) {
    query_param_encoded <- stringr::str_extract(path, "(?<=q=).*")
    query_param <- URLdecode(query_param_encoded)

    response_content <- ""
    if (grepl("Area marina Sant Feliu", query_param, ignore.case = TRUE)) {
      response_content <- mock_response_json_success_multiple
    } else if (grepl("Single Place", query_param, ignore.case = TRUE)) {
      response_content <- mock_response_json_success_single
    } else {
      response_content <- mock_response_json_empty_results # Fallback para queries no mockeadas
    }

    response_obj <- structure(list(
      url = paste0("https://api.minka-sdg.org", path),
      status_code = 200L,
      headers = list("Content-Type" = "application/json"),
      content = charToRaw(response_content)
    ), class = c("response", "handle"))
    return(response_obj)
  }

  with_mocked_bindings(
    GET = mock_httr_GET,
    .package = "httr",
    {
      result_multiple <- mnk_places_byname("Area marina Sant Feliu")
      expect_s3_class(result_multiple, "tbl_df")
      expect_equal(nrow(result_multiple), 2)
      # Añadido "area" a la comprobación de nombres de columnas
      expect_true(all(c("place_id", "slug", "name", "area", "display_name", "location_latitud", "location_longitud") %in% names(result_multiple)))
      expect_equal(result_multiple$place_id[1], "123")
      expect_equal(result_multiple$name[1], "Area marina Sant Feliu")
      # Ajustado al valor exacto del JSON
      expect_equal(result_multiple$area[1], 0.005301438053870476)
      # Ajustado al valor exacto del JSON
      expect_equal(result_multiple$location_latitud[1], 41.7597741479)
      expect_equal(result_multiple$location_longitud[1], 3.0226481505)

      result_single <- mnk_places_byname("Single Place")
      expect_s3_class(result_single, "tbl_df")
      expect_equal(nrow(result_single), 1)
      expect_equal(result_single$place_id[1], "100")
      expect_equal(result_single$location_latitud[1], 30.00)
      # Ajustado al valor exacto del JSON
      expect_equal(result_single$area[1], 0.0075)
    }
  )
})

# --- Test para no resultados de la API ---
test_that("mnk_places_byname handles no results from API", {
  mock_httr_GET <- function(url = NULL,..., path = NULL, as) {
    query_param_encoded <- stringr::str_extract(path, "(?<=q=).*")
    query_param <- URLdecode(query_param_encoded)

    response_content <- ""
    if (grepl("No existente", query_param, ignore.case = TRUE)) {
      response_content <- mock_response_json_empty_results
    } else if (grepl("Empty response", query_param, ignore.case = TRUE)) {
      response_content <- ""
    } else {
      stop("Mock no configurado para esta URL en el test de no resultados: ", path)
    }

    response_obj <- structure(list(
      url = paste0("https://api.minka-sdg.org", path),
      status_code = 200L,
      headers = list("Content-Type" = "application/json"),
      content = charToRaw(response_content)
    ), class = c("response", "handle"))
    return(response_obj)
  }

  with_mocked_bindings(
    GET = mock_httr_GET,
    .package = "httr",
    {
      expect_message(result <- mnk_places_byname("No existente"), "No places found for your query.")
      expect_null(result)

      expect_message(result_empty_content <- mnk_places_byname("Empty response"), "API returned an empty response.")
      expect_null(result_empty_content)
    }
  )
})

# --- Test para error HTTP de la API ---
test_that("mnk_places_byname handles API HTTP error", {
  mock_httr_GET <- function(url = NULL,..., path = NULL, as) {
    query_param_encoded <- stringr::str_extract(path, "(?<=q=).*")
    query_param <- URLdecode(query_param_encoded)

    status_code <- 200L
    response_content <- mock_response_json_empty_results # Default

    if (grepl("error_query_500", query_param, ignore.case = TRUE)) {
      status_code <- 500L
      response_content <- '{"error": "internal server error"}'
    } else if (grepl("error_query_404", query_param, ignore.case = TRUE)) {
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
      expect_message(result_500 <- mnk_places_byname("error_query_500"), regexp = "Minka API request failed. Status code: 500")
      expect_null(result_500)

      expect_message(result_404 <- mnk_places_byname("error_query_404"), regexp = "Minka API request failed. Status code: 404")
      expect_null(result_404)
    }
  )
})

# --- Test para respuesta JSON malformada o sin 'results' ---
test_that("mnk_places_byname handles malformed JSON or missing 'results' key", {
  mock_httr_GET <- function(url = NULL,..., path = NULL, as) {
    query_param_encoded <- stringr::str_extract(path, "(?<=q=).*")
    query_param <- URLdecode(query_param_encoded)

    response_content <- ""
    status_code <- 200L

    if (grepl("Malformed JSON", query_param, ignore.case = TRUE)) {
      response_content <- mock_response_json_malformed_for_parsing
    } else if (grepl("No results key", query_param, ignore.case = TRUE)) {
      response_content <- mock_response_json_no_results_key
    } else if (grepl("Malformed location", query_param, ignore.case = TRUE)) {
      response_content <- '{
        "results": [
          {
            "id": "999",
            "slug": "malformed-location",
            "name": "Malformed Location Place",
            "display_name": "Place with bad location",
            "location": "invalid_coords",
            "bbox_area": 0.0
          }
        ]
      }'
    } else if (grepl("Location no numeric", query_param, ignore.case = TRUE)) {
      response_content <- '{
        "results": [
          {
            "id": "888",
            "slug": "location-non-numeric",
            "name": "Location Non-Numeric",
            "display_name": "Place with non-numeric location",
            "location": "41.78,abc",
            "bbox_area": 0.0
          }
        ]
      }'
    } else {
      stop("Mock no configurado para esta URL en el test de malformado: ", path)
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
      expect_error(mnk_places_byname("Malformed JSON"), regexp = "lexical error|syntax error|unallowed token|premature EOF", class = "error")

      expect_message(result <- mnk_places_byname("No results key"), "No places found for your query.")
      expect_null(result)

      # Ahora esperamos el mensaje de error que lanzas explícitamente desde la función
      expect_error(suppressWarnings(mnk_places_byname("Malformed location")), regexp = "Invalid 'location' format received from API", class = "error")

      # Ahora esperamos el mensaje de error que lanzas explícitamente desde la función
      expect_error(suppressWarnings(mnk_places_byname("Location no numeric")), regexp = "Invalid 'location' format received from API", class = "error")
    }
  )
})
