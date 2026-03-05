library(testthat)
library(rminkav5) # Tu paquete
library(httr) # Tu función usa httr::GET
library(jsonlite)
library(sf)
library(purrr)
library(dplyr)
# Ya no necesitamos httptest ni httptest2 aquí

# --- Test para argumentos inválidos (este ya funciona) ---
test_that("mnk_places_byname throws error for invalid query", {
  expect_error(mnk_places_byname(NULL), "You must provide a non-empty 'query' string.")
  expect_error(mnk_places_byname(""), "You must provide a non-empty 'query' string.")
  expect_error(mnk_places_byname(123), "You must provide a non-empty 'query' string.")
  expect_error(mnk_places_byname("   "), "You must provide a non-empty 'query' string.")
})

# --- Test para respuesta válida con mock de API redefiniendo httr::GET ---
test_that("mnk_places_byname returns sf object for valid query with mocked API", {
  mock_response_json <- '{
        "results": [
          {
            "id": "123",
            "slug": "area-marina-sant-feliu",
            "name": "Area marina Sant Feliu",
            "display_name": "Area marina Sant Feliu de Guíxols",
            "location": "41.78,3.00",
            "geometry_geojson": {"type": "Point", "coordinates": [3.00, 41.78]}
          }
        ]
      }'

  # Crear una función mock para httr::GET
  mock_httr_GET <- function(url = NULL, ..., path = NULL, as) {
    full_url <- if (!is.null(url) && !is.null(path)) paste0(url, path) else url

    if (grepl("q=Area%20marina%20Sant%20Feliu", full_url)) {
      # --- CAMBIO CRUCIAL AQUÍ: Creamos un objeto de respuesta simulado manualmente ---
      response_obj <- list(
        url = full_url,
        status_code = 200L,
        headers = list("Content-Type" = "application/json"),
        content = charToRaw(mock_response_json)
      )
      class(response_obj) <- c("response", "handle") # Asignamos las clases que httr espera

      return(response_obj)
    } else {
      stop("Mock no configurado para esta URL en el test de exito: ", full_url)
    }
  }

  with_mocked_bindings(
    GET = mock_httr_GET,
    .package = "httr",
    {
      result <- mnk_places_byname("Area marina Sant Feliu")

      expect_s3_class(result, "sf")
      expect_equal(nrow(result), 1)
      expect_true("place_id" %in% names(result))
      expect_equal(sf::st_crs(result)$epsg, 4326)
      expect_equal(result$place_id, "123")
      expect_equal(result$name, "Area marina Sant Feliu")
      expect_true(sf::st_is(result$sf_geometry, "POINT"))
    }
  )
})

# --- Test para no resultados de la API redefiniendo httr::GET ---
test_that("mnk_places_byname handles no results from API", {
  mock_httr_GET <- function(url = NULL, ..., path = NULL, as) {
    full_url <- if (!is.null(url) && !is.null(path)) paste0(url, path) else url
    if (grepl("q=No%20existente", full_url)) {
      response_obj <- list(
        url = full_url,
        status_code = 200L,
        headers = list("Content-Type" = "application/json"),
        content = charToRaw('{"results": []}')
      )
      class(response_obj) <- c("response", "handle") # Asignamos las clases que httr espera
      return(response_obj)
    } else {
      stop("Mock no configurado para esta URL en el test de no resultados: ", full_url)
    }
  }

  with_mocked_bindings(
    GET = mock_httr_GET,
    .package = "httr",
    {
      expect_message(result <- mnk_places_byname("No existente"), "No places found for your query.")
      expect_null(result)
    }
  )
})

# --- Test para error HTTP de la API redefiniendo httr::GET ---
test_that("mnk_places_byname handles API HTTP error", {
  mock_httr_GET <- function(url = NULL, ..., path = NULL, as) {
    full_url <- if (!is.null(url) && !is.null(path)) paste0(url, path) else url
    if (grepl("q=error_query", full_url)) {
      response_obj <- list(
        url = full_url,
        status_code = 500L, # Código de error HTTP
        headers = list("Content-Type" = "application/json"),
        content = charToRaw('{"error": "internal server error"}')
      )
      class(response_obj) <- c("response", "handle") # Asignamos las clases que httr espera
      return(response_obj)
    } else {
      stop("Mock no configurado para esta URL en el test de error: ", full_url)
    }
  }

  with_mocked_bindings(
    GET = mock_httr_GET,
    .package = "httr",
    {
      expect_message(result <- mnk_places_byname("error_query"), regexp = "Minka API request failed. Status code: 500")
      expect_null(result)
    }
  )
})
