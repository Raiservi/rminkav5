library(testthat)
library(rminkav5) # Tu paquete
library(httr)
library(jsonlite)
library(stringr) # Necesario para str_replace_all en la función
library(tibble)  # Útil si fromJSON devuelve tibbles, aunque tu función usa data.frame

# --- Test para argumentos inválidos ---
test_that("mnk_proj_byname throws error for invalid query", {
  # Mensajes actualizados para reflejar la validación de la función
  expect_error(mnk_proj_byname(NULL), "You must provide a single, non-empty, non-NA character 'query' for the project search.")
  expect_error(mnk_proj_byname(NA_character_), "You must provide a single, non-empty, non-NA character 'query' for the project search.")
  expect_error(mnk_proj_byname(""), "You must provide a single, non-empty, non-NA character 'query' for the project search.")
  expect_error(mnk_proj_byname("   "), "You must provide a single, non-empty, non-NA character 'query' for the project search.")
  # CAMBIO AQUÍ: Ahora esperamos el mensaje de error específico si hay más de un query
  expect_error(mnk_proj_byname(c("query1", "query2")), "You must provide a single query string. Only one query is accepted.")
})

# --- Test para API devuelve error HTTP ---
test_that("mnk_proj_byname handles API HTTP errors", {
  mock_httr_GET <- function(url = NULL, ..., path = NULL, as) {
    full_url <- if (!is.null(url) && !is.null(path)) paste0(url, path) else url
    if (grepl("q=error_query", full_url)) {
      response_obj <- list(
        url = full_url,
        status_code = 500L, # Código de error HTTP
        headers = list(`Content-Type` = "application/json"),
        content = charToRaw('{"error": "Internal server error"}')
      )
      class(response_obj) <- c("response", "handle")
      return(response_obj)
    } else {
      stop("Mock no configurado para esta URL en el test de error HTTP: ", full_url)
    }
  }

  with_mocked_bindings(
    GET = mock_httr_GET,
    .package = "httr",
    {
      expect_message(result <- mnk_proj_byname("error_query"), regexp = "Minka API request failed for query 'error_query'. Status code: 500")
      expect_null(result)
    }
  )
})

# --- Test para API devuelve respuesta vacía o 'null' ---
test_that("mnk_proj_byname handles empty or null API response", {
  mock_httr_GET <- function(url = NULL, ..., path = NULL, as) {
    full_url <- if (!is.null(url) && !is.null(path)) paste0(url, path) else url
    if (grepl("q=empty_response", full_url)) {
      response_obj <- list(
        url = full_url,
        status_code = 200L,
        headers = list(`Content-Type` = "application/json"),
        content = charToRaw('') # Respuesta vacía
      )
      class(response_obj) <- c("response", "handle")
      return(response_obj)
    } else if (grepl("q=null_response", full_url)) {
      response_obj <- list(
        url = full_url,
        status_code = 200L,
        headers = list(`Content-Type` = "application/json"),
        content = charToRaw('null') # Respuesta JSON 'null'
      )
      class(response_obj) <- c("response", "handle")
      return(response_obj)
    } else {
      stop("Mock no configurado para esta URL en el test de respuesta vacía/nula: ", full_url)
    }
  }

  with_mocked_bindings(
    GET = mock_httr_GET,
    .package = "httr",
    {
      expect_message(result <- mnk_proj_byname("empty_response"), regexp = "API returned an empty or null response for query 'empty_response'.")
      expect_null(result)

      expect_message(result <- mnk_proj_byname("null_response"), regexp = "API returned an empty or null response for query 'null_response'.")
      expect_null(result)
    }
  )
})

# --- Test para API devuelve JSON sin resultados (projects: []) ---
test_that("mnk_proj_byname handles JSON with no projects found", {
  mock_response_json <- '{"results": []}'

  mock_httr_GET <- function(url = NULL, ..., path = NULL, as) {
    full_url <- if (!is.null(url) && !is.null(path)) paste0(url, path) else full_url <- url
    if (grepl("q=no_projects", full_url)) {
      response_obj <- list(
        url = full_url,
        status_code = 200L,
        headers = list(`Content-Type` = "application/json"),
        content = charToRaw(mock_response_json)
      )
      class(response_obj) <- c("response", "handle")
      return(response_obj)
    } else {
      stop("Mock no configurado para esta URL en el test de no proyectos: ", full_url)
    }
  }

  with_mocked_bindings(
    GET = mock_httr_GET,
    .package = "httr",
    {
      expect_message(result <- mnk_proj_byname("no_projects"), regexp = "No projects found for query 'no_projects'.")
      expect_s3_class(result, "data.frame") # Devuelve un data.frame vacío
      expect_equal(nrow(result), 0)
    }
  )
})

# --- Test para API devuelve datos válidos ---
test_that("mnk_proj_byname returns a data frame for valid query", {
  mock_response_json <- '{
    "results": [
      {
        "id": "proj1",
        "name": "Proyecto A",
        "description": "Descripción del Proyecto A",
        "status": "active"
      },
      {
        "id": "proj2",
        "name": "Proyecto B",
        "description": "Descripción del Proyecto B",
        "status": "pending"
      }
    ]
  }'

  mock_httr_GET <- function(url = NULL, ..., path = NULL, as) {
    full_url <- if (!is.null(url) && !is.null(path)) paste0(url, path) else url
    if (grepl("q=Proyecto%20A", full_url)) {
      response_obj <- list(
        url = full_url,
        status_code = 200L,
        headers = list(`Content-Type` = "application/json"),
        content = charToRaw(mock_response_json)
      )
      class(response_obj) <- c("response", "handle")
      return(response_obj)
    } else {
      stop("Mock no configurado para esta URL en el test de exito: ", full_url)
    }
  }

  with_mocked_bindings(
    GET = mock_httr_GET,
    .package = "httr",
    {
      result <- mnk_proj_byname("Proyecto A")

      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 2)
      expect_true(all(c("id", "name", "description", "status") %in% names(result))) # Las columnas de 'results'
      expect_equal(result$id, c("proj1", "proj2"))
      expect_equal(result$name, c("Proyecto A", "Proyecto B"))
    }
  )
})

