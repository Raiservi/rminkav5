library(testthat)
library(mockery)
library(httr)
library(jsonlite)
library(dplyr)
library(tibble)

# Asegúrate de que tu función esté cargada.
# source("R/mnk_user_byname.R")

test_that("mnk_user_byname procesa correctamente una respuesta de API simulada", {
  # Mock del contenido que devolverá httr::content
  mock_content_success <- list(
    total_results = 2, page = 1, per_page = 2,
    results = list(
      list(id = 123, login = "testuser1", name = "Test User One", observations_count = 50, created_at = "2023-01-01T12:00:00Z"),
      list(id = 456, login = "testuser2", name = NULL, observations_count = 100, created_at = "2023-01-02T13:00:00Z")
    )
  )

  # Mock de httr::GET que solo devuelve una respuesta exitosa vacía
  mock_GET_generic_success <- function(url, path, query) {
    return(structure(list(status_code = 200L), class = c("response", "handle")))
  }

  # Suplantamos GET (para que no falle) y content (para devolver los datos)
  local_mocked_bindings(
    GET = mock_GET_generic_success,
    content = function(x, as) mock_content_success,
    .package = "httr")
  {
    result <- mnk_user_byname("test")
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 2)
    expect_equal(result$id, c(123, 456))
    expect_true(is.na(result$name[2]))
  }
})

test_that("mnk_user_byname maneja un error de API correctamente", {
  # Mock de httr::GET que devuelve un error HTTP
  mock_GET_error <- function(url, path, query) {
    return(structure(list(status_code = 500L), class = c("response", "handle")))
  }

  local_mocked_bindings(GET = mock_GET_error,.package = "httr")
  {
    expect_message(result <- mnk_user_byname("error"), "Minka API request failed. Status: 500")
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0)
  }
})

# Test CORREGIDO para respuesta sin 'results'
test_that("mnk_user_byname handles API response without 'results' field", {
  # Contenido que queremos que devuelva httr::content
  mock_content_no_results <- list(message = "This is not the data you are looking for")

  # Mock de httr::GET que solo devuelve una respuesta exitosa vacía
  mock_GET_generic_success <- function(url, path, query) {
    return(structure(list(status_code = 200L), class = c("response", "handle")))
  }

  # Suplantamos GET y, crucialmente, content
  local_mocked_bindings(
    GET = mock_GET_generic_success,
    content = function(x, as) mock_content_no_results, # Forzamos el contenido sin 'results'
    .package = "httr")
  {
    expect_message(
      result <- mnk_user_byname("no_results"),
      "API response was not in the expected format \\(missing a 'results' list\\)."
    )
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0)
  }
})

