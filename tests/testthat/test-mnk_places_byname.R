library(testthat)
library(mockery)
library(httr)
library(jsonlite)
library(purrr)
library(dplyr)
library(tibble)
library(stringr)

# Asegúrate de que tu función mnk_places_byname esté cargada.
# source("R/mnk_places_byname.R") # ¡Descomenta si tu paquete no está instalado o cargado!

# --- DEFINICIONES DE MOCKS JSON GLOBALES ---
mock_response_json_success_multiple <- '{
  "results": [
    {"id":"123", "slug":"area-marina-sant-feliu", "name":"Area marina Sant Feliu", "display_name":"Area marina Sant Feliu de Guíxols", "location":"41.7597741479,3.0226481505", "bbox_area":0.005301438053870476},
    {"id":"456", "slug":"otra-area", "name":"Otra Area", "display_name":"Otra Area en alguna parte", "location":"40.00,2.00", "bbox_area":0.01}
  ]
}'
mock_response_json_success_single <- '{
  "results": [
    {"id":"100", "slug":"single-place", "name":"Single Place", "display_name":"Just one place here", "location":"30.00,1.00", "bbox_area":0.0075}
  ]
}'
mock_response_json_malformed_for_parsing <- '{"error": "bad json", "results": ['
mock_response_json_no_results_key <- '{"some_other_key": "some_value"}'
mock_response_json_empty_results <- '{"results": []}'

# --- Tests ---
test_that("mnk_places_byname throws error for invalid query", {
  expect_error(mnk_places_byname(NULL), "You must provide a non-empty 'query' string.")
  expect_error(mnk_places_byname(""), "You must provide a non-empty 'query' string.")
  expect_error(mnk_places_byname(123), "You must provide a non-empty 'query' string.")
  expect_error(mnk_places_byname(" "), "You must provide a non-empty 'query' string.")
})

test_that("mnk_places_byname returns a tibble for a valid query", {
  mock_httr_GET <- function(url = NULL,..., path = NULL, as) {
    query_param_encoded <- stringr::str_extract(path, "(?<=q=).*")
    query_param <- URLdecode(query_param_encoded)
    response_content <- if (grepl("Area marina Sant Feliu", query_param, ignore.case = TRUE)) {
      mock_response_json_success_multiple
    } else {
      mock_response_json_success_single
    }
    return(structure(list(status_code = 200L, headers = list("Content-Type" = "application/json"), content = charToRaw(response_content)), class = c("response", "handle")))
  }

  with_mocked_bindings(GET = mock_httr_GET,.package = "httr", {
    result_multiple <- mnk_places_byname("Area marina Sant Feliu")
    expect_s3_class(result_multiple, "tbl_df")
    expect_equal(nrow(result_multiple), 2)

    result_single <- mnk_places_byname("Single Place")
    expect_s3_class(result_single, "tbl_df")
    expect_equal(nrow(result_single), 1)
  })
})

test_that("mnk_places_byname handles no results from API", {
  mock_httr_GET <- function(url = NULL,..., path = NULL, as) {
    query_param_encoded <- stringr::str_extract(path, "(?<=q=).*")
    query_param <- URLdecode(query_param_encoded)
    response_content <- if (grepl("No existente", query_param, ignore.case = TRUE)) {
      mock_response_json_empty_results
    } else {
      ""
    }
    return(structure(list(status_code = 200L, headers = list("Content-Type" = "application/json"), content = charToRaw(response_content)), class = c("response", "handle")))
  }

  with_mocked_bindings(GET = mock_httr_GET,.package = "httr", {
    expect_message(result <- mnk_places_byname("No existente"), "No places found for your query.")
    expect_null(result)
    expect_message(result_empty_content <- mnk_places_byname("Empty response"), "API returned an empty response.")
    expect_null(result_empty_content)
  })
})

test_that("mnk_places_byname handles API HTTP error", {
  mock_httr_GET <- function(url = NULL,..., path = NULL, as) {
    return(structure(list(status_code = 500L, content = charToRaw('{}')), class = c("response", "handle")))
  }

  with_mocked_bindings(GET = mock_httr_GET,.package = "httr", {
    expect_message(result_500 <- mnk_places_byname("error_query_500"), "Minka API request failed. Status code: 500")
    expect_null(result_500)
  })
})

test_that("mnk_places_byname handles malformed JSON or missing 'results' key", {
  mock_httr_GET <- function(url = NULL,..., path = NULL, as) {
    query_param_encoded <- stringr::str_extract(path, "(?<=q=).*")
    query_param <- URLdecode(query_param_encoded)
    response_content <- switch(query_param,
                               "Malformed JSON" = mock_response_json_malformed_for_parsing,
                               "No results key" = mock_response_json_no_results_key,
                               "Malformed location" = '{"results": [{"location": "invalid"}]}',
                               "Location no numeric" = '{"results": [{"location": "41.78,abc"}]}',
                               stop("Mock malformed no configurado")
    )
    return(structure(list(status_code = 200L, headers = list("Content-Type" = "application/json"), content = charToRaw(response_content)), class = c("response", "handle")))
  }

  with_mocked_bindings(GET = mock_httr_GET,.package = "httr", {
    # Captura cualquier error de parseo de jsonlite
    expect_error(mnk_places_byname("Malformed JSON"))

    # Cubre el if (is.null(parsed_json$results) ... )
    expect_message(result <- mnk_places_byname("No results key"), "No places found for your query.")
    expect_null(result)

    # Cubre el stop() para location mal formada, suprimiendo las advertencias de as.numeric
    expect_error(suppressWarnings(mnk_places_byname("Malformed location")), "Invalid 'location' format")
    expect_error(suppressWarnings(mnk_places_byname("Location no numeric")), "Invalid 'location' format")
  })
})
