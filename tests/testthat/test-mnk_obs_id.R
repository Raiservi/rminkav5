library(testthat)
library(mockery)
library(httr)
library(jsonlite)
library(dplyr)
library(tibble)
library(stringr)

# Asegúrate de que tu función mnk_obs_id esté cargada.
# source("R/mnk_obs_id.R")

# --- DEFINICIONES DE MOCKS JSON GLOBALES ---
mock_response_json_success_full <- '{
  "total_results": 1, "page": 1, "per_page": 1, "results": [
    { "quality_grade": "research", "time_observed_at": null, "taxon_geoprivacy": null, "annotations": [], "uuid": "b165f8a7-7c0a-463c-bdd9-821f5583b401", "observed_on_details": { "date": "2021-05-23", "week": 20, "month": 5, "hour": 0, "year": 2021, "day": 23 }, "id": 3546, "cached_votes_total": 0, "identifications_most_agree": true, "created_at_details": { "date": "2022-04-16", "week": 15, "month": 4, "hour": 14, "year": 2022, "day": 16 }, "species_guess": "Gobius incognitus", "identifications_most_disagree": false, "tags": [], "positional_accuracy": null, "comments_count": 1, "site_id": 1, "created_time_zone": "Europe/Madrid", "license_code": "cc-by-nc", "observed_time_zone": "Europe/Madrid", "quality_metrics": [], "public_positional_accuracy": null, "reviewed_by": [ 3, 4 ], "oauth_application_id": null, "flags": [], "created_at": "2022-04-16T14:51:17+02:00", "description": "", "time_zone_offset": "+01:00", "project_ids_with_curator_id": [], "observed_on": "2021-05-23", "observed_on_string": "2021-05-23", "updated_at": "2023-12-21T16:28:05+01:00", "sounds": [], "place_ids": [ 55, 244, 248, 276, 374, 406, 437, 682, 684, 737 ], "captive": false, "taxon": {}, "ident_taxon_ids": [ 1, 2, 4, 3, 264553 ], "outlinks": [], "faves_count": 0, "ofvs": [], "num_identification_agreements": 1, "preferences": { "prefers_community_taxon": null }, "comments": [], "map_scale": null, "uri": "https://minka-sdg.org/observations/3546", "project_ids": [], "community_taxon_id": 35120, "geojson": { "coordinates": [ 2.491906, 41.551755 ], "type": "Point" }, "owners_identification_from_vision": false, "identifications_count": 1, "obscured": false, "num_identification_disagreements": 0, "geoprivacy": null, "location": "41.551755,2.491906", "votes": [], "ai_identified": false, "spam": false, "user": {}, "mappable": true, "identifications_some_agree": true, "project_ids_without_curator_id": [], "place_guess": "Spain", "identifications": [], "photos": [], "observation_photos": [], "community_taxon": {}, "faves": [], "non_owner_ids": [] }
  ]
}'
mock_response_json_null <- 'null'
mock_response_json_empty_object <- '{}'
mock_response_json_empty_results_array <- '{"total_results": 0, "page": 1, "per_page": 1, "results": []}'
mock_response_json_atomic_type <- '"Just a string"'

# --- Test para validación de entrada (ID) ---
test_that("mnk_obs_id handles invalid ID input", {
  expect_error(mnk_obs_id(NULL), "You must provide a single, non-empty, non-NA ID for the observation.")
  expect_error(mnk_obs_id(numeric(0)), "You must provide a single, non-empty, non-NA ID for the observation.")
  expect_error(mnk_obs_id(NA), "You must provide a single, non-empty, non-NA ID for the observation.")
  expect_error(mnk_obs_id(c(1, 2)), "You must provide a single, non-empty, non-NA ID for the observation.")
  expect_error(mnk_obs_id(" "), "You must provide a single, non-empty, non-NA ID for the observation.")
  expect_error(mnk_obs_id(""), "You must provide a single, non-empty, non-NA ID for the observation.")
})

# --- Test de éxito: ID válido y respuesta correcta ---
test_that("mnk_obs_id returns a dataframe for a valid ID and API response", {
  mock_httr_GET <- function(url = NULL,..., path = NULL, as) {
    id_from_path <- as.character(stringr::str_extract(path, "[0-9]+$"))
    response_content <- ""
    status_code <- 200L
    if (id_from_path == "3546") {
      response_content <- mock_response_json_success_full
    } else {
      status_code <- 404L
      response_content <- '{"error": "Not Found"}'
    }
    response_obj <- structure(list(url = paste0("https://api.minka-sdg.org", path), status_code = status_code, headers = list("Content-Type" = "application/json"), content = charToRaw(response_content)), class = c("response", "handle"))
    return(response_obj)
  }

  with_mocked_bindings(
    GET = mock_httr_GET,.package = "httr", {
      result <- mnk_obs_id(3546)
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 1)
      expect_true("id" %in% names(result))
      expect_equal(result$id, 3546)
      # ... (otras expectativas)
    }
  )
})

# --- Test para no resultados o respuesta vacía/nula/array vacío de 'results' de la API ---
test_that("mnk_obs_id handles empty/null/empty_results_array responses", {
  mock_httr_GET_empty <- function(url = NULL,..., path = NULL, as) {
    id_from_path <- as.character(stringr::str_extract(path, "[0-9]+$"))
    response_content <- ""
    status_code <- 200L
    if (id_from_path == "99999") { response_content <- "" }
    else if (id_from_path == "88888") { response_content <- mock_response_json_null }
    else if (id_from_path == "77777") { response_content <- mock_response_json_empty_results_array }
    else if (id_from_path == "66666") { response_content <- mock_response_json_empty_object }
    else { stop("Mock no configurado para esta URL en el test de no resultados: ", path) }
    response_obj <- structure(list(url = paste0("https://api.minka-sdg.org", path), status_code = status_code, headers = list("Content-Type" = "application/json"), content = charToRaw(response_content)), class = c("response", "handle"))
    return(response_obj)
  }

  with_mocked_bindings(
    GET = mock_httr_GET_empty,.package = "httr", {
      expect_message(result_empty_string <- mnk_obs_id(99999), "API returned an empty or null response for observation ID 99999.")
      expect_null(result_empty_string)

      expect_message(result_null_string <- mnk_obs_id(88888), "API returned an empty or null response for observation ID 88888.")
      expect_null(result_null_string)

      expect_message(result_empty_results_array <- mnk_obs_id(77777), "No data found or unexpected JSON structure for observation ID 77777.")
      expect_null(result_empty_results_array)

      expect_message(result_empty_object <- mnk_obs_id(66666), "No data found or unexpected JSON structure for observation ID 66666.")
      expect_null(result_empty_object)
    }
  )
})

# --- Test para error HTTP de la API ---
test_that("mnk_obs_id handles API HTTP error", {
  mock_httr_GET_error <- function(url = NULL,..., path = NULL, as) {
    id_from_path <- as.character(stringr::str_extract(path, "[0-9]+$"))
    response_content <- ""
    status_code <- 200L
    if (id_from_path == "50000") { status_code <- 500L }
    else if (id_from_path == "40400") { status_code <- 404L }
    else { stop("Mock no configurado para esta URL en el test de error HTTP: ", path) }
    response_obj <- structure(list(url = paste0("https://api.minka-sdg.org", path), status_code = status_code, headers = list("Content-Type" = "application/json"), content = charToRaw(response_content)), class = c("response", "handle"))
    return(response_obj)
  }

  with_mocked_bindings(
    GET = mock_httr_GET_error,.package = "httr", {
      expect_message(result_500 <- mnk_obs_id(50000), regexp = "Minka API request failed for observation ID 50000. Status code: 500")
      expect_null(result_500)

      expect_message(result_404 <- mnk_obs_id(40400), regexp = "Minka API request failed for observation ID 40400. Status code: 404")
      expect_null(result_404)
    }
  )
})

# --- Test para JSON completamente malformado (no parseable) ---
test_that("mnk_obs_id handles completely malformed JSON", {
  mock_httr_GET_malformed <- function(url = NULL,..., path = NULL, as) {
    response_content <- '{ "bad_json": "missing_bracket'
    response_obj <- structure(list(url = "some_url", status_code = 200L, headers = list("Content-Type" = "application/json"), content = charToRaw(response_content)), class = c("response", "handle"))
    return(response_obj)
  }

  with_mocked_bindings(
    GET = mock_httr_GET_malformed,.package = "httr", {
      expect_error(mnk_obs_id(10000), regexp = "Failed to parse JSON response for observation ID 10000", class = "error")
    }
  )
})

# --- Test para JSON que es un valor atómico (e.g., "Just a string") ---
test_that("mnk_obs_id handles JSON that is an atomic value", {
  mock_httr_GET_atomic_json <- function(url = NULL,..., path = NULL, as) {
    response_obj <- structure(list(url = "some_url", status_code = 200L, headers = list("Content-Type" = "application/json"), content = charToRaw(mock_response_json_atomic_type)), class = c("response", "handle"))
    return(response_obj)
  }

  with_mocked_bindings(
    GET = mock_httr_GET_atomic_json,.package = "httr", {
      expect_message(result <- mnk_obs_id(20000), regexp = "No data found or unexpected JSON structure \\(atomic type\\) for observation ID 20000.")
      expect_null(result)
    }
  )
})

# --- Test para Manejo de múltiples resultados para un ID ---
test_that("mnk_obs_id handles multiple results for one ID", {
  mock_response_multiple_results <- '{
    "total_results": 2, "page": 1, "per_page": 2, "results": [
      { "id": 111, "species_guess": "First Result" },
      { "id": 222, "species_guess": "Second Result" }
    ]
  }'
  mock_GET_multiple <- function(url = NULL,..., path = NULL, as) {
    return(structure(list(status_code = 200L, headers = list("Content-Type" = "application/json"), content = charToRaw(mock_response_multiple_results)), class = c("response", "handle")))
  }

  with_mocked_bindings(
    GET = mock_GET_multiple,.package = "httr", {
      expect_warning(result <- mnk_obs_id(111), "Multiple observations found")
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 1)
      expect_equal(result$id, 111)
    }
  )
})

# NUEVO TEST: Manejo de respuesta sin el envoltorio 'results'
test_that("mnk_obs_id handles direct dataframe response", {
  # Este mock devuelve un array de objetos directamente, sin "results"
  mock_response_direct_df <- '[
    { "id": 333, "species_guess": "Direct DataFrame Result" }
  ]'

  mock_GET_direct <- function(url = NULL,..., path = NULL, as) {
    return(structure(
      list(
        status_code = 200L,
        headers = list("Content-Type" = "application/json"),
        content = charToRaw(mock_response_direct_df)
      ),
      class = c("response", "handle")
    ))
  }

  with_mocked_bindings(
    GET = mock_GET_direct,.package = "httr", {
      result <- mnk_obs_id(333)

      # Verificar que no hay warnings ni mensajes y que procesa el dato
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 1)
      expect_equal(result$id, 333)
      expect_equal(result$species_guess, "Direct DataFrame Result")
    }
  )
})
