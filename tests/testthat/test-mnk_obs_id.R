library(testthat)
library(rminkav5)
library(httr)
library(jsonlite)
library(purrr)
library(tibble)


test_that("mnk_obs_id throws error for invalid id", {

  expect_error(mnk_obs_id(NULL), "You must provide a single, non-empty, non-NA ID for the observation.")
  expect_error(mnk_obs_id(NA_character_), "You must provide a single, non-empty, non-NA ID for the observation.")
  expect_error(mnk_obs_id(""), "You must provide a single, non-empty, non-NA ID for the observation.")
  expect_error(mnk_obs_id(" "), "You must provide a single, non-empty, non-NA ID for the observation.")
  expect_error(mnk_obs_id(c("123", "456")), "You must provide a single, non-empty, non-NA ID for the observation.")
})


test_that("mnk_obs_id handles API HTTP errors", {
  mock_httr_GET <- function(url = NULL, ..., path = NULL, as) {
    full_url <- if (!is.null(url) && !is.null(path)) paste0(url, path) else url
    if (grepl("v1/observations/error_id", full_url)) {
      response_obj <- list(
        url = full_url,
        status_code = 404L,
        headers = list(`Content-Type` = "application/json"),
        content = charToRaw('{"error": "Observation not found"}')
      )
      class(response_obj) <- c("response", "handle")
      return(response_obj)
    } else {
      stop("Mock no configurado para esta URL en el test de error: ", full_url)
    }
  }

  with_mocked_bindings(
    GET = mock_httr_GET,
    .package = "httr",
    {
      expect_message(result <- mnk_obs_id("error_id"), regexp = "Minka API request failed for observation ID error_id. Status code: 404")
      expect_null(result)
    }
  )
})

test_that("mnk_obs_id handles empty or null JSON response", {
  mock_httr_GET <- function(url = NULL, ..., path = NULL, as) {
    full_url <- if (!is.null(url) && !is.null(path)) paste0(url, path) else url
    if (grepl("v1/observations/empty_json", full_url)) {
      response_obj <- list(
        url = full_url,
        status_code = 200L,
        headers = list(`Content-Type` = "application/json"),
        content = charToRaw('{}')
      )
      class(response_obj) <- c("response", "handle")
      return(response_obj)
    } else if (grepl("v1/observations/null_json", full_url)) {
      response_obj <- list(
        url = full_url,
        status_code = 200L,
        headers = list(`Content-Type` = "application/json"),
        content = charToRaw('null')
      )
      class(response_obj) <- c("response", "handle")
      return(response_obj)
    } else {
      stop("Mock no configurado para esta URL en el test de JSON vacio/nulo: ", full_url)
    }
  }

  with_mocked_bindings(
    GET = mock_httr_GET,
    .package = "httr",
    {
      expect_message(result <- mnk_obs_id("empty_json"), regexp = "No data found for observation ID empty_json.")
      expect_null(result)

      expect_message(result <- mnk_obs_id("null_json"), regexp = "API returned an empty or null response for observation ID null_json.")
      expect_null(result)
    }
  )
})


test_that("mnk_obs_id returns tibble for valid id", {
  mock_response_json <- '{
    "id": 113711,
    "scientific_name": "Araneus diadematus",
    "datetime": "2018-04-27 00:00:00 +0100",
    "description": "Sobre una herba de Sant Jordi",
    "place_guess": "Ctra. de Sant Cugat, 114-132 (ctra. de l\'Arrabassada). Barcelona",
    "latitude": 41.42569,
    "longitude": 2.133057,
    "tag_list": null,
    "common_name": "Cross Orbweaver",
    "url": "https://minka-sdg.org/observations/113711",
    "image_url": "https://minka-sdg.org/attachments/local_photos/files/144528/medium.jpeg",
    "user_login": "argelaga",
    "iconic_taxon_name": "Arachnida",
    "taxon_id": 245814,
    "num_identification_agreements": 1,
    "num_identification_disagreements": 0,
    "observed_on_string": "2018-04-27",
    "observed_on": "2018-04-27",
    "time_observed_at": null,
    "time_zone": "Madrid",
    "positional_accuracy": null,
    "public_positional_accuracy": null,
    "geoprivacy": null,
    "taxon_geoprivacy": null,
    "coordinates_obscured": false,
    "positioning_method": null,
    "positioning_device": null,
    "user_id": 478,
    "user_name": null,
    "created_at": "2023-04-03 13:32:42 UTC",
    "updated_at": "2025-12-03 16:26:02 UTC",
    "quality_grade": "research",
    "license": "CC-BY-NC",
    "sound_url": null,
    "oauth_application_id": null,
    "captive_cultivated": false
  }'

  mock_httr_GET <- function(url = NULL, ..., path = NULL, as) {
    full_url <- if (!is.null(url) && !is.null(path)) paste0(url, path) else url
    if (grepl("v1/observations/113711", full_url)) {
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
      result <- mnk_obs_id("113711")

      # Verificar que el resultado es un tibble
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 1) # Es una sola observación
      expect_true("id" %in% names(result))
      expect_equal(result$id, 113711)
      expect_equal(result$scientific_name, "Araneus diadematus")
      expect_equal(result$latitude, 41.42569)
      expect_equal(result$image_url, "https://minka-sdg.org/attachments/local_photos/files/144528/medium.jpeg")
    }
  )
})
