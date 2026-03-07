library(testthat)
library(mockery) # Para local_mocked_bindings
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)

# Asegúrate de que tus funciones estén cargadas.
# source("R/mnk_proj_obs.R")
# ... y las demás ...

# --- MOCKS JSON GLOBALES ---
mock_json_single_observation_record_content <- '{
  "id": 12345, "observed_on": "2025-01-15",
  "observed_on_details": {"year": 2025, "month": 1, "week":3, "day":15, "hour":10},
  "created_at": "2025-01-15T10:00:00Z", "updated_at": "2025-01-15T10:00:00Z",
  "geojson": {"coordinates": [2.0, 41.0]}, "positional_accuracy": 5,
  "taxon_geoprivacy": "obscured", "obscured": true, "uri": "http://minka/obs/12345",
  "taxon": {"default_photo": {"square_url": "sq.jpg", "medium_url": "med.jpg"}, "id":100, "name":"Sp", "rank":"species", "min_species_ancestry":"anc", "endemic":true, "threatened":false, "introduced":false, "native":true},
  "quality_grade": "research", "species_guess": "Guess", "user": {"id":1, "login":"user1"}
}'

# --- MOCK PRINCIPAL PARA HTTR::GET ---
mock_httr_GET_pings <- function(url = NULL, path = NULL, query = NULL,...) {
  pid <- query$project_id
  yr <- query$year
  mth <- query$month
  per_page <- query$per_page

  status_code <- 200L
  response_content <- ""

  # --- PING ANUAL (mnk_proj_obs) ---
  if (!is.null(pid) &&!is.null(yr) && is.null(mth) && per_page == 1) {
    if (pid == 123 && yr == 2025) {
      response_content <- '{"total_results": 15000, "page": 1, "per_page": 1, "results": [ {"id": 1} ]}'
    } else if (pid == 999 && yr == 2030) {
      response_content <- '{"total_results": 0, "page": 1, "per_page": 1, "results": []}'
    } else if (pid == "400_http_error_anual" && yr == 2025) {
      status_code <- 400L
      response_content <- '{"error": "Bad Request for annual ping"}'
    }
  }
  # --- PING MENSUAL ---
  else if (!is.null(pid) &&!is.null(yr) &&!is.null(mth) && is.null(query$day) && per_page == 1) {
    if (pid == 123 && yr == 2025 && mth == 1) {
      response_content <- '{"total_results": 500, "page": 1, "per_page": 1, "results": [ {"id": 101} ]}'
    } else if (pid == 123 && yr == 2025 && mth == 2) {
      response_content <- '{"total_results": 12000, "page": 1, "per_page": 1, "results": [ {"id": 201} ]}'
    } else if (pid == 123 && yr == 2025 && mth == 3) {
      response_content <- '{"total_results": 0, "page": 1, "per_page": 1, "results": []}'
    } else if (pid == "400_http_error_mensual" && yr == 2025 && mth == 4) {
      status_code <- 400L
      response_content <- '{"error": "Bad Request for monthly ping"}'
    }
  } else {
    message(paste("WARNING: mock_httr_GET_pings not configured for specific call: ", paste(c("path", path, names(query), query), collapse=",")))
    status_code <- 404L
    response_content <- '{"error": "mock_httr_GET_pings Not Configured"}'
  }

  response_obj <- structure(
    list(
      url = url,
      status_code = status_code,
      headers = list('Content-Type' = "application/json; charset=utf-8"),
      content = charToRaw(response_content)
    ),
    class = c("response", "handle")
  )
  return(response_obj)
}

# --- TEST SET ---

# Test de mnk_proj_obs (validación de entrada)
test_that("mnk_proj_obs handles invalid input", {
  expect_error(mnk_proj_obs(project_id = NULL, year = 2025), "You must provide 'project_id' and 'year'")
  expect_error(mnk_proj_obs(project_id = 123, year = NULL), "You must provide 'project_id' and 'year'")
  expect_error(mnk_proj_obs(project_id = 123, year = 2025, month = 0), "'month' must be a number between 1 and 12.")
  expect_error(mnk_proj_obs(project_id = 123, year = 2025, month = 13), "'month' must be a number between 1 and 12.")
  expect_error(suppressWarnings(mnk_proj_obs(project_id = 123, year = 2025, month = "invalid")), "'month' must be a number between 1 and 12.")
})

# Test para el modo mensual de mnk_proj_obs
test_that("mnk_proj_obs monthly mode downloads data", {
  mock_download_month_data <- function(project_id, year, current_month) {
    if (project_id == 123 && year == 2025 && current_month == 1) {
      return(tibble(id = 1:500, data = "mock_data"))
    }
    return(tibble::tibble())
  }

  local_mocked_bindings(
    download_month_data = mock_download_month_data,
    .package = "rminkav5"
  )
  {
    result <- suppressMessages(mnk_proj_obs(project_id = 123, year = 2025, month = 1))

    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 500)
  }
})

# Test para el modo anual - sin resultados
test_that("mnk_proj_obs annual mode handles no yearly results", {
  local_mocked_bindings(
    GET = mock_httr_GET_pings,
    .package = "httr"
  )
  {
    result <- suppressMessages(mnk_proj_obs(project_id = 999, year = 2030))

    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0)
  }
})

# Test para el modo anual - error en el ping
test_that("mnk_proj_obs annual mode handles yearly ping HTTP error", {
  local_mocked_bindings(
    GET = mock_httr_GET_pings,
    .package = "httr"
  )
  {
    result <- suppressMessages(mnk_proj_obs(project_id = "400_http_error_anual", year = 2025))

    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0)
  }
})

# Test para el modo anual con resultados
test_that("mnk_proj_obs annual mode processes monthly data correctly", {
  mock_download_month_data <- function(project_id, year, current_month) {
    if (project_id == 123 && year == 2025) {
      if (current_month == 1) return(tibble(id = 1:100, month = 1))
      if (current_month == 2) return(tibble(id = 101:300, month = 2))
    }
    return(tibble::tibble())
  }

  local_mocked_bindings(
    GET = mock_httr_GET_pings,
    .package = "httr"
  )
  {
    local_mocked_bindings(
      download_month_data = mock_download_month_data,
      .package = "rminkav5"
    )
    {
      result <- suppressMessages(mnk_proj_obs(project_id = 123, year = 2025))

      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 300)
      expect_true(all(c(1, 2) %in% unique(result$month)))
    }
  }
})

# Test para download_month_data (error en el ping mensual)
test_that("download_month_data handles monthly ping HTTP error", {
  local_mocked_bindings(
    GET = mock_httr_GET_pings,
    .package = "httr"
  )
  {
    result <- suppressMessages(download_month_data(project_id = "400_http_error_mensual", year = 2025, current_month = 4))

    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0)
  }
})

# NUEVO TEST: download_month_data (sin resultados)
test_that("download_month_data handles no monthly records", {
  local_mocked_bindings(
    GET = mock_httr_GET_pings,
    .package = "httr"
  )
  {
    # El mes 3 para project_id=123 está configurado para devolver 0 resultados
    result <- suppressMessages(download_month_data(project_id = 123, year = 2025, current_month = 3))

    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0)
  }
})
