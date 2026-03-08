
library(testthat)
library(mockery)
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)

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
  if (!is.null(pid) &&!is.null(yr) && is.null(mth) && per_page == 1) {
    if (pid == 123 && yr == 2025) {
      response_content <- '{"total_results": 15000, "page": 1, "per_page": 1, "results": [ {"id": 1} ]}'
    } else if (pid == 999 && yr == 2030) {
      response_content <- '{"total_results": 0, "page": 1, "per_page": 1, "results": []}'
    } else if (pid == "400_http_error_anual" && yr == 2025) {
      status_code <- 400L
      response_content <- '{"error": "Bad Request for annual ping"}'
    }
  } else if (!is.null(pid) &&!is.null(yr) &&!is.null(mth) && is.null(query$day) && per_page == 1) {
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
    status_code <- 404L
    response_content <- '{"error": "mock_httr_GET_pings Not Configured"}'
  }
  return(structure(list(url=url, status_code=status_code, headers=list('Content-Type'="application/json; charset=utf-8"), content=charToRaw(response_content)), class=c("response","handle")))
}

# --- TEST SET ---
test_that("mnk_proj_obs handles invalid input", {
  expect_error(mnk_proj_obs(project_id = NULL, year = 2025), "You must provide 'project_id' and 'year'")
  expect_error(mnk_proj_obs(project_id = 123, year = NULL), "You must provide 'project_id' and 'year'")
  expect_error(mnk_proj_obs(project_id = 123, year = 2025, month = 0), "'month' must be a number between 1 and 12.")
  expect_error(mnk_proj_obs(project_id = 123, year = 2025, month = 13), "'month' must be a number between 1 and 12.")
  expect_error(suppressWarnings(mnk_proj_obs(project_id = 123, year = 2025, month = "invalid")), "'month' must be a number between 1 and 12.")
})

test_that("mnk_proj_obs monthly mode downloads data", {
  mock_download_month_data <- function(project_id, year, current_month) {
    if (project_id == 123 && year == 2025 && current_month == 1) {
      return(tibble(id = 1:500, data = "mock_data"))
    }
    return(tibble::tibble())
  }
  with_mocked_bindings(download_month_data = mock_download_month_data,.package = "rminkav5", {
    result <- suppressMessages(mnk_proj_obs(project_id = 123, year = 2025, month = 1))
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 500)
  })
})

test_that("mnk_proj_obs annual mode handles no yearly results", {
  with_mocked_bindings(GET = mock_httr_GET_pings,.package = "httr", {
    result <- suppressMessages(mnk_proj_obs(project_id = 999, year = 2030))
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0)
  })
})

test_that("mnk_proj_obs annual mode handles yearly ping HTTP error", {
  with_mocked_bindings(GET = mock_httr_GET_pings,.package = "httr", {
    result <- suppressMessages(mnk_proj_obs(project_id = "400_http_error_anual", year = 2025))
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0)
  })
})

test_that("mnk_proj_obs annual mode processes monthly data correctly", {
  mock_download_month_data <- function(project_id, year, current_month) {
    if (project_id == 123 && year == 2025) {
      if (current_month == 1) return(tibble(id = 1:100, month = 1))
      if (current_month == 2) return(tibble(id = 101:300, month = 2))
    }
    return(tibble::tibble())
  }
  with_mocked_bindings(GET = mock_httr_GET_pings,.package = "httr", {
    with_mocked_bindings(download_month_data = mock_download_month_data,.package = "rminkav5", {
      result <- suppressMessages(mnk_proj_obs(project_id = 123, year = 2025))
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 300)
      expect_true(all(c(1, 2) %in% unique(result$month)))
    })
  })
})

test_that("download_month_data handles monthly ping HTTP error", {
  with_mocked_bindings(GET = mock_httr_GET_pings,.package = "httr", {
    result <- suppressMessages(download_month_data(project_id = "400_http_error_mensual", year = 2025, current_month = 4))
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0)
  })
})

test_that("download_month_data handles no monthly records", {
  with_mocked_bindings(GET = mock_httr_GET_pings,.package = "httr", {
    result <- suppressMessages(download_month_data(project_id = 123, year = 2025, current_month = 3))
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0)
  })
})

test_that("download_month_data handles <10000 monthly records", {
  mock_get_minka_obs <- function(params, meta = FALSE, total_res = NULL) {
    return(tibble(id = 1:total_res))
  }
  with_mocked_bindings(GET = mock_httr_GET_pings,.package = "httr", {
    with_mocked_bindings(get_minka_obs = mock_get_minka_obs,.package = "rminkav5", {
      result <- suppressMessages(download_month_data(project_id = 123, year = 2025, current_month = 1))
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 500)
    })
  })
})

test_that("download_month_data handles >10000 monthly records by day", {
  mock_get_minka_obs <- function(params, meta = FALSE, total_res = NULL) {
    if (!is.null(params$day)) {
      return(tibble(id = 1:100, day = params$day))
    }
    return(tibble::tibble())
  }
  with_mocked_bindings(GET = mock_httr_GET_pings,.package = "httr", {
    with_mocked_bindings(get_minka_obs = mock_get_minka_obs,.package = "rminkav5", {
      result <- suppressMessages(download_month_data(project_id = 123, year = 2025, current_month = 2))
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 28 * 100)
      expect_equal(length(unique(result$day)), 28)
    })
  })
})

test_that("get_minka_obs handles pagination", {
  mock_GET_pagination <- function(url, path, query,...) {
    page <- query$page; total_results_mock <- 500
    results_this_page <- list()
    if (page == 1) { results_this_page <- lapply(1:200, function(i) list(id = i)) }
    else if (page == 2) { results_this_page <- lapply(201:400, function(i) list(id = i)) }
    else if (page == 3) { results_this_page <- lapply(401:500, function(i) list(id = i)) }
    response_content <- jsonlite::toJSON(list(total_results = total_results_mock, page = page, per_page = 200, results = results_this_page), auto_unbox = TRUE)
    return(structure(list(status_code = 200L, headers = list('Content-Type' = "application/json; charset=utf-8"), content = charToRaw(response_content)), class = c("response", "handle")))
  }
  with_mocked_bindings(GET = mock_GET_pagination,.package = "httr", {
    with_mocked_bindings(process_minka_results = function(all_results) { purrr::map_dfr(all_results, ~tibble(id =.x$id)) },.package = "rminkav5", {
      result <- suppressMessages(get_minka_obs(params = list(project_id = "test"), total_res = 500))
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 500)
    })
  })
})

test_that("get_minka_obs returns metadata when meta = TRUE", {
  mock_GET_meta_test <- function(url, path, query,...) {
    if (!is.null(query$per_page) && query$per_page == 1) {
      response_content <- '{"total_results": 250, "page": 1, "per_page": 1, "results": [{"id": 1}]}'
      return(structure(list(status_code = 200L, headers = list('Content-Type' = "application/json; charset=utf-8"), content = charToRaw(response_content)), class = c("response", "handle")))
    }
    page <- query$page; total_results_mock <- 250
    if (page == 1) { results_this_page <- lapply(1:200, function(i) list(id = i)) }
    else if (page == 2) { results_this_page <- lapply(201:250, function(i) list(id = i)) }
    else { results_this_page <- list() }
    response_content <- jsonlite::toJSON(list(total_results = total_results_mock, page = page, per_page = 200, results = results_this_page), auto_unbox = TRUE)
    return(structure(list(status_code = 200L, headers = list('Content-Type' = "application/json; charset=utf-8"), content = charToRaw(response_content)), class = c("response", "handle")))
  }
  with_mocked_bindings(GET = mock_GET_meta_test,.package = "httr", {
    with_mocked_bindings(process_minka_results = function(all_results) { purrr::map_dfr(all_results, ~tibble(id =.x$id)) },.package = "rminkav5", {
      result <- suppressMessages(get_minka_obs(params = list(project_id = "test_meta"), meta = TRUE))
      expect_type(result, "list")
      expect_true(all(c("meta", "data") %in% names(result)))
      expect_s3_class(result$data, "tbl_df")
      expect_equal(nrow(result$data), 250)
      expect_equal(result$meta$found, 250)
      expect_equal(result$meta$returned, 250)
    })
  })
})

test_that("get_minka_obs handles internal ping HTTP error", {
  mock_GET_internal_ping_error <- function(url, path, query,...) {
    if (!is.null(query$per_page) && query$per_page == 1) {
      return(structure(list(status_code = 400L, headers = list('Content-Type' = "application/json; charset=utf-8"), content = charToRaw('{"error": "Bad Request on ping"}')), class = c("response", "handle")))
    }
    return(structure(list(status_code = 200L, headers = list('Content-Type' = "application/json; charset=utf-8"), content = charToRaw('{"results":[]}')), class = c("response", "handle")))
  }
  with_mocked_bindings(GET = mock_GET_internal_ping_error,.package = "httr", {
    result <- suppressMessages(get_minka_obs(params = list(project_id = "test")))
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0)
  })
})

test_that("get_minka_obs handles internal ping and >10k results warning", {
  mock_GET_large_dataset <- function(url, path, query,...) {
    per_page <- query$per_page
    if (!is.null(per_page) && per_page == 1) {
      response_content <- '{"total_results": 11000, "page": 1, "per_page": 1, "results": [{"id": 1}]}'
      return(structure(list(status_code = 200L, headers = list('Content-Type' = "application/json; charset=utf-8"), content = charToRaw(response_content)), class = c("response", "handle")))
    }
    page <- query$page; results_this_page <- lapply(1:200, function(i) list(id = ((page - 1) * 200) + i))
    response_content <- jsonlite::toJSON(list(total_results = 11000, page = page, per_page = 200, results = results_this_page), auto_unbox = TRUE)
    return(structure(list(status_code = 200L, headers = list('Content-Type' = "application/json; charset=utf-8"), content = charToRaw(response_content)), class = c("response", "handle")))
  }
  with_mocked_bindings(GET = mock_GET_large_dataset,.package = "httr", {
    with_mocked_bindings(process_minka_results = function(all_results) { purrr::map_dfr(all_results, ~tibble(id =.x$id)) },.package = "rminkav5", {
      msgs <- capture_messages({ result <- get_minka_obs(params = list(project_id = "large_dataset")) })
      expect_match(msgs, "WARNING: The query found 11000 records. The API limits the download to the first few 10000.")
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 10000)
    })
  })
})

test_that("process_minka_results handles empty list", {
  result <- process_minka_results(list())
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("process_minka_results correctly transforms list with missing data", {
  mock_data_list <- list(
    jsonlite::fromJSON(mock_json_single_observation_record_content, simplifyVector = FALSE),
    list(id = 2, observed_on = NULL, observed_on_details = list(year = 2025), geojson = list(coordinates = c(3.0, 42.0)), taxon = list(default_photo = NULL, id = 101), user = NULL)
  )
  result <- process_minka_results(mock_data_list)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_true(is.na(result$observed_on[2]))
  expect_true(is.na(result$uri.photo.square[2]))
  expect_true(is.na(result$user_id[2]))
  expect_equal(result$id[1], 12345)
  expect_equal(result$longitude[2], 3.0)
})

# NUEVO TEST para cubrir la línea 199 de get_minka_obs
test_that("get_minka_obs handles zero or null total_results from ping", {
  mock_GET_no_results_ping <- function(url, path, query,...) {
    if (!is.null(query$per_page) && query$per_page == 1) {
      if (query$project_id == "zero_results") {
        response_content <- '{"total_results": 0, "results": []}'
      } else {
        response_content <- '{"page": 1, "results": []}' # "total_results" es NULL
      }
      return(structure(list(status_code = 200L, headers = list('Content-Type'="application/json; charset=utf-8"), content = charToRaw(response_content)), class = c("response", "handle")))
    }
    return(structure(list(status_code = 500L), class = c("response", "handle")))
  }

  with_mocked_bindings(GET = mock_GET_no_results_ping,.package = "httr", {
    # Prueba del caso 1: total_results = 0
    result_zero <- get_minka_obs(params = list(project_id = "zero_results"))
    expect_s3_class(result_zero, "tbl_df"); expect_equal(nrow(result_zero), 0)

    # Prueba del caso 2: total_results = NULL
    result_null <- get_minka_obs(params = list(project_id = "null_results"))
    expect_s3_class(result_null, "tbl_df"); expect_equal(nrow(result_null), 0)
  })
})
