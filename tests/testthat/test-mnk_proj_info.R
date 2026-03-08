library(testthat)
library(mockery)
library(httr)
library(jsonlite)
library(dplyr)
library(tibble)

# Asegúrate de que tu función esté cargada.
# source("R/mnk_proj_info.R")

# --- TEST SET ---

test_that("mnk_proj_info handles invalid input", {
  expect_error(mnk_proj_info(project_id = NULL, grpid = NULL), "You must provide either 'project_id' or 'grpid'")
  expect_error(mnk_proj_info(project_id = c("1", "2")), "'project_id' must be a single character string or number.")
  expect_error(mnk_proj_info(grpid = c("group1", "group2")), "'grpid' must be a single character string or number.")
})

test_that("mnk_proj_info successfully retrieves project by ID", {
  mock_response_content <- '{
    "results": [ { "id": 420, "title": "Test Project by ID", "user": {"id": 1}, "admins": [], "user_ids": [1] } ]
  }'
  mock_GET <- function(url, path, query,...) {
    return(structure(list(status_code = 200L, headers = list('Content-Type' = "application/json; charset=utf-8"), content = charToRaw(mock_response_content)), class = c("response", "handle")))
  }
  local_mocked_bindings(GET = mock_GET,.package = "httr")
  {
    result <- mnk_proj_info(project_id = 420)
    expect_type(result, "list")
    expect_equal(result$id, 420)
  }
})

test_that("mnk_proj_info successfully retrieves project by grpid", {
  mock_response_content <- '{
    "results": [ { "id": 888, "title": "Test Project by Group", "user": {"id": 1}, "admins": [], "user_ids": [1] } ]
  }'
  mock_GET_by_q <- function(url, path, query,...) {
    return(structure(list(status_code = 200L, headers = list('Content-Type' = "application/json; charset=utf-8"), content = charToRaw(mock_response_content)), class = c("response", "handle")))
  }
  local_mocked_bindings(GET = mock_GET_by_q,.package = "httr")
  {
    result <- mnk_proj_info(grpid = "test-group-slug")
    expect_type(result, "list")
    expect_equal(result$id, 888)
  }
})

test_that("mnk_proj_info handles network errors", {
  mock_GET_network_error <- function(url, path, query,...) { stop("Failed to connect") }
  local_mocked_bindings(GET = mock_GET_network_error,.package = "httr")
  {
    expect_message(result <- mnk_proj_info(project_id = 123), "Network error")
    expect_null(result)
  }
})

test_that("mnk_proj_info handles empty or null API responses", {
  mock_GET_empty <- function(url, path, query,...) {
    response_content <- if (query$id == "empty") "" else "null"
    return(structure(list(status_code = 200L, headers = list('Content-Type' = "application/json; charset=utf-8"), content = charToRaw(response_content)), class = c("response", "handle")))
  }
  local_mocked_bindings(GET = mock_GET_empty,.package = "httr")
  {
    expect_message(result_empty <- mnk_proj_info(project_id = "empty"), "API returned an empty or null response")
    expect_null(result_empty)
    expect_message(result_null <- mnk_proj_info(project_id = "null_string"), "API returned an empty or null response")
    expect_null(result_null)
  }
})

test_that("mnk_proj_info handles missing fields in API response", {
  mock_response_missing_fields <- '{
    "results": [ { "id": 777, "title": "Missing Data", "description": null, "user": null, "admins": null, "user_ids": null } ]
  }'
  mock_GET <- function(url, path, query,...) {
    return(structure(list(status_code = 200L, headers = list('Content-Type' = "application/json; charset=utf-8"), content = charToRaw(mock_response_missing_fields)), class = c("response", "handle")))
  }
  local_mocked_bindings(GET = mock_GET,.package = "httr")
  {
    result <- mnk_proj_info(project_id = 777)
    expect_true(is.na(result$description))
    expect_true(is.na(result$slug))
    expect_true(is.na(result$user_info$id))
    expect_equal(nrow(result$admins_info), 0)
    expect_length(result$user_ids_list, 0)
  }
})

test_that("mnk_proj_info handles API HTTP errors", {
  mock_GET_http_error <- function(url, path, query,...) {
    return(structure(list(status_code = 404L, headers = list('Content-Type' = "application/json; charset=utf-8"), content = charToRaw('{}')), class = c("response", "handle")))
  }
  local_mocked_bindings(GET = mock_GET_http_error,.package = "httr")
  {
    expect_message(result <- mnk_proj_info(project_id = "not_found"), "Minka API request failed")
    expect_null(result)
  }
})

test_that("mnk_proj_info handles null or empty 'results' field", {
  mock_GET_no_results <- function(url, path, query,...) {
    if (query$id == "no_results_field") {
      response_content <- '{"total_results": 0, "page": 1}'
    } else {
      response_content <- '{"total_results": 0, "page": 1, "results": []}'
    }
    return(structure(list(status_code = 200L, headers = list('Content-Type' = "application/json; charset=utf-8"), content = charToRaw(response_content)), class = c("response", "handle")))
  }
  local_mocked_bindings(GET = mock_GET_no_results,.package = "httr")
  {
    expect_message(result_no_field <- mnk_proj_info(project_id = "no_results_field"), "No project details found")
    expect_null(result_no_field)
    expect_message(result_empty_array <- mnk_proj_info(project_id = "empty_array"), "No project details found")
    expect_null(result_empty_array)
  }
})

# NUEVO TEST: 'admins' no es nulo
test_that("mnk_proj_info processes 'admins' field correctly", {
  # Mock con un campo 'admins' que contiene datos
  mock_response_with_admins <- '{
    "results": [
      {
        "id": 999,
        "title": "Project with Admins",
        "user": null,
        "admins": [
          {"id": 10, "login": "admin1", "name": "Admin One"},
          {"id": 11, "login": "admin2", "name": "Admin Two"}
        ],
        "user_ids": []
      }
    ]
  }'

  mock_GET <- function(url, path, query,...) {
    return(structure(
      list(status_code = 200L, headers = list('Content-Type' = "application/json; charset=utf-8"), content = charToRaw(mock_response_with_admins)),
      class = c("response", "handle")
    ))
  }

  local_mocked_bindings(GET = mock_GET,.package = "httr")
  {
    result <- mnk_proj_info(project_id = 999)

    expect_s3_class(result$admins_info, "tbl_df")
    expect_equal(nrow(result$admins_info), 2)
    expect_equal(result$admins_info$id, c(10, 11))
    expect_equal(result$admins_info$login, c("admin1", "admin2"))
  }
})

