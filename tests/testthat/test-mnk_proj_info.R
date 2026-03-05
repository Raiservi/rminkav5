library(testthat)
library(rminkav5)
library(httr)
library(mockery) # Asegúrate de que este paquete está instalado

# --- Función auxiliar para crear respuestas mock de httr ---
# La creamos una vez para no repetir código en cada test
create_mock_response <- function(json_content, status_code = 200L, url = "mocked_url") {
  response_obj <- list(
    url = url,
    status_code = status_code,
    headers = list("Content-Type" = "application/json"),
    content = charToRaw(json_content)
  )
  class(response_obj) <- c("response", "handle")
  return(response_obj)
}

# --- Test para argumentos inválidos ---
test_that("mnk_proj_info handles invalid arguments", {
  # No necesitamos mockear aquí, ya que la función debe fallar antes de la llamada a la API
  expect_error(
    mnk_proj_info(project_id = NULL, grpid = NULL),
    "You must provide either 'project_id' or 'grpid'. Both cannot be NULL."
  )

  # --- ESTA ES LA LÍNEA QUE HA CAMBIADO ---
  expect_error(
    mnk_proj_info(project_id = list(a = 1)),
    "'project_id' must be a single character string or number."
  )
})

# --- Test para respuesta válida de la API (caso completo) ---
test_that("mnk_proj_info processes a full, valid API response", {
  mock_json <- '{
    "results": [{
      "id": "proj123", "title": "Proyecto Ejemplo Completo", "description": "Una descripción detallada.",
      "slug": "proyecto-ejemplo-completo", "created_at": "2023-01-01T12:00:00Z",
      "user": { "id": 1, "login": "usuario1", "name": "Usuario Uno" },
      "admins": [{"id": 10, "login": "admin1", "name": "Admin Uno"}, {"id": 11, "login": "admin2", "name": "Admin Dos"}],
      "user_ids": [101, 102, 103]
    }]
  }'
  mock_response <- create_mock_response(mock_json)

  # stub() intercepta la llamada a httr::GET dentro de mnk_proj_info
  stub(mnk_proj_info, "httr::GET", mock_response)

  result <- mnk_proj_info(project_id = "proj123")

  expect_type(result, "list")
  expect_named(result, c("title", "description", "slug", "created_at", "id", "user_info", "admins_info", "user_ids_list", "subscrib_users"))
  expect_equal(result$title, "Proyecto Ejemplo Completo")
  expect_equal(result$id, "proj123")
  expect_s3_class(result$admins_info, "tbl_df")
  expect_equal(nrow(result$admins_info), 2)
  expect_equal(result$subscrib_users, 3)
})

# --- Test para API no encuentra el proyecto ---
test_that("mnk_proj_info handles 'not found' response", {
  mock_response <- create_mock_response('{"results": []}')

  stub(mnk_proj_info, "httr::GET", mock_response)

  expect_message(result <- mnk_proj_info(project_id = "not_found_id"), "No project details found")
  expect_null(result)
})

# --- Test para API devuelve un error HTTP ---
test_that("mnk_proj_info handles API HTTP error", {
  mock_response <- create_mock_response('{"error": "server exploded"}', status_code = 500L)

  stub(mnk_proj_info, "httr::GET", mock_response)

  expect_message(result <- mnk_proj_info(project_id = "error_id"), regexp = "Minka API request failed.*Status code: 500")
  expect_null(result)
})

# --- Test para proyecto sin campos opcionales ---
test_that("mnk_proj_info handles missing optional fields", {
  mock_json <- '{
    "results": [{
      "id": "proj_minimal", "title": "Proyecto Minimalista", "description": null,
      "slug": "proyecto-minimal", "created_at": "2023-01-01T12:00:00Z",
      "user": {"id": 1, "login": "usuario1", "name": "Usuario Uno"},
      "admins": [], "user_ids": null
    }]
  }'
  mock_response <- create_mock_response(mock_json)

  stub(mnk_proj_info, "httr::GET", mock_response)

  result <- mnk_proj_info(project_id = "proj_minimal")

  expect_type(result, "list")
  expect_true(is.na(result$description))
  expect_s3_class(result$admins_info, "tbl_df")
  expect_equal(nrow(result$admins_info), 0)
  expect_equal(result$user_ids_list, integer(0))
  expect_equal(result$subscrib_users, 0)
})
