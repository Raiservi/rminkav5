# httptest2 hace todo el trabajo pesado de suplantación por nosotros.
# Este es el código correcto que usa with_mock_dir.

test_that("mnk_proj_obs maneja el modo mensual con POCOS datos (<10k)", {
  httptest2::with_mock_dir("mocks/mnk_proj_obs_pocos", {

    # !! IMPORTANTE !!
    # Ajusta los parámetros a un caso REAL con pocos datos
    result <- mnk_proj_obs(project_id = 420, year = 2025, month = 5)

  })

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
  expect_equal(nrow(result), 2582) # Ajustaremos este número
})

test_that("mnk_proj_obs maneja el modo mensual con MUCHOS datos (>10k)", {
  httptest2::with_mock_dir("mocks/mnk_proj_obs_muchos", {

    # !! IMPORTANTE !!
    # Ajusta los parámetros a un caso REAL con muchos datos
    result <- mnk_proj_obs(project_id = 418, year = 2025, month = 8)

  })

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
  expect_equal(nrow(result), 12008) # Ajustaremos este número
})

test_that("mnk_proj_obs maneja el modo anual y los casos de error", {
  # --- MODO ANUAL SIN DATOS ---
  httptest2::with_mock_dir("mocks/mnk_proj_obs_anual_cero", {

    # !! IMPORTANTE !!
    # Ajusta los parámetros a un caso REAL sin datos
    msgs <- capture_messages({
      result <- mnk_proj_obs(project_id = 999, year = 2030)
    })

  })
  expect_equal(nrow(result), 0)


  # --- MANEJO DE ERROR HTTP ---
  httptest2::with_mock_dir("mocks/mnk_proj_obs_error", {
    result_error <- mnk_proj_obs(project_id = 1, year = 2025, month = 1)
  })
  expect_equal(nrow(result_error), 0)
})
