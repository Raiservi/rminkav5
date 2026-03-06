# Test final. Combinando las únicas dos estrategias que han funcionado.

test_that("Los tests se ejecutan 100% offline y pasan", {

  # --- TEST 1: MODO MENSUAL ---

  # Suplantamos las funciones de httr que dan problemas
  with_mocked_bindings(
    {
      # Y DENTRO, suplantamos nuestra función de descarga
      with_mocked_bindings(
        {
          result <- mnk_proj_obs(project_id = 1, year = 2025, month = 1)
          expect_equal(nrow(result), 5)
        },
        # La suplantación de nuestra propia función SÍ funciona así:
        download_month_data = function(...) tibble::tibble(id = 1:5)
      )
    },
    # Suplantar funciones de httr necesita la sintaxis .package
    .package = "httr",
    http_error = function(...) FALSE,
    content = function(...) list(total_results = 5) # Decimos que hay 5 para que no salga antes
  )


  # --- TEST 2: MODO ANUAL SIN DATOS ---

  with_mocked_bindings(
    {
      # El ping anual usa httr, así que lo suplantamos
      msgs <- capture_messages({
        result <- mnk_proj_obs(project_id = 1, year = 2025)
      })
      expect_equal(nrow(result), 0)
      expect_true(any(grepl("No records found for the entire year", msgs)))
    },
    .package = "httr",
    http_error = function(...) FALSE,
    content = function(...) list(total_results = 0) # El ping dice que hay 0
  )


  # --- TEST 3: MODO ANUAL CON DATOS ---

  with_mocked_bindings(
    {
      # El ping anual dice que hay datos
      with_mocked_bindings(
        {
          # El bucle de 12 meses llamará a nuestra 'download_month_data' falsa
          result <- mnk_proj_obs(project_id = 1, year = 2025)
          expect_equal(nrow(result), 60) # 12 llamadas * 5 filas/llamada = 60
        },
        download_month_data = function(...) tibble::tibble(id = 1:5)
      )
    },
    .package = "httr",
    http_error = function(...) FALSE,
    content = function(...) list(total_results = 100) # El ping dice que hay > 0
  )
})
