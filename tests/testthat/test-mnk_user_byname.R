# Test para la función mnk_user_byname

test_that("mnk_user_byname procesa correctamente una respuesta de API simulada", {

  # 1. PREPARACIÓN: Creamos una respuesta de API FALSA.
  # Esta lista imita la estructura que descubrimos con str().
  fake_api_response <- list(
    total_results = 2,
    page = 1,
    per_page = 2,
    results = list(
      list(
        id = 123,
        login = "testuser1",
        name = "Test User One",
        observations_count = 50,
        created_at = "2023-01-01T12:00:00Z"
      ),
      list(
        id = 456,
        login = "testuser2",
        name = NULL, # <-- Probamos un nombre nulo
        observations_count = 100,
        created_at = "2023-01-02T13:00:00Z"
      )
    )
  )

  # 2. EJECUCIÓN CON SUPLANTACIÓN
  with_mocked_bindings(
    {
      # Ejecutamos la función. La 'query' no importa, porque la respuesta está falseada.
      result <- mnk_user_byname("test")

      # 3. COMPROBACIÓN
      expect_s3_class(result, "tbl_df") # ¿Es un tibble?
      expect_equal(nrow(result), 2) # ¿Tiene 2 filas?
      expect_equal(ncol(result), 5) # ¿Tiene las 5 columnas que definimos?

      # Comprobamos los valores
      expect_equal(result$id, c(123, 456))
      expect_equal(result$login, c("testuser1", "testuser2"))

      # Comprobamos que el NULL se convirtió en NA
      expect_true(is.na(result$name[2]))
    },
    # Suplantamos las funciones de httr que necesitamos controlar
    .package = "httr",
    http_error = function(...) FALSE, # Decimos que la llamada fue exitosa
    content = function(...) fake_api_response # Devolvemos nuestros datos falsos
  )
})

test_that("mnk_user_byname maneja un error de API correctamente", {

  # Probamos el caso en que la API devuelve un error
  with_mocked_bindings(
    {
      result <- mnk_user_byname("error")

      # Esperamos que devuelva un tibble vacío
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 0)
    },
    .package = "httr",
    http_error = function(...) TRUE # ¡Ahora decimos que SÍ hay un error!
  )
})
