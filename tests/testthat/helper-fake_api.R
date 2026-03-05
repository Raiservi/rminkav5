
#    # Esta función crea un objeto que es idéntico a una respuesta real de httr,
# lo que evitará todos los errores que hemos visto.
create_fake_response <- function(payload) {

  # Convertimos la carga útil (nuestra lista de 'total_results') a formato raw JSON,
  # que es lo que httr espera en el campo 'content'.
  raw_content <- charToRaw(jsonlite::toJSON(payload, auto_unbox = TRUE))

  response <- list(
    status_code = 200,
    headers = list('Content-Type' = 'application/json'),
    content = raw_content # <-- El contenido debe estar en formato 'raw'
  )

  # Le asignamos la clase correcta para que httr::content() y httr::http_error() la reconozcan.
  class(response) <- "response"
  return(response)
}
