# utils.R

# Declaración de variables globales para evitar notas de R CMD check
utils::globalVariables(c(
  "geojson_string",
  "sf_geometry",
  ".data",                   # Para las expresiones de dplyr/purrr
  "id",                      # Columna 'id' del proyecto
  "title",                   # Columna 'title' del proyecto
  "description",             # Columna 'description' del proyecto
  "slug",                    # Columna 'slug' del proyecto
  "place_id",                # Columna 'place_id' del proyecto
  "icon",                    # Columna 'icon' del proyecto
  "header_image_url"         # Columna 'header_image_url' del proyecto
))

#' Null-coalescing operator
#'
#' See \code{rlang::\link[rlang:op-null-default]{\%||\%}} for details.
#'
#' @name null-default
#' @aliases %||%
#' @rdname null-default
#' @keywords internal
#' @importFrom rlang %||%
NULL

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL
