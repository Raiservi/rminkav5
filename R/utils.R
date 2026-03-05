
utils::globalVariables(c("geojson_string", "sf_geometry"))

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
