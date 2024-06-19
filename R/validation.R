#' Check if an object is the correct class
#' @name validation
#' @rdname validation
#'
#' @param x An object
#'
#' @returns A logical indication whether `x` belongs to the given class
#'
#' @keywords internal
NULL

#' @name is_input_call
#' @rdname validation
#'
#' @export
is_input_call <- function(x) {
  needed <- c('function', 'input_call')
  length(class(x)) == length(needed) && all(needed %in% class(x))
}

#' @name is_col_def
#' @rdname validation
#'
#' @export
is_col_def <- function(x) {
  needed <- c(class(tibble::tibble()), 'col_def')
  length(class(x)) == length(needed) && all(needed %in% class(x))
}

#' @name is_table_def
#' @rdname validation
#'
#' @export
is_table_def <- function(x) {
  needed <- c(class(tibble::tibble()), 'table_def')
  length(class(x)) == length(needed) && all(needed %in% class(x))
}

#' @name is_faketable
#' @rdname validation
#'
#' @export
is_faketable <- function(x) {
  S7::check_is_S7(x, 'faketable')
}
