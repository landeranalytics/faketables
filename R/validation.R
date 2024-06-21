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
  {
    length(class(x)) == length(needed) &&
      all(needed %in% class(x)) &&
      rlang::is_function(x) &&
      names(formals(x)) == '...'
  }
}

#' @name is_col_def
#' @rdname validation
#'
#' @export
is_col_def <- function(x) {
  needed <- c(class(tibble::tibble()), 'col_def')
  {
    length(class(x)) == length(needed) &&
      all(needed %in% class(x)) &&
      all(c('name', 'input_call', 'cast', 'width', 'display_name') %in% colnames(x)) &&
      nrow(x) == 1 &&
      is.character(x$name) &&
      rlang::is_list(x$input_call) &&
      rlang::is_list(x$cast) &&
      rlang::is_bare_numeric(x$width) &&
      is.character(x$display_name) &&
      x$width %% 1 == 0
  }
}

#' @name is_table_def
#' @rdname validation
#'
#' @export
is_table_def <- function(x) {
  needed <- c(class(tibble::tibble()), 'table_def')
  {
    length(class(x)) == length(needed) &&
      all(needed %in% class(x)) &&
      {
        split(x, ~ seq_len(nrow(x))) |>
          purrr::map_lgl(\(x) {
            class(x) <- c(class(tibble::tibble()), 'col_def')
            is_col_def(x)
          }) |>
          all()
      } &&
      max(table(x$name)) == 1
  }
}

#' @name is_faketable
#' @rdname validation
#'
#' @export
is_faketable <- function(x) {
  S7::check_is_S7(x, 'faketable')
}
