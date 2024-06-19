#' Create a `faketable` bare input call
#'
#' @param fun A bare [shiny] input function
#' @param args A list of named arguments to pass to `fun`. This should not
#'   include an `inputId`.
#'
#' @returns A [faketables::input_call()] object
#' @export
#'
#' @examples
#' input_call(shiny::textInput, args = list(label = 'Text Input'))
input_call <- function(fun, args) {
  if (!rlang::is_function(fun) | !rlang::is_list(args))
    cli::cli_abort('{.fun faketables::input_call} expects `fun` to be a bare function call and `args` to be a list')
  structure(
    {
      function(...) {
        rlang::call2(fun, !!!args) |>
          rlang::call_modify(..., .homonyms = 'last') |>
          rlang::eval_tidy()
      }
    },
    class = c('function', 'input_call')
  )
}

#' Create a `faketable` column definition
#'
#' @param name The column name
#' @param input_call A [faketables::input_call()] object
#' @param cast A bare function call to convert the column to its intended class.
#'   It is important that this match exactly, or the `updated` table will
#'   contain all rows.
#' @param display_name The name to use for the column header
#' @inheritParams shiny::column
#'
#' @returns A [faketables::col_def()] object
#' @export
#'
#' @examples
#' # to create a col_def for mtcars$mpg
#' # it is a shiny::textInput that is disabled using shinyjs::disabled
#' col_def(
#'   name = 'mpg',
#'   input = input_call(
#'     fun = \(...) { shinyjs::disabled(shiny::textInput(...)) },
#'     args = list(label = NULL, placeholder = 'mpg')
#'   ),
#'   cast = as.numeric,
#'   width = 3,
#'   display_name = 'MPG'
#' )
col_def <- function(name, input_call, cast, width, display_name = name, ...) {
  if (!rlang::is_character(name) | !rlang::is_character(display_name))
    cli::cli_abort('{.fun faketables::col_def} expects `name` and `dispaly_name` to be a character')
  if (!is_input_call(input_call))
    cli::cli_abort('{.fun faketables::col_def} expects `input_call` to be an {.fun faketables::input_call}')
  if (!rlang::is_function(cast))
    cli::cli_abort('{.fun faketables::col_def} expects `cast` to be a bare function call')
  if (!rlang::is_bare_numeric(width) | width %% 1 != 0)
    cli::cli_abort('{.fun faketables::col_def} expects `width` to be a whole number')
  structure(
    tibble::tibble(
      'name' = name,
      'input_call' = list(input_call),
      'cast' = list(cast),
      'width' = width,
      'display_name' = display_name,
      ...
    ),
    class = c(class(tibble::tibble()), 'col_def')
  )
}

#' Create a `faketable` table definition
#'
#' @param ... At least one [faketables::col_def()] object. Only columns with a
#'   definition will be displayed. If a column does not have a definition and
#'   rows are added, any column without a definition will receive an `NA` value.
#'
#' @returns A [faketables::table_def()] object
#' @export
#'
#' @examples
#' # to create a table_def to display mtcars$mpg
#' # see faketables::col_def() for more information
#' table_def(
#'  col_def(
#'    name = 'mpg',
#'    input = input_call(
#'      fun = \(...) { shinyjs::disabled(shiny::textInput(...)) },
#'      args = list(label = NULL, placeholder = 'mpg')
#'    ),
#'    cast = as.numeric,
#'    width = 3,
#'    display_name = 'MPG'
#'  )
#' )
table_def <- function(...) {
  c_def <- rlang::list2(...)
  if (rlang::dots_n(...) == 1 & rlang::is_list(..1) & !is_col_def(..1)) c_def <- c_def[[1]]
  if (length(c_def) == 0)
    cli::cli_abort('{.fun faketables::table_def} requires at least one {.fun faketables::col_def} object')
  if (!all(purrr::map_lgl(c_def, is_col_def)))
    cli::cli_abort('{.fun faketables::table_def} requires all arguments to be a {.fun faketables::col_def} object')
  structure(
    do.call(rbind, c_def),
    class = c(class(tibble::tibble()), 'table_def')
  )
}
