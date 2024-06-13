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
#' @param input A [faketables::input_call()] object
#' @param cast A bare function call to convert the column to its intended class
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
col_def <- function(name, input, cast, width, display_name = name, ...) {
  structure(
    tibble::tibble(
      'name' = name,
      'input_call' = list(input),
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
#'   definition will be displayed.
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
  if (length(c_def) == 0) cli::cli_abort('{.fun faketables::table_def} requires at least one {.fun faketables::col_def} object')
  structure(
    {
      c_def <- .better_list_flatten(c_def, tibble::is_tibble)
      do.call(rbind, c_def)
    },
    class = c(class(tibble::tibble()), 'table_def')
  )
}

