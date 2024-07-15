#' Manipulate a `faketable` in Shiny
#' @name shiny-manipulate
#' @rdname shiny-manipulate
#'
#' @description These are convenient references to [faketables::methods] for use
#'   in Shiny applications.
#'
#' @param inputId An ID string used to identify the module UI
#' @param reactive_faketable A [shiny::reactive] object that holds an underlying
#'   [faketables::faketable()]
#' @param data A data.frame to add to the data in the [faketables::faketable()]
#'   object. If it does not already have a primary key column as specified in
#'   [faketables::table_def()], one will be created and primary keys will be
#'   generated.
#'
#' @returns These methods do not return, but do reassign the `faketable`
#'   reactive object in the parent environment
#'
#' @keywords internal
NULL

#' @keywords internal
.check_shiny_method_args <- function(reactive_faketable, data) {
  if (!shiny::is.reactive(reactive_faketable) | !is.data.frame(data))
    cli::cli_abort('{.fun faketables::faketablesInsert} requires a {.fun shiny::reactive} object and a data.frame')
}

#' @keywords internal
.call_shiny_method <- function(method, inputId, reactive_faketable, data, env) {
  reactive_faketable |>
    substitute(env = rlang::caller_env()) |>
    deparse() |>
    assign(
      value = faketablesServer(
        inputId = inputId,
        faketable = method(do.call(reactive_faketable, args = list()), data)
      ),
      envir = env
    )
}

#' @name faketablesInsert
#' @rdname shiny-manipulate
#'
#' @seealso For more details, see the vignette by running
#'   \code{vignette('inserting_data')}
#'
#' @export
faketablesInsert <- function(inputId = 'faketables', reactive_faketable, data) {
  .check_shiny_method_args(reactive_faketable, data)
  env <- rlang::env_parent(rlang::caller_env(), n = 2)
  .call_shiny_method(insert, inputId, reactive_faketable, data, env)
}

#' @name faketablesUpdate
#' @rdname shiny-manipulate
#'
#' @export
faketablesUpdate <- function(inputId = 'faketables', reactive_faketable, data) {
  .check_shiny_method_args(reactive_faketable, data)
  env <- rlang::env_parent(rlang::caller_env(), n = 2)
  .call_shiny_method(update, inputId, reactive_faketable, data, env)
}

#' @name faketablesDelete
#' @rdname shiny-manipulate
#'
#' @export
faketablesDelete <- function(inputId = 'faketables', reactive_faketable, data) {
  .check_shiny_method_args(reactive_faketable, data)
  env <- rlang::env_parent(rlang::caller_env(), n = 2)
  .call_shiny_method(delete, inputId, reactive_faketable, data, env)
}
