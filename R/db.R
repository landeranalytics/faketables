#' Write a `faketable` to a database table
#' @name dbWriteTable
#' @rdname db
#'
#' @description A convenience function to use the `@inserted`, `@updated`, and
#'   `@deleted` tables to write back to the database using a series of
#'   corresponding [dplyr::rows] functions, then restart the `faketablesServer`
#'   with the new data to maintain integrity for future writes.
#'
#' @param src A DBIConnection object produced by [DBI::dbConnect()]
#' @param name A table name for a table present in the `src`
#' @param reactive_faketable A [shiny::reactive] object that holds an underlying
#'   [faketables::faketable()]
#'
#' @returns `dbWriteTable` does not return, but does reassign the `faketable`
#'   reactive object in the parent environment
#' @export
dbWriteTable <- function(src, name, reactive_faketable) {
  if (!inherits(src, 'DBIConnection'))
    cli::cli_abort('{.fun faketables::dbWriteTable} requires a valid {.fun DBI::dbConnect} object')
  if (!shiny::is.reactive(reactive_faketable))
    cli::cli_abort('{.fun faketables::faketablesInsert} requires a {.fun shiny::reactive} object and a data.frame')

  faketable <- do.call(reactive_faketable, args = list())

  join_by <- if (faketable@.rowId == '.rowId') colnames(faketable@data) else faketable@.rowId

  src |>
    dplyr::tbl(name) |>
    dplyr::rows_delete(
      y = faketable@deleted,
      by = join_by,
      copy = TRUE,
      in_place = TRUE,
      unmatched = 'ignore'
    ) |>
    dplyr::rows_update(
      y = faketable@updated,
      by = join_by,
      copy = TRUE,
      in_place = TRUE,
      unmatched = 'ignore'
    ) |>
    dplyr::rows_insert(
      y = faketable@inserted,
      by = join_by,
      copy = TRUE,
      in_place = TRUE,
      conflict = 'ignore'
    )

  env <- rlang::env_parent(rlang::caller_env(), n = 2)
  rowId <- if (faketable@.rowId %in% colnames(faketable@data)) faketable@.rowId else NULL
  reactive_faketable |>
    substitute() |>
    deparse() |>
    assign(
      value = faketablesServer(
        faketable = {
          src |>
            dplyr::tbl(name) |>
            faketable(faketable@.table_def, rowId, faketable@.show_delete) |>
            S7::`prop<-`('.iteration', value = faketable@.iteration + 1L)
        }
      ),
      envir = env
    )
}
