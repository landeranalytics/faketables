dbWriteTable <- function(conn, name, reactive_faketable) {
  if (!inherits(conn, 'DBIConnection'))
    cli::cli_abort('{.fun faketables::dbWriteTable} requires a valid {.fun DBI::dbConnect} object')
  if (!shiny::is.reactive(reactive_faketable))
    cli::cli_abort('{.fun faketables::faketablesInsert} requires a {.fun shiny::reactive} object and a data.frame')

  faketable <- do.call(reactive_faketable, args = list())

  join_by <- if (faketable@.rowId == '.rowId') colnames(faketable@data) else faketable@.rowId

  conn |>
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
          conn |>
            dplyr::tbl(name) |>
            faketable(faketable@.table_def, rowId, faketable@.show_delete) |>
            S7::`prop<-`('.iteration', value = faketable@.iteration + 1L)
        }
      ),
      envir = env
    )
}
