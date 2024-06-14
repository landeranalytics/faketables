#' `insert`, `update`, or `delete` rows from a `faketables` object
#' @name methods
#' @rdname methods
#'
#' @param f_tab A [faketables::faketable()] object
#' @param x
#'  * `insert`: A data.frame to add to the data in the [faketables::faketable()]
#'   object. If it does not already have a primary key column as specified in
#'   [faketables::table_def()], one will be created and primary keys will be
#'   generated.
#'  * `update`: A data.frame with a primary key column as specified in
#'   [faketables::table_def()] with primary key values already present in the
#'   data.
#'  * `delete`: Either a character vector of values from the primary key column
#'   as specified in [faketables::table_def()] or a data.frame of rows to remove
#'   with that vector as a column.
#'
#' @details Unlike [faketables::update()] and [faketables::delete()] which are
#' generally only used internally [faketables::insert()] must be implemented by
#' end users. To do so, the return from [faketables::insert()] must be passed
#' back into a [faketables::faketablesServer()] call and then reassigned to the
#' users chosen variable as done with the initial
#' [faketables::faketablesServer()] call.
#'
#' @returns A [faketables::faketable()] object
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # insert
#' # to insert new rows of data from a data.frame called `ins`
#' f_tab <- faketablesServer(faketable = insert(f_tab(), ins))
#'
#' # update
#' # to update with a new column called 'new' that has the value 'new'
#' faketable <- update(faketable, dplyr::mutate(faketable@x, 'new' = 'new'))
#'
#' # delete
#' # to delete the first six rows of the data where the primary key column is `rowId`
#' rows_to_delete <- utils::head(faketable@x)
#' faketable <- delete(faketable, rows_to_delete$.rowId)
#' # OR
#' faketable <- delete(faketable, rows_to_delete)
#' }
NULL

insert <- S7::new_generic('insert', c('f_tab', 'x'), \(f_tab, x) {
  S7::S7_dispatch()
})
#' @name insert
#' @rdname methods
#'
#' @usage insert(f_tab, x)
#'
#' @export
S7::method(insert, list(faketable, S7::class_missing)) <- function(f_tab, x) {
  cli::cli_abort('{.fun faketables::insert} requires a second argument')
}

#' @export
S7::method(insert, list(faketable, S7::class_data.frame)) <- function(f_tab, x) {
  x <- .create_rowid(x, f_tab@.rowId)
  f_tab@x <- dplyr::rows_insert(
    x = f_tab@x,
    y = x,
    by = '.rowId'
  )
  return(f_tab)
}


update <- S7::new_generic('update', c('f_tab', 'x'), \(f_tab, x) {
  S7::S7_dispatch()
})
#' @name update
#' @rdname methods
#'
#' @usage update(f_tab, x)
#'
#' @export
S7::method(update, list(faketable, S7::class_missing)) <- function(f_tab, x) {
  cli::cli_abort('{.fun faketables::update} requires a second argument')
}

#' @export
S7::method(update, list(faketable, S7::class_data.frame)) <- function(f_tab, x) {
  f_tab@x <- dplyr::rows_upsert(
    x = f_tab@x,
    y = x,
    by = '.rowId'
  )
  return(f_tab)
}

delete <- S7::new_generic('delete', c('f_tab', 'x'), \(f_tab, x) {
  S7::S7_dispatch()
})
#' @name delete
#' @rdname methods
#'
#' @usage delete(f_tab, x)
#'
#' @export
S7::method(delete, list(faketable, S7::class_missing)) <- function(f_tab, x) {
  cli::cli_abort('{.fun faketables::delete} requires a second argument')
}

#' @export
S7::method(delete, list(faketable, S7::class_vector)) <- function(f_tab, x) {
  x <- dplyr::filter(f_tab@x, .data$.rowId %in% .env$x)
  f_tab <- delete(f_tab, x)
  return(f_tab)
}

#' @export
S7::method(delete, list(faketable, S7::class_data.frame)) <- function(f_tab, x) {
  f_tab@.deleted <- dplyr::bind_rows(f_tab@.deleted, x)
  f_tab@x <- dplyr::anti_join(f_tab@x, x, by = '.rowId')
  return(f_tab)
}
