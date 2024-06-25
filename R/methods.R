#' `insert`, `update`, or `delete` rows from a `faketables` object
#' @name methods
#' @rdname methods
#'
#' @param faketable A [faketables::faketable()] object
#' @param data
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
#' @returns A [faketables::faketable()] object
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # insert
#' # to insert new rows of data from a data.frame called `ins`
#' faketable <- faketablesServer(faketable = insert(faketable(), ins))
#'
#' # update
#' # to update with a new column called 'new' that has the value 'new'
#' faketable <- update(faketable, dplyr::mutate(faketable@data, 'new' = 'new'))
#'
#' # delete
#' # to delete the first six rows of the data where the primary key column is `rowId`
#' rows_to_delete <- utils::head(faketable@data)
#' faketable <- delete(faketable, rows_to_delete$.rowId)
#' # OR
#' faketable <- delete(faketable, rows_to_delete)
#' }
NULL

insert <- S7::new_generic('insert', c('faketable', 'data'), \(faketable, data) {
  S7::S7_dispatch()
})
#' @name insert
#' @rdname methods
#'
#' @usage insert(faketable, data)
#'
#' @export
S7::method(insert, list(faketable, S7::class_missing)) <- function(faketable, data) {
  cli::cli_abort('{.fun faketables::insert} requires a second argument')
}

#' @export
S7::method(insert, list(faketable, S7::class_data.frame)) <- function(faketable, data) {
  data <- .create_rowid(data, faketable@.rowId)
  faketable@data <- dplyr::rows_insert(
    x = faketable@data,
    y = data,
    by = '.rowId'
  )
  return(faketable)
}

update <- S7::new_generic('update', c('faketable', 'data'), \(faketable, data) {
  S7::S7_dispatch()
})
#' @name update
#' @rdname methods
#'
#' @usage update(faketable, data)
#'
#' @export
S7::method(update, list(faketable, S7::class_missing)) <- function(faketable, data) {
  cli::cli_abort('{.fun faketables::update} requires a second argument')
}

#' @export
S7::method(update, list(faketable, S7::class_data.frame)) <- function(faketable, data) {
  faketable@data <- dplyr::rows_update(
    x = faketable@data,
    y = data,
    by = '.rowId',
    unmatched = 'ignore'
  )
  return(faketable)
}

delete <- S7::new_generic('delete', c('faketable', 'data'), \(faketable, data) {
  S7::S7_dispatch()
})
#' @name delete
#' @rdname methods
#'
#' @usage delete(faketable, data)
#'
#' @export
S7::method(delete, list(faketable, S7::class_missing)) <- function(faketable, data) {
  cli::cli_abort('{.fun faketables::delete} requires a second argument')
}

#' @export
S7::method(delete, list(faketable, S7::class_character)) <- function(faketable, data) {
  data <- dplyr::filter(faketable@data, .data$.rowId %in% .env$data)
  faketable <- delete(faketable, data)
  return(faketable)
}

#' @export
S7::method(delete, list(faketable, S7::class_data.frame)) <- function(faketable, data) {
  faketable@.deleted <- dplyr::bind_rows(faketable@.deleted, data)
  faketable@data <- dplyr::anti_join(faketable@data, data, by = '.rowId')
  return(faketable)
}
