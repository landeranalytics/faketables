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
#' @details
#'  * `update`: This method cannot add new columns or change the class of
#'    existing columns because it is based on [dplyr::rows_update()].
#'
#' @keywords internal
#' @examples
#' faketable <- faketable(
#'   mtcars,
#'   table_def(
#'     col_def(
#'       name = 'mpg',
#'       input = input_call(
#'         fun = shiny::textInput,
#'         args = list(label = NULL, placeholder = 'mpg')
#'       ),
#'       cast = as.numeric,
#'       width = 3,
#'       display_name = 'MPG'
#'     )
#'   )
#' )
#'
#' # insert
#' # to insert a copy of the first row of `mtcars`
#' faketable <- insert(faketable, utils::head(mtcars, 1))
#'
#' # update
#' # to update 'mpg' to only whole numbers
#' faketable <- update(faketable, dplyr::mutate(faketable@.data, 'mpg' = round(mpg)))
#'
#' # delete
#' # to delete the first six rows of the data where the primary key column is `rowId`
#' rows_to_delete <- utils::head(faketable@.data)
#' faketable <- delete(faketable, rows_to_delete$.rowId)
#' # OR
#' faketable <- delete(faketable, rows_to_delete)
#'
#' faketable@data
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
S7::method(insert, list(faketable, S7::new_union(S7::class_data.frame, S7::new_S3_class('tbl')))) <- function(faketable, data) {
  data <- .create_rowid(data, faketable@.rowId)
  faketable@.data <- dplyr::rows_insert(
    x = faketable@.data,
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
S7::method(update, list(faketable, S7::new_union(S7::class_data.frame, S7::new_S3_class('tbl')))) <- function(faketable, data) {
  faketable@.data <- dplyr::rows_update(
    x = faketable@.data,
    y = data,
    by = '.rowId'
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
  data <- dplyr::filter(faketable@.data, .data$.rowId %in% .env$data)
  faketable <- delete(faketable, data)
  return(faketable)
}

#' @export
S7::method(delete, list(faketable, S7::new_union(S7::class_data.frame, S7::new_S3_class('tbl')))) <- function(faketable, data) {
  faketable@.deleted <- dplyr::bind_rows(faketable@.deleted, data)
  faketable@.data <- dplyr::anti_join(faketable@.data, data, by = '.rowId')
  return(faketable)
}
