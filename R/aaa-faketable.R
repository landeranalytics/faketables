#' Create a `faketable` object
#'
#' @param data A data.frame
#' @param table_def A [faketables::table_def()] object
#' @param rowId A character vector of length one identifying which column is a
#'   primary key, if any.
#' @param show_delete A named list passed to the [shiny::column()] that holds
#'   the Delete button. If `NULL`, the delete column will not be shown.
#'
#' @returns A `faketable` object with the following properties:
#'  * `data`: The current state of the table of inputs
#'  * `inserted`: Rows from `data` that were inserted into the data (were not
#'   present in `.raw_data`)
#'  * `updated`: Rows from `data` that have been modified, but were present in
#'   `.raw_data`
#'  * `deleted`: Rows that were in `.raw_data`, but have been removed from and
#'   do not appear in `data`
#'  * `.raw_data`: The data originally pased to [faketables::faketable()] with
#'   the addition of a `.rowId` column as the first column. This column is
#'   calculated using by hashing either the provided `rowId` column or using the
#'   row number and system time.
#'  * `.rowId`: The value of the `rowId` argument
#'  * `.deleted`: All rows that have been removed from `data`, including those that
#'   were inserted then deleted
#'  * `.table_def`: A copy of the user supplied [faketables::table_def()] passed
#'   as the argument `table_def`
#'  * `.show_delete`: A copy of the user supplied list passed as the argument
#'   `show_delete`
#'
#' @details A `faketable` object is an [S7::S7_object()] with the class
#'   `faketable`. S7 object properties are accessed using an `@`, rather than
#'   the traditional `$`. For example, the property `data` for a `faketable`
#'   object called `faketable` can be accessed using `faketable@data`.
#'
#' @seealso For more details on `S7`, see the vignette [on the
#'   website](https://rconsortium.github.io/S7/articles/S7.html) or by running:
#'   \code{vignette('S7', package = 'S7')}
#'
#' @export
faketable <- S7::new_class(
  name = 'faketable',
  package = 'faketables',
  properties = list( # .prop values are "private"
    'data' = S7::class_data.frame,
    'inserted' = S7::new_property(
      class = S7::class_data.frame,
      getter = \(self) {
        dplyr::anti_join(self@data, self@.raw_data, by = '.rowId')
      }
    ),
    'updated' = S7::new_property(
      class = S7::class_data.frame,
      getter = \(self) {
        self@data |>
          dplyr::anti_join(
            y = self@inserted,
            by = '.rowId'
          ) |>
          dplyr::anti_join(
            y = self@.raw_data,
            by = colnames(self@.raw_data)
          )
      }
    ),
    'deleted' = S7::new_property(
      class = S7::class_data.frame,
      getter = \(self) {
        dplyr::filter(self@.deleted, .data$.rowId %in% self@.raw_data$.rowId)
      }
    ),
    '.raw_data' = S7::class_data.frame,
    '.rowId' = S7::class_character,
    '.deleted' = S7::class_data.frame,
    '.table_def' = S7::new_S3_class('table_def'),
    '.show_delete' = S7::class_list
  ),
  constructor = \(data, table_def, rowId = NULL, show_delete = NULL) {
    if (!is.data.frame(data))
      cli::cli_abort('{.fun faketables::faketable} expects `data` to be a `data.frame`')
    if (!is_table_def(table_def))
      cli::cli_abort('{.fun faketables::faketable} expects `table_def` to be a valid {.fun faketables::table_def}')
    if (!all(table_def$name %in% colnames(data)))
      cli::cli_abort('{.fun faketables::faketable} expects all `table_def$name` to be column names in `data`')
    if (!is.null(rowId) && !(rowId %in% colnames(data)))
      cli::cli_abort('{.fun faketables::faketable} expects `rowId` to be `NULL` or a column name in `data`')
    if (!is.null(show_delete) && (!rlang::is_list(show_delete) || !rlang::is_named2(show_delete)))
      cli::cli_abort('{.fun faketables::faketable} expects `show_delete` to be `NULL` or a named list')

    if (is.null(show_delete)) show_delete <- list()
    data <-
      data |>
      tibble::as_tibble() |>
      .create_rowid(rowId)

    if (is.null(rowId)) rowId <- '.rowId'

    S7::new_object(
      S7::S7_object(),
      'data' = data,
      '.raw_data' = data,
      '.rowId' = rowId,
      '.deleted' = utils::head(data, 0),
      '.table_def' = table_def,
      '.show_delete' = show_delete
    )
  },
  validator = \(self) {
    if (!is.data.frame(self@data))
      cli::cli_abort('{.fun faketables::faketable} expects `self@data` to be a `data.frame`')
    if (!is_table_def(self@.table_def))
      cli::cli_abort('{.fun faketables::faketable} expects `self@.table_def` to be a valid {.fun faketables::table_def}')
    if (!all(self@.table_def$name %in% colnames(self@data)))
      cli::cli_abort('{.fun faketables::faketable} expects all `self@.table_def$name` to be column names in `self@data`')
    if (!(self@.rowId %in% colnames(self@data)))
      cli::cli_abort("{.fun faketables::faketable} expects '.rowId' to be  a column name in `self@data`")
    if ((!rlang::is_list(self@.show_delete) || !rlang::is_named2(self@.show_delete)))
      cli::cli_abort('{.fun faketables::faketable} expects `self@.show_delete` to be an empty or named list')
    if(!is_faketable(self))
      cli::cli_abort("This is awkward. You've managed to make an invalid `faketables` object despite our best efforts.")
  }
)
