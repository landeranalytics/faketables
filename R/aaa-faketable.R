#' Create a `faketable` object
#'
#' @param x A data.frame
#' @param table_def A [faketables::table_def()] object
#' @param rowId A character vector of length one identifying which column is a
#'   primary key, if any.
#' @param show_delete A named list passed to the [shiny::column()] that holds
#'   the Delete button. If `NULL`, the delete column will not be shown.
#'
#' @returns A `faketable` object with the following properties:
#'  * `x`: The current state of the table of inputs
#'  * `inserted`: Rows from `x` that were inserted into the data (were not
#'   present in `.raw_data`)
#'  * `updated`: Rows from `x` that have been modified, but were present in
#'   `.raw_data`
#'  * `deleted`: Rows that were in `.raw_data`, but have been removed from and
#'   do not appear in `x`
#'  * `.raw_data`: The data originally pased to [faketables::faketable()] with
#'   the addition of a `.rowId` column as the first column. This column is
#'   calculated using by hashing either the provided `rowId` column or using the
#'   row number and system time.
#'  * `.deleted`: All rows that have been removed from `x`, including those that
#'   were inserted then deleted
#'  * `.table_def`: A copy of the user supplied [faketables::table_def()] passed
#'   as the argument `table_def`
#'  * `.show_delete`: A copy of the user supplied list passed as the argument
#'   `show_delete`
#'
#' @details A `faketable` object is an [S7::S7_object()] with the class
#'   `faketable`. S7 object properties are access using an `@`, rather than the
#'   traditional `$`. For example, the property `x` for a `faketable` object
#'   called `f_tab` can be accessed using `f_tab@x`.
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
    'x' = S7::class_data.frame,
    'inserted' = S7::new_property(
      class = S7::class_data.frame,
      getter = \(self) {
        dplyr::anti_join(self@x, self@.raw_data, by = '.rowId')
      }
    ),
    'updated' = S7::new_property(
      class = S7::class_data.frame,
      getter = \(self) {
        self@x |>
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
    '.deleted' = S7::class_data.frame,
    '.table_def' = S7::new_S3_class('table_def'),
    '.show_delete' = S7::new_property(
      class = S7::class_list,
      # getter = \(self) { self@.show_delete },
      # default = list()
    )
  ),
  constructor = \(x, table_def, rowId = NULL, show_delete = list()) {
    x <-
      x |>
      tibble::as_tibble() |>
      .create_rowid(rowId)

    S7::new_object(
      S7::S7_object(),
      'x' = x,
      '.raw_data' = x,
      '.deleted' = utils::head(x, 0),
      '.table_def' = table_def,
      '.show_delete' = show_delete
    )
  }
)
