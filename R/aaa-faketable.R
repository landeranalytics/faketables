faketable <- S7::new_class(
  name = 'faketable',
  package = 'faketables',
  properties = list( # .prop values are read-only
    'x' = S7::class_data.frame,
    '.raw_data' = S7::class_data.frame,
    '.inserted' = S7::new_property(
      class = S7::class_data.frame,
      getter = \(self) {
        dplyr::anti_join(self@x, self@.raw_data, by = self@.rowId)
      }
    ),
    '.updated' = S7::new_property(
      class = S7::class_data.frame,
      getter = \(self) {
        dplyr::anti_join(self@x, self@.raw_data, by = colnames(self@.raw_data))
      }
    ),
    '.deleted' = S7::new_property( # just the rows that have been removed
      class = S7::class_data.frame,
      getter = \(self) {
        dplyr::anti_join(self@.raw_data, self@x, by = self@.rowId)
      }
    ),
    '.table_def' = S7::new_S3_class('table_def'),
    '.rowId' = S7::new_property(
      class = S7::class_character,
      # getter = \(self) { self@.rowId },
      # default = 'rowId'
    ),
    '.show_delete' = S7::new_property(
      class = S7::class_list,
      # getter = \(self) { self@.show_delete },
      # default = list()
    )
  ),
  constructor = \(x, table_def, rowId = 'rowId', show_delete = list()) {
    x <- tibble::as_tibble(x)
    if (is.null(x[[rowId]])) {
      x <-
        x |>
        dplyr::mutate('.rowId' = dplyr::row_number()) |>
        dplyr::mutate(
          {{rowId}} := purrr::map_chr(.data, digest::digest),
          .before = 0,
          .by = '.rowId'
        ) |>
        dplyr::select(-'.rowId')
    }

    S7::new_object(
      S7::S7_object(),
      'x' = x,
      '.raw_data' = x,
      # '.updated' = head(x, 0),
      # '.deleted' = head(x, 0),
      '.table_def' = table_def,
      '.rowId' = rowId,
      '.show_delete' = show_delete
    )
  }
)
