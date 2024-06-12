faketable <- S7::new_class(
  name = 'faketable',
  package = 'faketables',
  properties = list( # .prop values are "private"
    'x' = S7::class_data.frame,
    '.raw_data' = S7::class_data.frame,
    'inserted' = S7::new_property(
      class = S7::class_data.frame,
      getter = \(self) {
        dplyr::anti_join(self@x, self@.raw_data, by = self@.rowId)
      }
    ),
    'updated' = S7::new_property(
      class = S7::class_data.frame,
      getter = \(self) {
        self@x |>
          dplyr::anti_join(
            y = self@inserted,
            by = 'rowId'
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
        dplyr::filter(self@.deleted, .data$rowId %in% self@.raw_data$rowId)
      }
    ),
    '.deleted' = S7::new_property( # just the rows that have been removed
      class = S7::class_data.frame
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
      '.rowId' = rowId,
      '.show_delete' = show_delete
    )
  }
)
