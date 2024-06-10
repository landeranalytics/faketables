.better_rbind <- function(...) {
  dots <- rlang::list2(...)
  if (length(dots) == 1) dots[[1]] else dplyr::bind_rows(dots)
}

faketable <- S7::new_class(
  name = 'faketable',
  package = 'faketable',
  properties = list(
    'raw_data' = S7::class_data.frame,
    'data' = S7::class_data.frame,
    '.rowId' = S7::new_property(
      class = S7::class_character,
      # getter = \(self) { self@.rowId },
      # default = 'rowId'
    ),
    '.show_delete' = S7::new_property(
      class = S7::class_logical,
      # getter = \(self) { self@.show_delete },
      # default = TRUE
    ),
    '.insert_selector' = S7::new_property(
      class = S7::class_character,
      # getter = \(self) { self@.insert_selector },
      # default = '#table'
    ),
    '.updated_data' = S7::new_property( # just the rows that have been updated
      class = S7::class_data.frame,
      setter = function(self, value) {
        self@.updated_data <- .better_rbind(self@.updated_data, value); self;
      }
    ),
    '.removed_data' = S7::new_property( # just the rows that have been removed
      class = S7::class_data.frame,
      setter = function(self, value) {
        self@.removed_data <- .better_rbind(self@.removed_data, value); self;
      }
    )
  ),
  constructor = \(data, rowId = 'rowId') {
    data <- tibble::as_tibble(data)
    S7::new_object(
      S7::S7_object(),
      'raw_data' = data,
      'data' = data,
      '.rowId' = rowId,
      '.updated_data' = head(data, 0),
      '.removed_data' = head(data, 0)
    )
  }
)

faketable(mtcars)
