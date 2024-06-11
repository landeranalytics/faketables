input_call <- S7::new_class(
  name = 'input_call',
  package = 'faketables',
  properties = list(
    'x' = S7::class_function
  ),
  constructor = \(fun, args, ...) {
    S7::new_object(
      S7::S7_object(),
      'x' = \(...) {
        rlang::call2(fun, !!!args) |>
          rlang::call_modify(...) |>
          rlang::eval_tidy()
      }
    )
  }
)

col_def <- S7::new_class(
  name = 'col_def',
  package = 'faketables',
  properties = list(
    'x' = S7::new_property(
      class = S7::class_data.frame,
      getter = \(self) {
        tibble::tibble(
          'name' = self@name,
          'input_call' = list(self@input_call),
          'cast' = list(self@cast),
          'width' = self@width,
          'display_name' = self@display_name,
          !!!self@dots
        )
      }
    ),
    'name' = S7::class_character,
    'input_call' = input_call,
    'cast' = S7::class_function,
    'width' = S7::class_integer,
    'display_name' = S7::class_character,
    'dots' = S7::class_list
  ),
  constructor = \(name, input, cast, width, display_name = name, ...) {
    width <- as.integer(width)
    S7::new_object(
      S7::S7_object(),
      'name' = name,
      'input_call' = input,
      'cast' = cast,
      'width' = width,
      'display_name' = display_name,
      'dots' = rlang::list2(...)
    )
  }
)

table_def <- S7::new_class(
  name = 'table_def',
  package = 'faketables',
  properties = list(
    'x' = S7::new_property(
      class = S7::class_data.frame
    )
  ),
  constructor = \(...) {
    col_defs <-
      rlang::list2(...) |>
      .better_list_flatten(\(x) 'faketables::col_def' %in% class(x)) |>
      purrr::map(\(x) x@x) |>
      purrr::list_rbind()

    S7::new_object(
      S7::S7_object(),
      'x' = col_defs
    )
  }
)

faketable <- S7::new_class(
  name = 'faketable',
  package = 'faketables',
  properties = list( # .prop values are read-only
    'x' = S7::class_data.frame,
    '.raw_data' = S7::class_data.frame,
    '.updated_data' = S7::new_property(
      class = S7::class_data.frame,
      getter = \(self) {
        dplyr::anti_join(self@x, self@.raw_data, by = colnames(self@.raw_data))
      }
    ),
    '.removed_data' = S7::new_property( # just the rows that have been removed
      class = S7::class_data.frame,
      getter = \(self) {
        dplyr::anti_join(self@.raw_data, self@x, by = self@.rowId)
      }
    ),
    '.table_def' = table_def,
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
        dplyr::mutate(
          {{rowId}} := purrr::map_chr(dplyr::row_number(), digest::digest),
          .before = 0
        )
    }

    S7::new_object(
      S7::S7_object(),
      'x' = x,
      '.raw_data' = x,
      '.table_def' = table_def,
      '.rowId' = rowId,
      '.show_delete' = show_delete
    )
  }
)
