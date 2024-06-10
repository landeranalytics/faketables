.better_rbind <- function(...) {
  dots <- rlang::list2(...)
  if (length(dots) == 1) dots[[1]] else dplyr::bind_rows(dots)
}

.better_list_flatten <- function(l, .f) {
  is_f <- purrr::map_lgl(l, .f)
  if (!all(is_f)) .better_list_flatten(purrr::list_flatten(l), .f) else l
}

.evaluate_fun <- function(fun, args, name, rowId, ns) {
  args$inputId <- ns(glue::glue('table_{rowId}_{name}'))
  fun(!!!args)
}

col_def <- S7::new_class(
  name = 'col_def',
  package = 'faketables',
  properties = list(
    'x' = S7::new_property(
      class = S7::class_data.frame,
      getter = \(self) {
        tibble::tibble(
          'name' = self@name,
          'fun' = list(self@fun),
          'args' = list(self@args),
          'value' = self@value,
          'cast' = list(self@cast),
          'width' = self@width,
          'display_name' = self@display_name,
          !!!self@dots
        )
      }
    ),
    'name' = S7::class_character,
    'fun' = S7::class_function,
    'args' = S7::class_list,
    'value' = S7::class_character,
    'cast' = S7::class_function,
    'width' = S7::class_integer,
    'display_name' = S7::class_character,
    'dots' = S7::class_list
  ),
  constructor = \(name, fun, args, value, cast, width, display_name = name, ...) {
    width <- as.integer(width)
    S7::new_object(
      S7::S7_object(),
      'name' = name,
      'fun' = fun,
      'args' = args,
      'value' = value,
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
