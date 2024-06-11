input_call <- function(fun, args, ...) {
  structure(
    {
      function(...) {
        rlang::call2(fun, !!!args) |>
          rlang::call_modify(...) |>
          rlang::eval_tidy()
      }
    },
    class = c('function', 'input_call')
  )
}

col_def <- function(name, input, cast, width, display_name = name, ...) {
  structure(
    tibble::tibble(
      'name' = name,
      'input_call' = list(input),
      'cast' = list(cast),
      'width' = width,
      'display_name' = display_name,
      ...
    ),
    class = c(class(tibble::tibble()), 'col_def')
  )
}

table_def <- function(...) {
  structure(
    {
      c_def <-
        rlang::list2(...) |>
        .better_list_flatten(tibble::is_tibble)
      do.call(rbind, c_def)
    },
    class = c(class(tibble::tibble()), 'table_def')
  )
}
