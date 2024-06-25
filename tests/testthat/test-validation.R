source('../test-setup.R')
test_that('is_input_call returns TRUE only for a valid input_call', {
  is_input_call(valid_input_call) |>
    expect_true()

  input_call(
    shiny::sliderInput,
    args = list(label = NULL, min = 0, max = 10)
  ) |>
    is_input_call() |>
    expect_true()

  is_input_call(tibble::tibble()) |>
    expect_false()

  is_input_call(shiny::textInput) |>
    expect_false()

  expect_equal(
    names(formals(valid_input_call)),
    '...'
  )
})

test_that('is_col_def returns TRUE only for a valid col_def', {
  is_col_def(valid_col_def) |>
    expect_no_error()

  structure(
    tibble::tibble(
      'name' = 'name',
      'input_call' = list(valid_input_call),
      'cast' = list(as.character),
      'width' = 1,
      'display_name' = 'display_name'
    ),
    class = c(class(tibble::tibble()))
  ) |>
    is_col_def() |>
    expect_false()

  structure(
    tibble::tibble(
      'input_call' = list(valid_input_call),
      'cast' = list(as.character),
      'width' = 1,
      'display_name' = 'display_name'
    ),
    class = c(class(tibble::tibble()), 'col_def')
  ) |>
    is_col_def() |>
    expect_false()

  structure(
    tibble::tibble(
      'name' = 'name',
      'input_call' = list(valid_input_call),
      'cast' = list(as.character),
      'width' = 1,
      'display_name' = 'display_name'
    ),
    class = c(class(tibble::tibble()), 'col_def')
  )[c(1, 1),] |>
    is_col_def() |>
    expect_false()

  structure(
    tibble::tibble(
      'name' = 1,
      'input_call' = list(valid_input_call),
      'cast' = list(as.character),
      'width' = 1,
      'display_name' = 'display_name'
    ),
    class = c(class(tibble::tibble()), 'col_def')
  ) |>
    is_col_def() |>
    expect_false()

  structure(
    tibble::tibble(
      'name' = 'name',
      'input_call' = list('input_call'),
      'cast' = list(as.character),
      'width' = 1,
      'display_name' = 'display_name'
    ),
    class = c(class(tibble::tibble()), 'col_def')
  ) |>
    is_col_def() |>
    expect_false()

  structure(
    tibble::tibble(
      'name' = 'name',
      'input_call' = list(valid_input_call),
      'cast' = list('cast'),
      'width' = 1,
      'display_name' = 'display_name'
    ),
    class = c(class(tibble::tibble()), 'col_def')
  ) |>
    is_col_def() |>
    expect_false()

  structure(
    tibble::tibble(
      'name' = 'name',
      'input_call' = list(valid_input_call),
      'cast' = list(as.character),
      'width' = 1.1,
      'display_name' = 'display_name'
    ),
    class = c(class(tibble::tibble()), 'col_def')
  ) |>
    is_col_def() |>
    expect_false()

  structure(
    tibble::tibble(
      'name' = 'name',
      'input_call' = list(valid_input_call),
      'cast' = list(as.character),
      'width' = 1,
      'display_name' = 2
    ),
    class = c(class(tibble::tibble()), 'col_def')
  )[c(1, 1),] |>
    is_col_def() |>
    expect_false()

  tibble::tibble() |>
    is_col_def() |>
    expect_false()
})

test_that('is_table_def returns TRUE for a valid table_def', {
  is_table_def(valid_table_def) |>
    expect_true()

  (class(valid_table_def) <- class(tibble::tibble())) |>
    is_table_def() |>
    expect_false()

  valid_table_def |>
    dplyr::mutate('name' = 'name') |>
    is_table_def() |>
    expect_false()
})

test_that('is_faketable returns TRUE for a valid faketable', {
  # S7 classes use a validator function
  is_faketable(valid_faketable) |>
    expect_true()

  expect_error(valid_faketable@data <- matrix(1))

  expect_error(valid_faketable@.table_def <- matrix(1))
  expect_error(valid_faketable@.table_def <- mtcars)

  expect_error(
    valid_faketable@.table_def <- table_def(col_def(
      name = 'col_doesnt_exist',
      input_call = input_call(shiny::textInput, list(label = NULL)),
      cast = as.character,
      width = 3
    )))

  expect_error(valid_faketable@.rowId <- 'col_doesnt_exist')

  expect_error(valid_faketable@.show_delete <- NULL)
  expect_error(valid_faketable@.show_delete <- c())
})
