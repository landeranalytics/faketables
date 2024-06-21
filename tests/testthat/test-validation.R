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
