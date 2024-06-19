valid_input_call <-
  input_call(
    shiny::sliderInput,
    args = list(label = NULL, min = 0, max = 10)
  )
valid_col_def <-
  structure(
    tibble::tibble(
      'name' = 'name',
      'input_call' = list(valid_input_call),
      'cast' = list(as.character),
      'width' = 1,
      'display_name' = 'display_name'
    ),
    class = c(class(tibble::tibble()), 'col_def')
  )

test_that("col_def has the required columns", {
  expect_in(
    c('name', 'input_call', 'cast', 'width', 'display_name'),
    valid_col_def |>
      colnames()
  )

  expect_in(
    c('name', 'input_call', 'cast', 'width', 'display_name'),
    col_def('name', valid_input_call, as.character, 1, 'display_name', 'extra_col' = 'test') |>
      colnames()
  )

  expect_in(
    c('name', 'input_call', 'cast', 'width', 'display_name', 'extra_col'),
    colnames(valid_col_def)
  ) |>
    expect_error()
})

test_that('col_def enforces classes', {
  expect_no_error(
    valid_col_def
  )

  expect_error( # name
    col_def(1, valid_input_call, as.character, 1, 'display_name')
  )

  expect_error( # input
    col_def('name', 'valid_input_call', as.character, 1, 'display_name')
  )

  expect_error( # cast
    col_def('name', valid_input_call, 'as.character', 1, 'display_name')
  )

  expect_error( # width
    col_def('name', valid_input_call, as.character, 1.1, 'display_name')
  )

  expect_error( # display_name
    col_def('name', valid_input_call, as.character, 1, 1)
  )
})

test_that('col_def returns a tibble with the proper structure', {
  expect_equal(
    valid_col_def,
    col_def('name', valid_input_call, as.character, 1, 'display_name')
  )

  expect_true(
    is_col_def(
      valid_col_def
    )
  )

  expect_true(
    valid_col_def$input_call |>
      rlang::is_list()
  )

  expect_true(
    valid_col_def$cast |>
      rlang::is_list()
  )
})
