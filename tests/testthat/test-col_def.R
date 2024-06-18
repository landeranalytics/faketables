test_that("col_def returns `tibble` with `col_def` class", {
  expect_s3_class(
    col_def('name', 'input', 'cast', 'width', 'display_name'),
    c(class(tibble::tibble()), 'col_def')
  )
})

test_that("col_def has the required columns", {
  expect_in(
    c('name', 'input_call', 'cast', 'width', 'display_name'),
    col_def('name', 'input', 'cast', 'width', 'display_name') |>
      colnames()
  )

  expect_in(
    c('name', 'input_call', 'cast', 'width', 'display_name'),
    col_def('name', 'input', 'cast', 'width', 'display_name', 'extra_col' = 'test') |>
      colnames()
  )

  expect_in(
    c('name', 'input_call', 'cast', 'width', 'display_name', 'extra_col'),
    col_def('name', 'input', 'cast', 'width', 'display_name') |>
      colnames()
  ) |>
    expect_error()
})
