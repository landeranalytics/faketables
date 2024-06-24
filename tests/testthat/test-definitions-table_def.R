source('../test-setup.R')
test_that('table_def requires at least one col_def', {
  table_def() |> expect_error()

  table_def(tibble::tibble()) |>
    expect_error()

  table_def(valid_col_def, tibble::tibble()) |>
    expect_error()

  table_def(valid_col_def, valid_col_def2) |>
    expect_no_error()

  table_def(valid_col_def) |>
    expect_no_error()

  table_def(list(valid_col_def)) |>
    expect_no_error()

  table_def(list(valid_col_def, valid_col_def2)) |>
    expect_no_error()
})

test_that('table_def requires unique col_def objects', {
  table_def(valid_col_def, valid_col_def) |>
    expect_error()

  table_def(list(valid_col_def, valid_col_def)) |>
    expect_error()
})

test_that('table_def returns a tibble with the proper structure', {
  expect_s3_class(
    table_def(valid_col_def),
    c(class(tibble::tibble()), 'table_def')
  )

  expect_equal(
    structure(
      valid_col_def,
      class = c(class(tibble::tibble()), 'table_def')
    ),
    table_def(valid_col_def)
  )
})
