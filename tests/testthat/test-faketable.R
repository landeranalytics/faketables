source('../test-setup.R')
test_that('faketables works', {
  faketable(x, valid_table_def) |>
    expect_no_error()
})

test_that('faketables requires a data.frame', {
  faketable(matrix(), valid_table_def) |>
    expect_error()
})

test_that('supplied table_def is valid', {
  faketable(x, valid_col_def) |>
    expect_error()
})

test_that('table_def$name must all be present in x', {
  x |>
    dplyr::rename('name3' = 'name2') |>
    faketable(valid_table_def) |>
    expect_error()
})

test_that('rowId is null or a column in x', {
  faketable(x, valid_table_def, rowId = NULL) |>
    expect_no_error()

  faketable(x, valid_table_def, rowId = 'name') |>
    expect_no_error()

  faketable(x, valid_table_def, rowId = 'does_not_exist') |>
    expect_error()
})

test_that('show_delete is NULL or a named list', {
  faketable(x, valid_table_def, show_delete = NULL) |>
    expect_no_error()

  faketable(x, valid_table_def, show_delete = list()) |>
    expect_no_error()

  faketable(x, valid_table_def, show_delete = list('width' = 2)) |>
    expect_no_error()

  faketable(x, valid_table_def, show_delete = list(2)) |>
    expect_error()
})
