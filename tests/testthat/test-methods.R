source('../test-setup.R')
test_that('insert works', {
  insert_data <- tibble::tibble('name' = 'new name', 'name2' = 'new name 2')

  insert(valid_faketable) |>
    expect_error()

  insert(valid_faketable, matrix(1)) |>
    expect_error()

  insert(valid_faketable, insert_data) |>
    expect_no_error()

  f_tab <- insert(valid_faketable, insert_data)

  expect_equal(
    f_tab@inserted,
    insert_data
  )

  expect_equal(
    {
      f_tab@data |>
        dplyr::semi_join(
          y = insert_data,
          by = c('name', 'name2')
        )
    },
    insert_data
  )
})

test_that('update works', {
  update_data <- tibble::tibble(
    '.rowId' = valid_faketable@.data$.rowId,
    'name' = 'new name',
    'name2' = 'new name 2'
  )

  update(valid_faketable) |>
    expect_error()

  update(valid_faketable, matrix(1)) |>
    expect_error()

  update(valid_faketable, update_data) |>
    expect_no_error()

  f_tab <- update(valid_faketable, update_data)

  expect_equal(
    f_tab@updated,
    dplyr::select(update_data, -'.rowId')
  )

  expect_equal(
    dplyr::semi_join(f_tab@.data, update_data, by = c('.rowId', 'name', 'name2')),
    update_data
  )
})

test_that('delete works', {
  delete_row <- valid_faketable@.data$.rowId
  delete_data <- valid_faketable@.data[1,]

  delete(valid_faketable) |>
    expect_error()

  delete(valid_faketable, matrix(1)) |>
    expect_error()

  delete(valid_faketable, delete_row) |>
    expect_no_error()

  delete(valid_faketable, delete_data) |>
    expect_no_error()

  f_tab_row <- delete(valid_faketable, delete_row)

  expect_equal(
    f_tab_row@deleted,
    dplyr::select(delete_data, -'.rowId')
  )

  expect_equal(
    head(valid_faketable@data, 0),
    f_tab_row@data
  )

  f_tab_data <- delete(valid_faketable, delete_data)

  expect_equal(
    f_tab_data@deleted,
    dplyr::select(delete_data, -'.rowId')
  )

  expect_equal(
    head(valid_faketable@data, 0),
    f_tab_data@data
  )
})

