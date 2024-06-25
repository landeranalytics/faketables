test_that('test that the rowId is created properly', {
  expect_equal(
    {
      mtcars |>
        tibble::rownames_to_column() |>
        .create_rowid(rowId = 'rowname') |>
        dplyr::pull(.data$.rowId)
    },
    {
      mtcars |>
        rownames() |>
        purrr::map_chr(digest::digest)
    }
  )
})

test_that('.create_rowid wants and creates unique values', {
  mtcars |>
    .create_rowid() |>
    dplyr::pull(.data$.rowId) |>
    duplicated() |>
    any() |>
    expect_false()

  mtcars |>
    .create_rowid(rowId = 'cyl') |>
    expect_error()

  tibble::tibble('a' = rep(1, 3)) |>
    .create_rowid() |>
    dplyr::pull(.data$.rowId) |>
    duplicated() |>
    any() |>
    expect_false()
})
