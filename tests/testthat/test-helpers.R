test_that('.list_col_to_chr converts a list column to a character vector', {
  dt <- tibble::tibble('fn' = list(sum, mean), 'vec' = list(c(1,2), c(3,4)))
  dt2 <- .list_col_to_chr(dt)

  rlang::is_list(dt2$fn) |>
    expect_true()

  rlang::is_character(dt2$vec) |>
    expect_true()

  expect_equal(
    dt2$vec,
    c("c('1','2')", "c('3','4')")
  )

  dt |>
    head(0) |>
    .list_col_to_chr() |>
    expect_no_error()
})
