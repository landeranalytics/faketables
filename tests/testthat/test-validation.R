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

})
