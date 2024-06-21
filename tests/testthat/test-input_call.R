source('../test-setup.R')
test_that("input is constructed properly", {
  expect_equal(
    shiny::selectInput(
      inputId = 'id',
      label = NULL,
      choices = c(2, 4, 6),
      selected = 2
    ),
    input_call(
      shiny::selectInput,
      args = list(label = NULL, choices = c(2,4,6))
    )('inputId' = 'id', 'selected' = 2)
  )

  expect_equal(
    shiny::sliderInput(
      inputId = 'id',
      label = NULL,
      min = 0,
      max = 10,
      value = 5
    ),
    input_call(
      shiny::sliderInput,
      args = list(label = NULL, min = 0, max = 10)
    )('inputId' = 'id', 'value' = 5)
  )
})

test_that('user supplied `inputId` is overwritten', {
  expect_equal(
    input_call(
      shiny::selectInput,
      args = list(label = NULL, choices = c(2,4,6), inputId = 'FAKE_ID')
    )('inputId' = 'id', 'selected' = 2),
    input_call(
      shiny::selectInput,
      args = list(label = NULL, choices = c(2,4,6))
    )('inputId' = 'id', 'selected' = 2)
  )
})

test_that('arg name not required for value', {
  expect_equal(
    shiny::selectInput(
      inputId = 'id',
      label = NULL,
      choices = c(2, 4, 6),
      selected = 2
    ),
    input_call(
      shiny::selectInput,
      args = list(label = NULL, choices = c(2,4,6))
    )('inputId' = 'id', 2)
  )

  expect_equal(
    shiny::textInput(
      inputId = 'id',
      label = NULL,
      value = 2
    ),
    input_call(
      shiny::textInput,
      args = list(label = NULL)
    )('inputId' = 'id', 2)
  )
})

test_that("'fun' and 'args' are the correct class/type", {
  expect_error(
    input_call(
      isTRUE(TRUE),
      args = list()
    )
  )

  expect_error(
    input_call(
      shiny::textInput,
      args = c(label = 'label')
    )
  )
})

test_that('args must have an `inputId` or be only `...`', {
  input_call(
    fun = shiny::textInput,
    args = list(label = 'label')
  ) |>
    expect_no_error()

  input_call(
    fun = \(...) { shiny::textInput(...) },
    args = list(label = 'label')
  ) |>
    expect_no_error()

  input_call(
    fun = \(inputId, ...) { shiny::textInput(inputId, ...) },
    args = list(label = 'label')
  ) |>
    expect_no_error()

  input_call(
    fun = \(bad, ...) { shiny::textInput(bad, ...) },
    args = list(label = 'label')
  ) |>
    expect_error()
})
