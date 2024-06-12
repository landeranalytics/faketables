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
