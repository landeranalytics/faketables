rowId <- 'rowId'
default_style <- 'background: #bf5959; color: #fff; border: none; width: 100%'
default_label <- 'Delete'

test_that('the delete button is created', {
  expect_equal(
    shiny::column(
      width = 3,
      shiny::actionButton(
        inputId = glue::glue('{rowId}_delete'),
        class = 'btn-remove',
        style = default_style,
        label = default_label
      )
    ) |> as.character(),
    .delete_button(rowId, width = 3) |> as.character()
  )

  expect_equal(
    shiny::column(
      width = 3,
      shiny::actionButton(
        inputId = glue::glue('{rowId}_delete'),
        class = 'btn-remove',
        style = default_style,
        label = default_label
      ),
      class = 'delete'
    ) |> as.character(),
    .delete_button(rowId, width = 3, class = 'delete') |> as.character()
  )

  expect_equal(
    shiny::column(
      width = 3,
      shiny::actionButton(
        inputId = glue::glue('{rowId}_delete'),
        class = 'btn-remove',
        style = default_style,
        label = default_label
      ),
      class = 'delete'
    ) |> as.character(),
    .delete_button(rowId, list(width = 3, class = 'delete')) |> as.character()
  )

  expect_equal(
    shiny::column(
      width = 3,
      shiny::actionButton(
        inputId = glue::glue('{rowId}_delete'),
        class = 'btn-remove',
        style = 'background: #bf5959; color: #000; border: none; width: 50%',
        label = default_label
      ),
      class = 'delete'
    ) |> as.character(),
    .delete_button(rowId, width = 3, class = 'delete', .delete_style = 'background: #bf5959; color: #000; border: none; width: 50%') |>
      as.character()
  )

  expect_equal(
    shiny::column(
      width = 3,
      shiny::actionButton(
        inputId = glue::glue('{rowId}_delete'),
        class = 'btn-remove',
        style = default_style,
        label = 'Remove'
      ),
      class = 'delete'
    ) |> as.character(),
    .delete_button(rowId, width = 3, class = 'delete', .delete_label = 'Remove') |>
      as.character()
  )
})

test_that('the delete button has a width of 2 when not specified', {
  expect_equal(
    shiny::column(
      width = 2,
      shiny::actionButton(
        inputId = glue::glue('{rowId}_delete'),
        class = 'btn-remove',
        style = default_style,
        label = default_label
      )
    ) |> as.character(),
    .delete_button(rowId) |> as.character()
  )
})

test_that('the delete button listener returns appropriately', {
  .create_delete_listener() |>
    expect_no_error()
})
