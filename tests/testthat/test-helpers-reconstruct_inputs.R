source('../test-setup.R')
test_that('inputs are correctly reconstructed from tables', {
  input <-
    valid_faketable@.data$name |>
    `names<-`(glue::glue('table_{valid_faketable@.data$.rowId}_name')) |>
    as.list() |>
    rlang::splice() |>
    shiny::reactiveValues()

  shiny::isolate({
    expect_equal(
      head(valid_faketable@.data, 0),
     .reconstruct_inputs(valid_faketable, shiny::reactiveValues())
    )

    expect_equal(
      tibble::tibble(
        '.rowId' = valid_faketable@.data$.rowId,
        'name' = valid_faketable@data$name
      ),
      .reconstruct_inputs(valid_faketable, input)
    )
  })
})
