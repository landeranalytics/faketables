source('../test-setup.R')
test_that('inputs are correctly reconstructed from tables', {
  input <-
    valid_faketable@x$name |>
    `names<-`(glue::glue('table_{valid_faketable@x$.rowId}_name')) |>
    as.list() |>
    rlang::splice() |>
    shiny::reactiveValues()

  shiny::isolate({
    expect_equal(
      head(valid_faketable@x, 0),
     .reconstruct_inputs(valid_faketable, shiny::reactiveValues())
    )

    expect_equal(
      tibble::tibble(
        '.rowId' = valid_faketable@x$.rowId,
        'name' = valid_faketable@x$name
      ),
      .reconstruct_inputs(valid_faketable, input)
    )
  })
})
