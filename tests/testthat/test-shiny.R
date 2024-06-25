source('../test-setup.R')
test_that("the ui is a div with two child divs", {
  faketablesUI() |>
    expect_no_error()

  faketablesUI('id') |>
    expect_no_error()

  faketablesUI() |>
    expect_s3_class(class(shiny::tagList()))

  ui <- "<div id=\"table-container\">\n  <div id=\"table-header\"></div>\n  <div id=\"faketables-table\" class=\"shiny-html-output\"></div>\n</div>"

  expect_equal(
    ui,
    faketablesUI() |> as.character()
  )

  ui <- "<div id=\"table-container\">\n  <div id=\"table-header\"></div>\n  <div id=\"id-table\" class=\"shiny-html-output\"></div>\n</div>"

  expect_equal(
    ui,
    faketablesUI('id') |> as.character()
  )
})

test_that('the server functions', {
  server <- function(input, output, session) {}

  shiny::testServer(server, {
    insert_data <- tibble::tibble(
      '.rowId' = digest::digest(TRUE),
      'name' = 'new name',
      'name2' = 'new name2'
    )

    expect_equal(
      faketablesServer(faketable = valid_faketable)(),
      valid_faketable
    )

    expect_equal(
      faketablesServer(
        faketable = valid_faketable,
        insert = insert_data
      )(),
      insert(valid_faketable, insert_data)
    )
  })
})

test_that('faketablesInsert functions and doesnt clutter the global env', {
  server <- function(input, output, session) {}

  shiny::testServer(server, {
    insert_data <- tibble::tibble(
      '.rowId' = digest::digest(TRUE),
      'name' = 'new name',
      'name2' = 'new name2'
    )

    f_tab <- faketablesServer(faketable = valid_faketable)
    expect_equal(
      faketablesInsert(f_tab, insert_data)(), # modifies original object
      insert(valid_faketable, insert_data)
    )

    # ensure f_tab isn't in the global env
    get('f_tab', envir = rlang::global_env()) |>
      expect_error()
  })
})
