source('../test-setup.R')
server <- function(input, output, session) {}

test_that('create_table_header returns a shiny.tag object', {
  shiny::testServer(server, {
    ns <- session$ns

    .create_table_header(valid_faketable) |>
      class() |>
      expect_equal('shiny.tag')
  })
})

test_that('render_header doesnt error', {
  shiny::testServer(server, {
    ns <- session$ns
    .render_header(valid_faketable) |>
      expect_no_error()
  })
})

test_that('create_table_body returns a list of shiny.tag objects', {
  shiny::testServer(server, {
    ns <- session$ns

    .create_table_body(valid_faketable, ns) |>
      rlang::is_list() |>
      expect_true()

    .create_table_body(valid_faketable, ns) |>
      purrr::map(class) |>
      unlist() |>
      expect_equal('shiny.tag')
  })
})
