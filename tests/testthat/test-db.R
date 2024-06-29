source('../test-setup.R')
test_that("dbWriteTable works", {
  skip_if_not_installed('duckdb')
  skip_if_not_installed('dbplyr')
  skip_on_cran()
  skip_on_ci()
  server <- function(input, output, session) {}
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = ':memory:')
  duckdb::dbWriteTable(con, 'data', valid_faketable@data)

  shiny::testServer(server, {
    f_tab <- faketablesServer(faketable = valid_faketable)

    dbWriteTable('con', 'test', f_tab) |>
      expect_error()

    dbWriteTable(con, 'test', f_tab) |>
      expect_error()

    dbWriteTable(con, 'test', f_tab()) |>
      expect_error()

    dbWriteTable(con, 'data', f_tab) |>
      expect_no_error()
  })

  expect_equal(
    as.data.frame(valid_faketable@data),
    as.data.frame(dplyr::tbl(con, 'data'))
  )

  duckdb::dbDisconnect(con)
})
