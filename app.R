ui <- shiny::fluidPage(
  faketablesUI(),
  shiny::tags$h3('Add Row'),
  shiny::fluidRow(
    shiny::column(
      width = 2,
      shiny::actionButton('add_row', 'Add Row')
    ),
    shiny::column(
      width = 3,
      shiny::textInput('mpg', 'MPG', value = 50)
    ),
    shiny::column(
      width = 3,
      shiny::selectInput('cyl', 'CYL', c(4, 6, 8))
    ),
    shiny::column(
      width = 3,
      shiny::sliderInput('qsec', 'QSEC', min = 10, max = 25, value = c(10, 25))
    )
  ),
  shiny::tags$h3('Table'),
  shiny::tableOutput('table'),
  shiny::tags$h3('Inserted'),
  shiny::tableOutput('inserted'),
  shiny::tags$h3('Updated'),
  shiny::tableOutput('updated'),
  shiny::tags$h3('Deleted'),
  shiny::tableOutput('deleted'),
  # shiny::tags$h3('Misc.'),
  # shiny::uiOutput('misc')
)

server <- function(input, output, session) {
  f_tab <- faketablesServer(faketable = f_tab)
  output$table <- shiny::renderTable(.list_col_to_chr(f_tab()@x))
  output$inserted <- shiny::renderTable(.list_col_to_chr(f_tab()@inserted))
  output$updated <- shiny::renderTable(.list_col_to_chr(f_tab()@updated))
  output$deleted <- shiny::renderTable(.list_col_to_chr(f_tab()@deleted))

  shiny::observe({
    ins <- tibble::tibble(
      'mpg' = as.numeric(input$mpg),
      'cyl' = as.numeric(input$cyl),
      'qsec' = list(as.integer(input$qsec))
    ) |>
      .create_rowid()
    f_tab <- faketablesServer(faketable = insert(f_tab(), ins))
  }) |>
    shiny::bindEvent(input$add_row, ignoreInit = TRUE)
}

c_def <- list(
  col_def(
    name = 'mpg',
    input = input_call(
      fun = \(...) { shinyjs::disabled(shiny::textInput(...)) },
      args = list(label = NULL, placeholder = 'mpg')
    ),
    cast = as.numeric,
    width = 3,
    display_name = 'MPG'
  ),
  col_def(
    name = 'cyl',
    input = input_call(
      fun = shiny::selectInput,
      args = list(label = NULL, choices = c(4, 6, 8))
    ),
    cast = as.integer,
    width = 3,
    display_name = 'CYL'
  ),
  col_def(
    name = 'qsec',
    input = input_call(
      fun = shiny::sliderInput,
      args = list(label = 'Quarter Mile', min = 10, max = 25)
    ),
    cast = \(x) purrr::map(x, as.numeric),
    width = 3,
    display_name = 'QSEC'
  )
)

t_def <- table_def(c_def)

f_tab <-
  mtcars |>
  tibble::as_tibble() |>
  head(3) |>
  dplyr::mutate('qsec' = purrr::map(.data$qsec, \(x) round(c(x - 1, x + 1)))) |>
  dplyr::select('mpg', 'cyl', 'qsec') |>
  faketable(t_def, show_delete = list())

shiny::shinyApp(ui, server)
