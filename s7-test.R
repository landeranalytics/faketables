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
  output$table <- shiny::renderTable(f_tab()@x)
  output$inserted <- shiny::renderTable(f_tab()@inserted)
  output$updated <- shiny::renderTable(f_tab()@updated)
  output$deleted <- shiny::renderTable(f_tab()@deleted)

  shiny::observe({
    ins <- tibble::tibble(
      'mpg' = as.numeric(input$mpg),
      'cyl' = as.numeric(input$cyl)
    ) |>
      dplyr::mutate('rowId' = digest::digest(.data), .before = 0)
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
  )
)

t_def <- table_def(c_def)

f_tab <-
  mtcars |>
  head() |>
  dplyr::select('mpg', 'cyl') |>
  faketable(t_def)

shiny::shinyApp(ui, server)
