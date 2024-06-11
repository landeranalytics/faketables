source('R/s7.R')
source('R/delete.R')
source('R/render.R')
source('R/definitions.R')
source('R/helpers.R')

ui_mod <- function(id = 'faketables') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    create_button_listener(ns),
    htmltools::div(id = 'table-header'),
    htmltools::div(id = 'table')
  )
}

server_mod <- function(id = 'faketables', faketable) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    render_table(faketable)
  })
}

ui <- shiny::fluidPage( ui_mod() )

server <- function(input, output, session) {
  c_def <- list(
    col_def(
      name = 'mpg',
      input = input_call(
        fun = \(...) { shinyjs::disabled(shiny::textInput(...)) },
        args = list(label = NULL, placeholder = 'Row Key'),
        value = 'value'
      ),
      cast = as.numeric,
      width = 3,
      display_name = 'MPG'
    ),
    col_def(
      name = 'cyl',
      input = input_call(
        fun = shiny::selectInput,
        args = list(label = NULL, choices = c(2,4,6)),
        value = 'selected',
      ),
      cast = as.integer,
      width = 3,
      display_name = 'CYL'
    )
  )

  t_def <- table_def(c_def)

  f_tab <- faketable(head(mtcars), t_def)
  server_mod(faketable = f_tab)
}

shiny::shinyApp(ui, server)


# c_def <- col_def(
#   name = 'mpg',
#   fun = \(...) { shinyjs::disabled(shiny::textInput(...)) },
#   args = list(label = NULL, placeholder = 'Row Key'),
#   cast = as.numeric,
#   width = 3,
#   display_name = 'MPG'
# )
#
# t_def <- table_def(c_def)
#
# tmp <- faketable(mtcars, t_def)
# tmp@x <- dplyr::mutate(tmp@x, 'cyl' = ifelse(.data$cyl == 4, -4, .data$cyl))
# tmp@x <- head(tmp@x)
# tmp
#
# render_table(tmp)
