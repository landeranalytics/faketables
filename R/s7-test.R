# devtools::load_all()

ui_mod <- function(id = 'faketables') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    create_button_listener(ns),
    htmltools::div(id = 'table-container',
    htmltools::div(id = 'table-header'),
    shiny::uiOutput(ns('table'))
    )
  )
}

server_mod <- function(id = 'faketables', faketable) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    deleted_rowId <- shiny::reactiveVal(character())

    shiny::observe({
      deleted_rowId() |>
        append(stringr::str_extract(input$table_btns, "[a-f0-9]{32}")) |>
        deleted_rowId()

      # rowId <- stringr::str_extract(input$table_btns, "[a-f0-9]{32}")
      # faketable <- .delete(faketable, rowId)
      # render_table(faketable)
    }) |>
      shiny::bindEvent(input$table_btns)

    f_tab <- shiny::reactive({
      # insert

      # update
      all_vals <- shiny::reactiveValuesToList(input)
      all_vals <- all_vals[grepl("table_[a-f0-9]{32}_", names(all_vals))]
      if (length(all_vals) > 0) {
        updated_data <-
          all_vals |>
          tibble::as_tibble() |>
          tidyr::pivot_longer(
            cols = tidyselect::everything(),
            names_pattern = 'table_([a-f0-9]{32})_(.*)$',
            names_to = c('rowId', 'col'),
            values_transform = as.character
          ) |>
          tidyr::pivot_wider(
            id_cols = 'rowId',
            names_from = 'col',
            values_from = 'value'
          ) |>
          dplyr::anti_join(
            y = faketable@.deleted,
            by = 'rowId'
          ) |>
          purrr::imap(\(x, idx) {
            col <- which(idx == faketable@.table_def$name)
            if (length(col) != 0) faketable@.table_def$cast[col][[1]](x) else x
          }) |>
          dplyr::bind_cols()
        faketable <- .update(faketable, updated_data)
      }

      # delete
      faketable <- .delete(faketable, deleted_rowId())

      # return
      return(faketable)
    })

    render_header(faketable)
    output$table <- shiny::renderUI({
      render_table(f_tab(), ns)
    })
    return(f_tab)
  })
}

ui <- shiny::fluidPage(
  ui_mod(),
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
  f_tab <- server_mod(faketable = f_tab)
  output$table <- shiny::renderTable(f_tab()@x)
  output$inserted <- shiny::renderTable(f_tab()@.inserted)
  output$updated <- shiny::renderTable(f_tab()@.updated)
  output$deleted <- shiny::renderTable(f_tab()@.deleted)
  output$misc <- shiny::renderPrint(f_tab())
}

c_def <- list(
  col_def(
    name = 'mpg',
    input = input_call(
      fun = \(...) { shinyjs::disabled(shiny::textInput(...)) },
      args = list(label = NULL, placeholder = 'Row Key')
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
