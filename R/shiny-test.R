source('R/tibble.R')
source('R/delete.R')
ui_mod <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    create_button_listener(ns),
    shiny::tags$h1('App'),
    shiny::tableOutput(outputId = 'deletedRows'),
    shiny::tags$hr(),
    shiny::actionButton(inputId = ns('add_row'), 'Add Row'),
    shiny::actionButton(inputId = ns('update_data'), 'Update Data'),
    shiny::tags$hr(),
    htmltools::div(id = 'table')
  )
}

ui <- shiny::fluidPage(
  ui_mod('draw')
)

server_mod <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    full_data <- shiny::reactiveVal(dt)
    visible_data <- shiny::reactiveVal(head(dt, 0))
    removed_data <- shiny::reactiveVal(head(dt, 0))
    counter <- shiny::reactiveVal(0)

    funs <- list(
      'mpg' = \(rowId, value) {
        shinyjs::disabled(
          shiny::textInput(
            inputId = ns(glue::glue("table_{rowId}_mpg")),
            label = NULL,
            placeholder = 'Row key',
            value = value
          )
        )
      },
      'cyl' = \(rowId, value) {
        shiny::selectInput(
          inputId = ns(glue::glue("table_{rowId}_cyl")),
          label = NULL,
          choices = c(2,4,6),
          selected = value
        )
      },
      'vs' = \(rowId, value) {
        shiny::checkboxInput(
          inputId = ns(glue::glue("table_{rowId}_vs")),
          label = 'vs',
          value = as.logical(value)
        )
      }
    )

    col_class <- list('mpg' = as.numeric, 'cyl' = as.integer, 'vs' = as.logical)

    defs <- col_def(ns, funs, col_class, width = c(3, 3, 3))
    call_faketable <- function(.data) {
      faketable(.data, defs, rowId = 'rowId', show_delete = list(width = 3))
    }

    shiny::observe({
      counter(counter() + 1)
      full_data()[1:counter(),] |>
        dplyr::anti_join(removed_data(), by = 'rowId') |>
        dplyr::anti_join(visible_data(), by = 'rowId') |>
        dplyr::bind_rows(visible_data()) |>
        visible_data()
      call_faketable(visible_data())
    }) |>
      shiny::bindEvent(input$add_row)

    shiny::observe({
      print('Removed Rows')
      rowId <- stringr::str_extract(input$table_btns, "[a-f0-9]{32}")
      removed_data() |>
        dplyr::bind_rows(dplyr::filter(full_data(), .data$rowId == .env$rowId)) |>
        print() |>
        removed_data()

      visible_data() |>
        dplyr::filter(.data$rowId != .env$rowId) |>
        visible_data()

      call_faketable(visible_data())
    }) |>
      shiny::bindEvent(input$table_btns)

    shiny::observe({
      print('Updated Rows')
      all_vals <- shiny::reactiveValuesToList(input)
      updated_data <-
        all_vals[grepl("^table_[a-f0-9]{32}_", names(all_vals))] |>
        tibble::as_tibble() |>
        tidyr::pivot_longer(
          cols = tidyselect::everything(),
          names_pattern = 'table_([a-z0-9]{32})_(.*)$',
          names_to = c('rowId', 'col'),
          values_transform = as.character
        ) |>
        tidyr::pivot_wider(
          id_cols = 'rowId',
          names_from = 'col',
          values_from = 'value'
        ) |>
        dplyr::anti_join(
          y = removed_data(),
          by = 'rowId'
        ) |>
        purrr::imap(\(x, idx) {
          col <- which(idx == defs$col_names)
          if (length(col) != 0) {
            defs$col_class[col][[1]](x)
          } else {
            x
          }
        }) |>
        dplyr::bind_cols()
      visible_data() |>
        dplyr::rows_upsert(
          y = updated_data,
          by = 'rowId'
        ) |>
        print() |>
        visible_data()

      call_faketable(visible_data())
    }) |>
      shiny::bindEvent(input$update_data)
  })
}

server <- function(input, output, session) {
  server_mod('draw')
}

shiny::shinyApp(ui, server)

# updated_data <-
#   all_vals |>
#   tibble::as_tibble() |>
#   tidyr::pivot_longer(
#     cols = tidyselect::everything(),
#     names_pattern = 'table_([a-z0-9]{32})_(.*)$',
#     names_to = c('row', 'col'),
#     values_transform = as.character
#   ) |>
#   tidyr::pivot_wider(
#     id_cols = 'row',
#     names_from = 'col',
#     values_from = 'value'
#   ) |>
#   purrr::imap(\(x, idx) {
#     col <- which(idx == defs$col_names)
#     if (length(col) != 0) {
#       defs$col_class[col][[1]](x)
#     } else {
#       x
#     }
#   }) |>
#   dplyr::bind_cols()
