#' Create the `shiny` components required to use `faketables`
#' @name shiny
#' @rdname shiny
#'
#' @param id An ID string used to identify the module UI
#' @param faketable A [faketables::faketable()] object
#'
#' @returns
#'  * `faketablesUI`: A [shiny::shinyApp()] ui
#'  * `faketablesServer`: A [shiny::reactive()] object that represents the
#'    passed [faketables::faketable()] object
#'
#' @keywords internal
NULL

#' @name faketablesUI
#' @rdname shiny
#'
#' @export
faketablesUI <- function(id = 'faketables') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    .create_delete_listener(ns),
    shiny::div(
      id = 'table-container',
      shiny::div(id = 'table-header'),
      shiny::uiOutput(ns('table'))
    )
  )
}

#' @name faketablesServer
#' @rdname shiny
#'
#' @export
faketablesServer <- function(id = 'faketables', faketable) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    deleted_rowId <- shiny::reactiveVal(character())

    shiny::observe({
      rowId <- regexec('[a-f0-9]{32}', input$table_btns)[[1]]
      rowId <-
        substr(input$table_btns, rowId, rowId + attr(rowId, 'match.length') - 1)
      deleted_rowId() |>
        append(rowId) |>
        deleted_rowId()

      # rowId <- stringr::str_extract(input$table_btns, "[a-f0-9]{32}")
      # faketable <- .delete(faketable, rowId)
      # render_table(faketable)
    }) |>
      shiny::bindEvent(input$table_btns)

    f_tab <- shiny::reactive({
      # insert

      # update
      updated_data <- .reconstruct_inputs(faketable, input)
      if (nrow(updated_data) > 0) faketable <- update(faketable, updated_data)

      # delete
      faketable <- delete(faketable, deleted_rowId())

      # return
      return(faketable)
    })

    .render_header(faketable)
    output$table <- shiny::renderUI({
      .render_table(f_tab(), ns)
    })
    return(f_tab)
  })
}
