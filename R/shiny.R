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
faketablesServer <- function(id = 'faketables', faketable, insert = NULL) {
  if (!is.null(insert)) faketable <- insert(faketable, insert)
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
    }) |>
      shiny::bindEvent(input$table_btns)

    f_tab <- shiny::reactive({
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


#' Insert data into a `faketable`
#' @name insert
#' @rdname insert
#'
#' @param reactive_faketable A [shiny::reactive] object that holds an underlying
#'   [faketables::faketable()]
#' @param x A data.frame to add to the data in the [faketables::faketable()]
#'   object. If it does not already have a primary key column as specified in
#'   [faketables::table_def()], one will be created and primary keys will be
#'   generated.
#'
#' @returns `faketablesInsert` does not return, but does reassign the
#'   `faketable` reactive object in the parent environment
#' @export
faketablesInsert <- function(reactive_faketable, x) {
  if (!shiny::is.reactive(reactive_faketable) | !is.data.frame(x)) {
    cli::cli_abort('{.fun faketables::faketablesInsert} requires a {.fun shiny::reactive} object and a data.frame')
  }
  env <- rlang::env_parent(rlang::caller_env(), n = 2)
  reactive_faketable |>
    substitute() |>
    deparse() |>
    assign(
      value = faketablesServer(
        faketable = do.call(reactive_faketable, args = list()),
        insert = x
      ),
      envir = env
    )
}
