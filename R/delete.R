#' Create and monitor a delete button
#' @name delete_button
#' @rdname delete_button
#'
#' @description These functions are used internally in `faketables`
#'
#'
#' @details The delete button itself does not actually delete a row from the
#'   data, this only creates a button that reports which row a user wishes to
#'   delete.
#'
#'   There are three components required to use the buttons. The first is
#'   placing `.create_delete_listener()` somewhere in the shiny app's ui. The
#'   second is calling `.delete_button()` to create the button and then
#'   inserting it using [shiny::insertUI()]. The third is creating a shiny event
#'   listener that acts on the button presses by monitoring `input$table_btns`.
#'
#' @examples
#' \dontrun{
#' # this example creates a button for every row of `mtcars`
#' # when the user presses a button, it will print the inputId of the button pressed
#' shiny::shinyApp(
#'   ui = shiny::tagList(
#'     .create_delete_listener(), # add button listener
#'     shiny::div(id = 'delete-buttons') # create div for buttons to be inserted
#'   ),
#'   server = \(input, output, session) {
#'     mtcars |>
#'       tibble::rownames_to_column() |>
#'       dplyr::pull(.data$rowname) |>
#'       purrr::map_chr(digest::digest) |> # create row ids
#'       purrr::walk(\(x) {
#'         shiny::insertUI( # insert delete button into ui
#'           selector = '#delete-buttons',
#'           where = 'afterBegin',
#'           ui = shiny::fluidRow(.delete_button(x, list(width = 2)))
#'         )
#'       })
#'
#'     shiny::observe({
#'      print(glue::glue('Button {input$table_btns} was pressed'))
#'     }) |>
#'      shiny::bindEvent(input$table_btns)
#'   }
#' )
#' }
#'
NULL

#' @rdname delete_button
#'
#' @param rowId The `.rowId` value that corresponds to the row where the button
#'   will be displayed
#' @param ... Further arguments to pass to [shiny::column()]. This can include
#'   `width`.
#'
#' @returns A [shiny::column()] containing a [shiny::actionButton()] that allows
#'   users to send a delete request.
#' @keywords internal
.delete_button <- function(rowId, ...) {
  dots <- rlang::list2(...) |> unlist(recursive = FALSE) |> as.list()
  if (is.null(dots$width)) width <- 2 else { width <- dots$width; dots$width <- NULL; }
  shiny::column(
    width = width,
    shiny::actionButton(
      inputId = glue::glue('{rowId}_delete'),
      class = 'btn-remove',
      style = 'background: #bf5959; color: #fff; border: none; width: 100%',
      label = "Delete"
    ),
    dots
  )
}

#' @rdname delete_button
#' @author Joe Marlo
#'
#' @param ns The output of a call to [shiny::NS()]
#'
#' @returns An HTML `<head>` tag with the button listener
#' @keywords internal
.create_delete_listener <- function(ns = shiny::NS(NULL)){
  shiny::tags$head(
    shiny::tags$script(
      shiny::HTML(
        glue::glue(
          .open = "<<",
          .close = ">>",
          "
          $(document).on('click', '.btn-remove', function(evt) {
            Shiny.setInputValue('<<ns('table_btns')>>', evt.target.id, {priority: 'event'});
          });
          "
        )
      )
    )
  )
}
