#' Create and render a `faketables` table header
#' @rdname render_header
#'
#' @param faketable A [faketables::faketable()] object
#'
#' @returns `NULL`
#' @keywords internal
.render_header <- function(faketable) {
  shiny::removeUI(
    selector = glue::glue("#table-header .table-header"),
    multiple = TRUE
  )

  shiny::insertUI(
    selector = '#table-header',
    where = 'afterBegin',
    ui = shiny::tags$div(
      .create_table_header(faketable),
      shiny::tags$hr(style = 'margin-top: 0;'),
      class = 'table-header'
    )
  )
}

#' @rdname render_header
#'
#' @returns A [shiny::fluidRow()] containing the display names from `faketable`'s
#'   [faketables::table_def()] each rendered in [shiny::column()]
#' @keywords internal
.create_table_header <- function(faketable) {
  cols <-
    faketable@.table_def |>
    purrr::pmap(\(...) {
      dots <- rlang::list2(...)
      shiny::column(
        width = dots$width,
        shiny::tags$b(dots$display_name)
      )
    })
  if (!is.null(faketable@.show_delete)) {
    if (is.null(faketable@.show_delete$width)) width <- 2 else width <- faketable@.show_delete$width
    cols <-
      cols |>
      append(
        list(shiny::column(
          width = width,
          shiny::tags$b('')
        )),
        after = 0
      )
  }
  shiny::fluidRow(cols, class = 'table-header-row')
}

#' Create and render a `faketables` table body
#' @rdname render_table
#'
#' @description This output should be passed to [shiny::renderUI()] in the
#'   server
#'
#' @param faketable A [faketables::faketable()] object
#' @param ns The session namespace from `shiny::NS()`or `session$ns`
#'
#' @keywords internal
.render_table <- function(faketable, ns) {
  .create_table_body(faketable, ns)
}

#' @rdname render_table
#'
#' @returns A [shiny::fluidRow()] containing a [shiny::column()] for each column
#'   specified in `faketable`'s [faketables::table_def()]
#' @keywords internal
.create_table_body <- function(faketable, ns) {
  faketable@data |>
    dplyr::select(tidyselect::all_of(c('.rowId', faketable@.table_def$name))) |>
    purrr::pmap(\(...) {
      dots <- rlang::list2(...)
      cols <-
        dots |>
        names() |>
        utils::tail(-1) |> # drop rowId column
        purrr::map(\(nm) {
          c_def <- which(faketable@.table_def$name == nm)
          args <- list(
            inputId = ns(glue::glue('table_{dots[[".rowId"]]}_{nm}')),
            dots[[nm]] # TODO: find way to name this to ensure right values are used
          )
          shiny::column(
            width = faketable@.table_def$width[c_def],
            {
              faketable@.table_def$input_call[[c_def]] |>
                rlang::call2(!!!args) |>
                rlang::eval_tidy()
            }
          )
        })
      if (!is.null(faketable@.show_delete)) {
        cols <-
          cols |>
          append(
            list(.delete_button(dots[[".rowId"]], !!!faketable@.show_delete)),
            after = 0
          )
      }
      return(cols)
    }) |>
    purrr::map(shiny::fluidRow, class = 'table-row')
}
