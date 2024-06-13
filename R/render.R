#' Create and render a `faketables` table header
#' @rdname render_header
#'
#' @param f_tab A [faketables::faketable()] object
#'
#' @returns `NULL`
#' @keywords internal
.render_header <- function(f_tab) {
  shiny::removeUI(
    selector = glue::glue("#table-header .table-header"),
    multiple = TRUE
  )

  shiny::insertUI(
    selector = '#table-header',
    where = 'afterBegin',
    ui = shiny::tags$div(
      .create_table_header(f_tab),
      shiny::tags$hr(style = 'margin-top: 0;'),
      class = 'table-header'
    )
  )
}

#' @rdname render_header
#'
#' @returns A [shiny::fluidRow()] containing the display names from `f_tab`'s
#'   [faketables::table_def()] each rendered in [shiny::column()]
#' @keywords internal
.create_table_header <- function(f_tab) {
  cols <-
    f_tab@.table_def |>
    purrr::pmap(\(...) {
      dots <- rlang::list2(...)
      shiny::column(
        width = dots$width,
        shiny::tags$b(dots$display_name)
      )
    })
  if (!is.null(f_tab@.show_delete)) {
    if (is.null(f_tab@.show_delete$width)) width <- 2 else width <- f_tab@.show_delete$width
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
#' @param f_tab A [faketables::faketable()] object
#' @param ns The session namespace from `shiny::NS()`or `session$ns`
#'
#' @keywords internal
.render_table <- function(f_tab, ns) {
  .create_table_body(f_tab, ns)
}

#' @rdname render_table
#'
#' @returns A [shiny::fluidRow()] containing a [shiny::column()] for each column
#'   specified in `f_tab`'s [faketables::table_def()]
#' @keywords internal
.create_table_body <- function(f_tab, ns) {
  f_tab@x |>
    dplyr::select(tidyselect::all_of(c('.rowId', f_tab@.table_def$name))) |>
    purrr::pmap(\(...) {
      dots <- rlang::list2(...)
      cols <-
        dots |>
        names() |>
        utils::tail(-1) |> # drop rowId column
        purrr::map(\(nm) {
          c_def <- which(f_tab@.table_def$name == nm)
          args <- list(
            inputId = ns(glue::glue('table_{dots[[".rowId"]]}_{nm}')),
            dots[[nm]] # TODO: find way to name this to ensure right values are used
          )
          shiny::column(
            width = f_tab@.table_def$width[c_def],
            {
              f_tab@.table_def$input_call[[c_def]] |>
                rlang::call2(!!!args) |>
                rlang::eval_tidy()
            }
          )
        })
      if (!is.null(f_tab@.show_delete)) {
        cols <-
          cols |>
          append(
            list(.delete_button(dots[[".rowId"]], !!!f_tab@.show_delete)),
            after = 0
          )
      }
      return(cols)
    }) |>
    purrr::map(shiny::fluidRow, class = 'table-row')
}
