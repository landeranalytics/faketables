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
  shiny::tags$div(
    shiny::fluidRow(cols)
  )
}

.create_table_body <- function(f_tab) {
  f_tab@x |>
    dplyr::select(tidyselect::all_of(c(f_tab@.rowId, f_tab@.table_def$name))) |>
    purrr::pmap(\(...) {
      dots <- rlang::list2(...)
      cols <-
        dots |>
        names() |>
        tail(-1) |> # drop rowId column
        purrr::map(\(nm) {
          c_def <- which(f_tab@.table_def$name == nm)
          args <- list(
            inputId = glue::glue('table_{dots[[f_tab@.rowId]]}_{nm}'),
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
            list(.delete_button(dots[[f_tab@.rowId]], rlang::inject(f_tab@.show_delete))),
            after = 0
          )
      }
      return(cols)
    }) |>
    purrr::map(shiny::fluidRow) |>
    shiny::tags$div()
}

render_table <- S7::new_generic('render_table', 'f_tab')
S7::method(render_table, faketable) <- function(f_tab) {
  shiny::removeUI(
    selector = glue::glue("#table .table-row"),
    multiple = TRUE
  )
  table_header <- .create_table_header(f_tab)
  table_body <- .create_table_body(f_tab)
  shiny::insertUI(
    selector = '#table',
    where = 'afterBegin',
    ui = shiny::tags$div(
      table_header,
      shiny::tags$hr(style = 'margin-top: 0;'),
      table_body
    )
  )
}
