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

render_header <- S7::new_generic('render_header', 'f_tab')
S7::method(render_header, faketable) <- function(f_tab) {
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

.create_table_body <- function(f_tab, ns) {
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
            inputId = ns(glue::glue('table_{dots[[f_tab@.rowId]]}_{nm}')),
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
            list(.delete_button(dots[[f_tab@.rowId]], !!!f_tab@.show_delete)),
            after = 0
          )
      }
      return(cols)
    }) |>
    purrr::map(shiny::fluidRow, class = 'table-row')
}

render_table <- S7::new_generic('render_table', 'f_tab')
S7::method(render_table, faketable) <- function(f_tab, ns) {
  shiny::removeUI(
    selector = glue::glue("#table .table-body"),
    multiple = TRUE
  )

  shiny::insertUI(
    selector = '#table',
    where = 'afterBegin',
    ui = shiny::tags$div(
      .create_table_body(f_tab, ns),
      class = 'table-body'
    )
  )
}
