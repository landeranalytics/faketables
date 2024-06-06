dt <-
  mtcars |>
  tibble::as_tibble() |>
  dplyr::mutate('rowId' = purrr::map_chr(dplyr::row_number(), digest::digest))

col_def <- function(ns, funs, col_class, width, col_names = names(funs), display_names = col_names, ...) {
  tibble::tibble(
    'col_names' = col_names,
    'col_class' = unname(col_class),
    'funs' = unname(funs),
    'width' = width,
    ...
  )
}

faketable <- function(data, defs, rowId, show_delete = NULL, insert_selector = '#table') {
  shiny::removeUI(
    selector = glue::glue("{insert_selector} .table-row"),
    multiple = TRUE
  )
  data |>
    dplyr::select(tidyselect::all_of(c(defs$col_names, rowId))) |>
    purrr::pmap(\(...) {
      dots <- rlang::list2(...)
      cols <-
        dots |>
        names() |>
        head(-1) |>
        purrr::map(\(nm) {
          col_def <- which(defs$col_names == nm)
          shiny::column(
            width = defs$width[col_def],
            defs$funs[[col_def]](dots[[rowId]], dots[[nm]])
          )
        })
      if (!is.null(show_delete)) {
        cols <-
          cols |>
          append(
            list(.delete_button(dots[[rowId]], rlang::inject(show_delete))),
            after = 0
          )
      }
      return(cols)
    }) |>
    purrr::walk(\(x) {
      shiny::insertUI(
        selector = insert_selector,
        where = 'afterBegin',
        ui = shiny::fluidRow(class = 'table-row', x)
      )
    })
}

