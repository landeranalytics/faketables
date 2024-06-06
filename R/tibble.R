dt <-
  mtcars |>
  tibble::as_tibble() |>
  dplyr::mutate('rowId' = purrr::map_chr(dplyr::row_number(), digest::digest))

.better_rbind <- function(...) {
  dots <- rlang::list2(...)
  if (length(dots) == 1) dots[[1]] else dplyr::bind_rows(dots)
}

.better_list_flatten <- function(l, .f) {
  depth <- purrr::pluck_depth(l)
  is_f <- purrr::map_lgl(l, .f)
  if (!all(is_f)) .better_list_flatten(purrr::list_flatten(l), .f) else l
}

col_def <- function(name, fun, cast, width, display_name = name, ...) {
  tibble::tibble(
    'name' = name,
    'fun' = list(fun),
    'cast' = list(cast),
    'width' = width,
    'display_name' = display_name,
    ...
  )
}

table_def <- function(...) {
  .better_rbind(...)
}

faketable <- function(data, defs, rowId, show_delete = NULL, insert_selector = '#table') {
  shiny::removeUI(
    selector = glue::glue("{insert_selector} .table-row"),
    multiple = TRUE
  )
  data |>
    dplyr::select(tidyselect::all_of(c(defs$name, rowId))) |>
    purrr::pmap(\(...) {
      dots <- rlang::list2(...)
      cols <-
        dots |>
        names() |>
        head(-1) |>
        purrr::map(\(nm) {
          col_def <- which(defs$name == nm)
          shiny::column(
            width = defs$width[col_def],
            defs$fun[[col_def]](dots[[rowId]], dots[[nm]])
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

