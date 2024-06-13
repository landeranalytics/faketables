.better_rbind <- function(...) {
  dots <- rlang::list2(...)
  if (length(dots) == 1) dots[[1]] else dplyr::bind_rows(dots)
}

.better_list_flatten <- function(l, .f) {
  is_f <- purrr::map_lgl(l, .f)
  if (!all(is_f)) .better_list_flatten(purrr::list_flatten(l), .f) else l
}

.create_rowid <- function(x, rowId) {
  if (is.null(x[[rowId]])) {
    x <-
      x |>
      dplyr::mutate('.rowId' = dplyr::row_number()) |>
      dplyr::mutate(
        {{rowId}} := purrr::map_chr(.data, digest::digest),
        .before = 0,
        .by = '.rowId'
      ) |>
      dplyr::select(-'.rowId') |>
      dplyr::select('rowId', tidyselect::everything())
  }
  return(x)
}

.reconstruct_inputs <- function(faketable, input) {
  all_vals <- shiny::reactiveValuesToList(input)
  all_vals <- all_vals[grepl("table_[a-f0-9]{32}_", names(all_vals))]
  if (length(all_vals) > 0) {
    updated_data <-
      all_vals |>
      tibble::as_tibble() |>
      tidyr::pivot_longer(
        cols = tidyselect::everything(),
        names_pattern = 'table_([a-f0-9]{32})_(.*)$',
        names_to = c('rowId', 'col'),
        values_transform = as.character
      ) |>
      tidyr::pivot_wider(
        id_cols = 'rowId',
        names_from = 'col',
        values_from = 'value'
      ) |>
      dplyr::anti_join(
        y = faketable@.deleted,
        by = 'rowId'
      ) |>
      purrr::imap(\(x, idx) {
        col <- which(idx == faketable@.table_def$name)
        if (length(col) != 0) faketable@.table_def$cast[col][[1]](x) else x
      }) |>
      dplyr::bind_cols() |>
      dplyr::select(tidyselect::all_of(colnames(faketable@x)))
  } else {
    utils::head(faketable@x, 0)
  }
}
