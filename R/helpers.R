#' @inherit dplyr::bind_rows title description params return
#'
#' @details Unlike [dplyr::bind_rows()], `.better_rbind()` does not complain if
#'   only one data frame is supplied.
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' .better_rbind(mtcars)
#' .better_rbind(mtcars, mtcars, .id = 'id')
#' }
#'
.better_rbind <- function(..., .id = NULL) {
  dots <- rlang::list2(...) |> purrr::list_flatten()
  if (length(dots) == 1) dots[[1]] else dplyr::bind_rows(dots, .id)
}

#' Add a `.rowId` column to a data frame
#'
#' @param x A data frame
#' @param rowId The name of the primary key column
#'
#' @returns `x` with a new column named `.rowId` at column position one that has
#'   an MD5 hash uniquely identifying that row
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' .create_rowid(mtcars)
#' mtcars |>
#'   tibble::rownames_to_column() |>
#'   .create_rowid(rowId = 'rowname')
#' }
#'
.create_rowid <- function(x, rowId = NULL) {
  if (is.null(rowId) || is.null(x[[rowId]])) {
    x <- dplyr::mutate(x, '.rowId' = dplyr::row_number() + Sys.time())
  } else {
    if (any(duplicated(x[[rowId]])))
      cli::cli_abort('{.fun faketables::.create_rowid} expects a provided `rowId` column to contain unique values')
    x <- dplyr::mutate(x, '.rowId' = .data[[rowId]])
  }
  x |>
    dplyr::mutate(
      '.rowId' = purrr::map_chr(.data$.rowId, digest::digest),
      .before = 0,
      .by = '.rowId'
    ) |>
    dplyr::select('.rowId', tidyselect::everything())
}

#' Reconstruct a table from shiny inputs
#'
#' @param faketable A [faketables::faketable()] object
#' @param input The shiny server `input`
#'
#' @details This is used internally to capture the changes made to the data in
#'   the UI by the user. It first captures all of `input`, then filters by input
#'   name to only inputs created by the table. These are then reconstructed into
#'   a format matching `faketable@x` with column class handling done by the user
#'   supplied `cast` function in [faketables::col_def()].
#'
#' @returns The state of `faketable@x` as the user sees it in the UI
#' @keywords internal
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
        names_to = c('.rowId', 'col'),
        values_transform = as.character
      ) |>
      # this handles duplicate rows if there is an input that takes vector of
      # length > 1 such as shiny::sliderInput
      unique() |>
      tidyr::pivot_wider(
        id_cols = '.rowId',
        names_from = 'col',
        values_from = 'value',
        values_fn = list # this ensures multi-value inputs are properly grouped
      ) |>
      dplyr::anti_join(
        y = faketable@.deleted,
        by = '.rowId'
      ) |>
      purrr::imap(\(x, idx) {
        col <- which(idx == faketable@.table_def$name)
        x <- if (length(col) != 0) faketable@.table_def$cast[col][[1]](x) else x
        tibble::tibble({{idx}} := x) # handle list-type cols
      }) |>
      dplyr::bind_cols() |>
      dplyr::select(tidyselect::any_of(colnames(faketable@x)))
  } else {
    utils::head(faketable@x, 0)
  }
}

#' Convert list-type columns to character
#'
#' @description This is handy for using [shiny::renderTable()] with list type
#'   columns
#'
#' @param x A data frame
#'
#' @returns `x`, except list type columns are now `character`
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble('ls' = list('a', 'b'), 'num' = 1:2)
#' .list_col_to_chr(df)
#' }
#'
.list_col_to_chr <- function(x) {
  list_cols <- names(x)[purrr::map_lgl(x, \(x) rlang::is_list(x) & rlang::is_atomic(x[[1]]))]

  for(n in list_cols) {
    x[[n]] <- purrr::map_chr(x[[n]], \(x) glue::glue("c('{paste0(x, collapse = \"','\")}')"))
  }

  return(x)
}
