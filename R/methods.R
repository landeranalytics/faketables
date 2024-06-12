.insert <- S7::new_generic('.insert', 'f_tab')
S7::method(.insert, faketable) <- function(f_tab, data) {
  f_tab@x <- dplyr::rows_insert(
    x = f_tab@x,
    y = data,
    by = 'rowId'
  )
  return(f_tab)
}

.update <- S7::new_generic('.update', 'f_tab')
S7::method(.update, faketable) <- function(f_tab, data) {
  f_tab@x <- dplyr::rows_update(
    x = f_tab@x,
    y = data,
    by = 'rowId'
  )
  return(f_tab)
}

.delete <- S7::new_generic('.delete', 'f_tab')
S7::method(.delete, faketable) <- function(f_tab, rowId) {
  f_tab@x <- dplyr::filter(f_tab@x, !(.data$rowId %in% .env$rowId))
  # f_tab@.deleted <- dplyr::bind_rows(
  #   dplyr::filter(f_tab@x, .data$rowId %in% .env$rowId),
  #   dplyr::bind_rows(f_tab@.deleted)
  # )
  return(f_tab)
}
