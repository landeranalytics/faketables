insert <- S7::new_generic('insert', 'f_tab')
S7::method(insert, faketable) <- function(f_tab, data) {
  f_tab@x <- dplyr::rows_insert(
    x = f_tab@x,
    y = data,
    by = 'rowId'
  )
  return(f_tab)
}

update <- S7::new_generic('update', 'f_tab')
S7::method(update, faketable) <- function(f_tab, data) {
  f_tab@x <- dplyr::rows_update(
    x = f_tab@x,
    y = data,
    by = 'rowId'
  )
  return(f_tab)
}

delete <- S7::new_generic('delete', c('f_tab', 'x'))
S7::method(delete, list(faketable, S7::class_character)) <- function(f_tab, x) {
  f_tab@x <- dplyr::filter(f_tab@x, !(.data$rowId %in% .env$x))
  return(f_tab)
}

S7::method(delete, list(faketable, S7::class_data.frame)) <- function(f_tab, x) {
  f_tab@x <- dplyr::anti_join(f_tab@x, x, by = f_tab@.rowId)
  return(f_tab)
}
