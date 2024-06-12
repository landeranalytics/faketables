insert <- S7::new_generic('insert', c('f_tab', 'x'))
S7::method(insert, list(faketable, S7::class_data.frame)) <- function(f_tab, x) {
  x <- .create_rowid(x, f_tab@.rowId)
  f_tab@x <- dplyr::rows_insert(
    x = f_tab@x,
    y = x,
    by = 'rowId'
  )
  return(f_tab)
}

update <- S7::new_generic('update', c('f_tab', 'x'))
S7::method(update, list(faketable, S7::class_data.frame)) <- function(f_tab, x) {
  f_tab@x <- dplyr::rows_upsert(
    x = f_tab@x,
    y = x,
    by = 'rowId'
  )
  return(f_tab)
}

delete <- S7::new_generic('delete', c('f_tab', 'x'))
S7::method(delete, list(faketable, S7::class_character)) <- function(f_tab, x) {
  x <- dplyr::filter(f_tab@x, .data$rowId %in% .env$x)
  f_tab <- delete(f_tab, x)
  return(f_tab)
}

S7::method(delete, list(faketable, S7::class_data.frame)) <- function(f_tab, x) {
  f_tab@.deleted <- dplyr::bind_rows(f_tab@.deleted, x)
  f_tab@x <- dplyr::anti_join(f_tab@x, x, by = f_tab@.rowId)
  return(f_tab)
}
