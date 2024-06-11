.better_rbind <- function(...) {
  dots <- rlang::list2(...)
  if (length(dots) == 1) dots[[1]] else dplyr::bind_rows(dots)
}

.better_list_flatten <- function(l, .f) {
  is_f <- purrr::map_lgl(l, .f)
  if (!all(is_f)) .better_list_flatten(purrr::list_flatten(l), .f) else l
}
