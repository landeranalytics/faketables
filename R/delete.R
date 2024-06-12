.delete_button <- function(rowId, ...) {
  dots <- rlang::list2(...) |> unlist(recursive = FALSE)
  if (is.null(dots$width)) width <- 2 else { width <- dots$width; dots$width <- NULL; }
  shiny::column(
    width = width,
    shiny::actionButton(
      inputId = glue::glue('{rowId}_delete'),
      class = 'btn-remove',
      style = 'background: #bf5959; color: #fff; border: none; width: 100%',
      label = "Delete"
    ),
    dots
  )
}

.create_delete_listener <- function(ns = shiny::NS(NULL)){
  shiny::tags$head(
    shiny::tags$script(
      shiny::HTML(
        glue::glue(
          .open = "<<",
          .close = ">>",
          "
          $(document).on('click', '.btn-remove', function(evt) {
            Shiny.setInputValue('<<ns('table_btns')>>', evt.target.id, {priority: 'event'});
          });
          "
        )
      )
    )
  )
}
