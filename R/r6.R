faketable <- R6::R6Class(
  classname = 'faketable',
  public = list(
    'raw_data' = NULL,
    'data' = NULL,
    'updated_data' = NULL,
    'removed_data' = NULL,
    'table_def' = NULL,
    'show_delete' = NULL,
    'rowId' = 'rowId',
    'insert_selector' = '#table',
    'initialize' = \(raw_data, table_def, show_delete, rowId, insert_selector) {

    }
  ),
  private = list(
    '.redraw_ui' = \() {
      shiny::removeUI(
        selector = glue::glue("{insert_selector} .table-row"),
        multiple = TRUE
      )
      data |>
        dplyr::select(tidyselect::all_of(c(self$table_def$name, self$rowId))) |>
        purrr::pmap(\(...) {
          dots <- rlang::list2(...)
          cols <-
            dots |>
            names() |>
            head(-1) |>
            purrr::map(\(nm) {
              col_def <- which(self$table_def$name == nm)
              shiny::column(
                width = self$table_def$width[col_def],
                self$table_def$fun[[col_def]](dots[[self$rowId]], dots[[nm]])
              )
            })
          if (!is.null(self$show_delete)) {
            cols <-
              cols |>
              append(
                list(private$.delete_button(dots[[self$rowId]], rlang::inject(self$show_delete))),
                after = 0
              )
          }
          return(cols)
        }) |>
        purrr::walk(\(x) {
          shiny::insertUI(
            selector = self$insert_selector,
            where = 'afterBegin',
            ui = shiny::fluidRow(class = 'table-row', x)
          )
        })
    },
    '.delete_button' = function(rowId, ...) {
      dots <- rlang::list2(...) |> unlist(recursive = FALSE)
      if (is.null(dots$width)) width <- 2 else { width <- dots$width; dots$width <- NULL; }
      shiny::column(
        width = width,
        shiny::actionButton(
          inputId = rowId,
          class = 'btn-remove',
          style = 'background: #bf5959; color: #fff; border: none; width: 100%',
          label = "Delete"
        )
      )
    }
  )
)
