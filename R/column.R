define_column <- function(width, val, ...) {
  shiny::column(
    width = width,
    rlang::eval_tidy(val),
    ...
  )
}

define_row <- function(rowId, ..., show_delete = NULL) {
  if (!is.null(show_delete)) {
    show_delete <- delete_button(rowId, rlang::inject(show_delete))
  }
  shiny::fluidRow(
    id = rowId,
    class = 'table-row',
    show_delete,
    ...
  )
}

define_table <- function(ns, ..., insert_selector = '#table') {
  rlang::list2(...) |>
    purrr::walk(\(x) {
      print(x)
      # shiny::insertUI(
      #   selector = insert_selector,
      #   where = 'afterBegin',
      #   ui = x
      # )
    })
}

create_table <- function(data, defined_table) {

}

# create_table(
#   data.frame('cyl' = mtcars[,'cyl']),
#   define_table(
#     ns = shiny::NS('NS'),
#     define_row(
#       rowId = digest::digest(Sys.time()),
#       define_column(
#         width = 10,
#         val = shinyjs::disabled(
#           shiny::textInput(
#             inputId = ns(glue::glue("{id}_rowId")),
#             label = NULL,
#             placeholder = 'Row key',
#             value =
#           )
#         )
#       ),
#       show_delete = list('width' = 2)
#     )
#   )
# )
#
# define_table(
#   ns = shiny::NS('NS'),
#   define_row(
#     id = digest::digest(Sys.time()),
    # define_column(
    #   width = 10,
    #   val = shinyjs::disabled(
    #     shiny::textInput(
    #       inputId = ns(glue::glue("{rowId}_rowId")),
    #       label = NULL,
    #       placeholder = 'Row key',
    #       value = ''
    #     )
    #   )
    # )
#     show_delete = delete_button(inputId = ns(glue::glue("{id}_remove")))
#   )
# )
