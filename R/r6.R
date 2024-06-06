faketable <- R6::R6Class(
  classname = 'faketable',
  public = list(
    data = NULL,
    insert_selector = NULL,
    rows = NULL
  )
)

fakerow <- R6::R6Class(
  classname = 'fakerow',
  public = list(
    id = NULL,
    columns = NULL,
    row = NULL,
    initialize = \(id, ..., show_delete = NULL) {
      dots <- rlang::list2(...)
      self$id <- id
      self$columns <- dots

      ui_row <- shiny::fluidRow(
        id = id,
        class = 'table-row',
        show_delete,
        ...
      )

      # shiny::insertUI(
      #   selector = insert_selector,
      #   where = 'afterBegin',
      #   ui = ui_row
      # )

      self$row <- ui_row
    }
  )
)

fakecol <- R6::R6Class(
  classname = 'fakecol',
  public = list(
    width = NULL,
    val = NULL,
    initialize = \(width, val, ...) {
      shiny::column(
        width = width,
        val,
        ...
      )
    }
  )
)

col <- fakecol$new(12, shiny::actionButton('test', 'HI'))
row <- fakerow$new('id', col)
row$row$children
