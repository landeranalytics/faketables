x <- tibble::tibble('name' = 'this is my test data', 'name2' = 'this is my other test data')
valid_input_call <-
  input_call(
    shiny::textInput,
    args = list(label = NULL)
  )
valid_col_def <-
  structure(
    tibble::tibble(
      'name' = 'name',
      'input_call' = list(valid_input_call),
      'cast' = list(as.character),
      'width' = 1,
      'display_name' = 'display_name'
    ),
    class = c(class(tibble::tibble()), 'col_def')
  )
valid_col_def2 <-
  structure(
    tibble::tibble(
      'name' = 'name2',
      'input_call' = list(valid_input_call),
      'cast' = list(as.character),
      'width' = 1,
      'display_name' = 'display_name'
    ),
    class = c(class(tibble::tibble()), 'col_def')
  )
valid_table_def <-
  structure(
    do.call(rbind, list(valid_col_def, valid_col_def2)),
    class = c(class(tibble::tibble()), 'table_def')
  )
valid_faketable <- faketable(x, valid_table_def)
