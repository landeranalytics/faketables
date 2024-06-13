# faketables <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
<!-- badges: end -->

## Goals

- Minimize code required for end users
- Squash namespace problems
- Turn `faketables` into an `S7` (maybe `R6`) object with the following
  - Contains:
    - The current state of the data
    - The original data
    - Removed rows (could be the whole row, or just an ID)
    - Modified rows (could be the whole row, or just an ID)
    - Column definitions with info about (are these a separate class?):
      - Column name
      - Column class (`S7` base class?)
      - Shiny column width
      - Shiny column HTML
      - Display name
  - Data is modified to have an ID column, or the column name can be provided in the constructor
  - Methods for:
    - Updating rows
    - Removing rows
  - An object call re-draws the UI and returns the current table as a tibble
- To use, users must (this means it's probably a shiny module):
  - Add function call to UI
  - Add function call to server to create object


## Notes

- If R6, how best to get/set data?
  - Another R6 class?

## 2024-06-10

- Split into data history class and shiny module
- Shiny module is UI element that can be added to other shiny apps
- function that takes shiny input function and args and creates the column
  - Is `shinyjs::disabled()` an arg?
  - `create_fun(bare, ..., .editable = TRUE)`
- User supplies starting data, column definitions
  - Column definition could be an input or render function
- Table only shows columns specified in the definitions
