
# faketables

<!-- badges: start -->
<!-- badges: end -->

## Goals

- Minimize code required for end users
- Squash namespace problems
- Turn `faketables` into an `S7` object with the following
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
