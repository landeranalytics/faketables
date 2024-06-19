pkgload::load_all()
## ---- col_def --------
c_def <- list(
  faketables::col_def(
    name = 'Name',
    input = faketables::input_call(
      fun = \(...) { shinyjs::disabled(shiny::textInput(...)) },
      args = list(label = NULL)
    ),
    cast = as.character,
    width = 2
  ),
  faketables::col_def(
    name = 'Address',
    input = faketables::input_call(
      fun = shiny::textInput,
      args = list(label = NULL)
    ),
    cast = as.character,
    width = 2
  ),
  faketables::col_def(
    name = 'City',
    input = faketables::input_call(
      fun = shiny::selectInput,
      args = list(
        label = NULL,
        choices = c('Bronx', 'Brooklyn', 'New York', 'Queens', 'Staten Island')
      )
    ),
    cast = as.character,
    width = 2
  ),
  faketables::col_def(
    name = 'Zip',
    input = faketables::input_call(
      fun = shiny::numericInput,
      args = list(label = NULL, min = 10001, max = 11697, step = 1)
    ),
    cast = as.integer,
    width = 1
  ),
  faketables::col_def(
    name = 'FavoritePizza',
    input = faketables::input_call(
      fun = shiny::textInput,
      args = list(label = NULL)
    ),
    cast = as.character,
    width = 2,
    display_name = 'Favorite Pizza'
  ),
  faketables::col_def(
    name = 'Rating',
    input = faketables::input_call(
      fun = shiny::sliderInput,
      args = list(label = NULL, min = 11, max = 20)
    ),
    cast = as.integer,
    width = 2
  )
)

## ---- table_def --------
t_def <- faketables::table_def(c_def)

## ---- faketables --------
pz <-
  jsonlite::read_json(
    path = 'https://www.jaredlander.com/data/FavoritePizzaPlaces.json',
    simplifyVector = TRUE
  ) |>
  tibble::as_tibble() |>
  tidyr::unnest(cols = c('Details', 'Coordinates')) |>
  dplyr::mutate(
    'Zip' = as.integer(.data$Zip),
    'FavoritePizza' = 'Cheese',
    'Rating' = 11L
  ) |>
  faketables::faketable(table_def = t_def, show_delete = list(width = 1))

## ---- ui --------
ui <- bslib::page_navbar(
  title = 'Favorite Pizza Places',
  header = shinyjs::useShinyjs(),
  bslib::nav_panel(
    title = 'NYC',
    bslib::layout_columns(
      col_widths = c(4, 8),
      min_height = '500px',
      bslib::card(
        bslib::card_header('Add Favorite'),
        bslib::card_body(
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  shiny::textInput('name', label = 'Name')
                ),
                shiny::column(
                  width = 6,
                  shiny::textInput('address', label = 'Address')
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  shiny::selectInput(
                    'city',
                    label = 'City',
                    choices = c('Bronx', 'Brooklyn', 'New York', 'Queens', 'Staten Island'),
                    selected = 'New York')
                ),
                shiny::column(
                  width = 6,
                  shiny::numericInput(
                    'zip',
                    label = 'Zip',
                    value = 10001,
                    min = 10001,
                    max = 11697,
                    step = 1)
                  )
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  shiny::textInput('latitude', label = 'Latitude')
                ),
                shiny::column(
                  width = 6,
                  shiny::textInput('longitude', label = 'Longitude')
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  shiny::textInput('favoritePizza', label = 'Favorite Pizza')
                ),
                shiny::column(
                  width = 6,
                  shiny::sliderInput(
                    'rating',
                    label = 'Pizza Rating',
                    min = 11,
                    max = 20,
                    value = 11)
                )
              )
            )
          ),
          shiny::fluidRow(
            shiny::actionButton('add', label = 'Add Data')
          ),
          shiny::fluidRow(
            shiny::actionButton('update', label = 'Update')
          )
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header('Map'),
        bslib::card_body(
          leaflet::leafletOutput(outputId = 'map'),
        )
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header('Table'),
      bslib::card_body(
        faketables::faketablesUI()
      )
    )
  )
)

## ---- server --------
server <- function(input, output, session) {
  pz <- faketables::faketablesServer(faketable = pz)
  output$map <-
    leaflet::leaflet() |>
    leaflet::addTiles() |>
    leaflet::setView(-74.0060, 40.7128, 11) |>
    leaflet::renderLeaflet()

  shiny::observe({
    ins <- tibble::tibble(
      'Name' = input$name,
      'Address' = input$address,
      'City' = input$city,
      'State' = 'NY',
      'Zip' = as.integer(input$zip),
      'latitude' = as.numeric(input$latitude),
      'longitude' = as.numeric(input$longitude),
      'FavoritePizza' = input$favoritePizza,
      'Rating' = as.integer(input$rating)
    ) |>
      suppressWarnings()
    if (any(is.na(unlist(ins)) | unlist(ins) == '')) {
      shiny::showNotification(
        'Something went wrong. Please validate your input data and try again.',
        type = 'error'
      )
    } else {
      faketables::faketablesInsert(pz, ins)
    }
  }) |>
    shiny::bindEvent(input$add)

  shiny::observe({
    map_data <-
      pz()@x |>
      dplyr::mutate(
        'label' = shiny::HTML(glue::glue('{.data$Name}<br>{.data$Address}, {.data$City}, NY, {.data$Zip}')),
        .by = '.rowId'
      ) |>
      dplyr::mutate(
        'fill_color' = dplyr::case_when(
          .data$label == 'cheese' ~ '#FFCA45',
          .default = '#FFCA45'
        )
      )
    leaflet::leafletProxy('map', data = map_data) |>
      leaflet::clearMarkers() |>
      leaflet::addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = ~Rating + 4,
        color = '#C9AA6E',
        weight = 4,
        opacity = 1,
        fillColor = ~fill_color,
        fillOpacity = 1,
        label = ~label
      )
  }) |>
    shiny::bindEvent(input$update, pz())
}

## ---- run --------
shiny::shinyApp(ui, server)
