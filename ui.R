library(shinyIncubator)
library(leaflet)
library(CalEnviroScreen)

data(CES2, package = "CalEnviroScreen")
data(CES2_metadata, package = "CalEnviroScreen")

weightInput <- function (inputId, label=paste0(inputId, ":"), value=1.0, ...) {
  sliderInput(inputId, label, value=value, min=0, max=1, step=0.1, ...)
}

multiSelectInput <- function (inputId, label, choices, ...) {
  selectInput(inputId, label, choices, selected=choices, multiple=TRUE, ...)
}

REGION_NAMES <- c("San Joaquin", "South Coast", "Bay Area", "Other")

shinyUI(

  navbarPage(
    "CalEnviroScreen Explorer",
    id = "nav",

    tabPanel(
      "Interactive map",
      div(class="outer",

          progressInit(),

          tags$head(
            includeCSS("styles.css")#,
            #includeScript("gomap.js")
          ),

          leafletMap(
            "map", width="100%", height="100%",
            initialTileLayer = "http://{s}.tiles.mapbox.com/v3/dholstius.j4a4ab9e/{z}/{x}/{y}.png",
            initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
            options = list(
              center = c(36.5, -119.5),
              zoom = 7,
              maxBounds = list(list(17, -180), list(59, 180))
            )
          ),

          absolutePanel(
            id = "controls", class = "modal", fixed = TRUE, draggable = TRUE,
            top = 60, left = "auto", right = 20, bottom = "auto",
            width = 270, height = "auto",

            h3("Model Settings"),

            selectInput("method", "Methodology:", choices = c("CES 2.0", "Rank product"), selected = "CES 2.0"),
            multiSelectInput("pollution_vars", "Pollution burden:", choices = unname(CES2_POLLUTION_VARS)),
            multiSelectInput("popchar_vars", "Population characteristics:", choices = unname(CES2_POPCHAR_VARS)),
            selectInput("impacted_percentile", "Impacted (% statewide):",
                        choices = c( "5 %" =  5, "10 %" = 10, "15 %" = 15,
                                     "20 %" = 20, "25 %" = 25, "30 %" = 30,
                                     "35 %" = 35, "40 %" = 40, "45 %" = 45, "50 %" = 50),
                        selected = 20)
          ),

          tags$div(
            id="cite",
            'Data: ', tags$a('CalEnviroScreen 2.0', href="http://oehha.ca.gov/ej/ces2.html"), '| Code: ', tags$a('Superzip', href="https://github.com/jcheng5/superzip")
          )
      )
    ),

    tabPanel(
      "Data explorer",
      dataTableOutput("data_tbl")
    ),

    tabPanel(
      "Weights",
      fluidRow(
        column(3,
               h4("Exposure"),
               weightInput("Ozone"),
               weightInput("PM25"),
               weightInput("DieselPM"),
               weightInput("DrinkWat"),
               weightInput("PestUse"),
               weightInput("ToxRel"),
               weightInput("Traffic")),

        column(3,
               h4("Environment"),
               weightInput("Cleanup",  value = 0.5),
               weightInput("GndWat",   value = 0.5),
               weightInput("HazWaste", value = 0.5),
               weightInput("ImpWat",   value = 0.5),
               weightInput("SolWaste", value = 0.5)),

        column(3,
               h4("Sensitivity"),
               weightInput("Age"),
               weightInput("Asthma"),
               weightInput("LBW")),

        column(3,
               h4("Socioecon"),
               weightInput("Edu"),
               weightInput("LingIso"),
               weightInput("Poverty"),
               weightInput("Unemp")))
    ),

    conditionalPanel("false", icon("crosshair"))

  ))
