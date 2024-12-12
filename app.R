library(bslib)
library(dplyr)
library(DT) # data explorer
library(ggplot2) # visualizations
library(leaflet) # interactive map
library(leaflet.extras)
library(lubridate)
library(shiny)

load("dat/aac_dataset.rda")

default_theme <- theme(
  plot.background = element_rect(fill = "grey", color = "black"),
  plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
  axis.title = element_text(size = 16),
  axis.text = element_text(size = 12)
)

aac_dataset_full <- aac_dataset
aac_dataset_min <- aac_dataset |> filter(lat != -1 & lon != -1)

toggle <- function(switch) {
  if (switch) {
    aac_dataset <<- aac_dataset_full
  } else {
    aac_dataset <<- aac_dataset_min
  }
}

get_plot <- function(input) {
  switch(
    input$plotName,
    # - line -

    # - scatter -

    # - bar -
    "Most Common Names" = {
      name_tbl <- head(
        sort(table(aac_dataset$name), decreasing = TRUE),
        input$nameCount + 1
      )[-1]

      name_tbl |>
        as.data.frame() |>
        setNames(c("Name", "Count")) |>
        ggplot(aes(Name, Count)) +
        ggtitle("Most Common Names") +
        default_theme +
        geom_col()
    },

    # - pie -

    # - box -

    # - default -
    ggplot() +
      default_theme
  )
}

ui <- navbarPage(
  title = "Austin Animal Center Data",
  id = "nav",

  tabPanel(
    title = "Data Explorer",
    fluidRow(
      column(4, # age
        sliderInput("age", "Age", min = 0, max = 30, value = c(0, 30))
      ),
      column(4, # animal type
        selectInput(
          "animalType", "Animal Type",
          choices = c("All types" = "", sort(unique(aac_dataset$animalType)))
        )
      ),
      column(4, # breed
        conditionalPanel(
          "input.animalType != ''",
          selectizeInput(
            "breed", "Breed", choices = c("All breeds" = ""), multiple = TRUE
          )
        )
      )
    ),
    fluidRow(
      column(4, # inType
        selectInput(
          "inType", "Intake Type",
          choices = c("All types" = "", sort(unique(aac_dataset$inType))),
          multiple = TRUE
        )
      ),
      column(4, # outType
        selectInput(
          "outType", "Outcome Type",
          choices = c("All types" = "", sort(unique(aac_dataset$outType))),
          multiple = TRUE
        )
      ),
      column(4, # column selector
        selectInput(
          "columns", "Columns",
          choices = c("All columns" = "", colnames(aac_dataset)),
          selected = "", multiple = TRUE
        )
      )
    ),
    hr(),
    DTOutput("aac_dataset")
  ),

  tabPanel(
    title = "Interactive Map",

    tags$style(
      "
      #controls {
        background-color: #FFFFFF;
        padding: 20px;
        opacity: 0.65;
        transition: opacity 250ms 500ms;
      }
      #controls:hover {
        opacity: 0.9;
      }
      "
    ),

    leafletOutput("map", width = "100%", height = "93vh"),
    absolutePanel(
      id = "controls", class = "panel panel-default", fixed = TRUE,
      draggable = TRUE, top = 106, left = "auto", right = 50,
      bottom = "auto", width = "auto", height = "auto",

      h2("Map Options"), 
    )
  ),

  tabPanel(
    title = "Visualizations",
    fluidRow(
      column(3,
        wellPanel(
          h3("Plot Type"),
          radioButtons(
            "plotType", "Plot Type",
            choices = c("Line", "Scatter", "Bar", "Pie", "Box"),
            selected = character(0)
          ),
          conditionalPanel(
            "input.plotType",
            selectInput(
              "plotName", "Select Plot",
              choices = NULL,
              selected = character(0)
            )
          )
        ),
        conditionalPanel(
          "input.plotName",
          wellPanel(
            h3("Plot Options"),
            conditionalPanel(
              "input.plotName == 'Most Common Names'",
              sliderInput(
                "nameCount", "How many names should be charted?",
                min = 3, max = 25, value = 10
              )
            )
          )
        )
      ),
      column(9,
        plotOutput("plot")
      )
    )
  ),

  tabPanel(
    title = "Models"
  ),

  tabPanel(
    title = "Settings",
    wellPanel(
      h3("Dataset Settings"),
      input_switch(
        "geo_switch", "Allow data with missing geolocation info?", value = TRUE
      ),
    )
  )
)

server <- function(input, output, session) {
  # - data explorer -
  observe({
    breeds <- if (input$animalType == "") character(0) else {
      sort(
        unique(aac_dataset$breed[aac_dataset$animalType %in% input$animalType])
      )
    }

    updateSelectizeInput(
      session, inputId = "breed",
      choices = breeds,
      server = TRUE
    )
  })

  output$aac_dataset <- renderDT({
    df <- aac_dataset |> filter(
      inAge %in% input$age[1]:input$age[2],
      outAge %in% input$age[1]:input$age[2],
      input$animalType == "" | animalType %in% input$animalType,
      is.null(input$breed) | breed %in% input$breed,
      is.null(input$inType) | inType %in% input$inType,
      is.null(input$outType) | outType %in% input$outType
    )
    
    if (!is.null(input$columns)) df <- df[, input$columns]

    return(df)
  }) |>
    bindEvent(
      input$age, input$animalType, input$breed,
      input$inType, input$outType, input$columns,
      input$geo_switch
    )

  # - interactive map -

  # - visualizations -
  observe({
    plots <- if (is.null(input$plotType)) character(0) else switch(
      input$plotType,
      "Line" = c("X over Time", "placeholder"),
      "Scatter" = c("placeholder"),
      "Bar" = c("Most Common Names", "placeholder"),
      "Pie" = c("placeholder"),
      "Box" = c("placeholder")
    )

    updateSelectInput(
      session, inputId = "plotName",
      choices = plots, selected = character(0)
    )
  })

  output$plot <- renderPlot({
    get_plot(input)
  })

  # - models -

  # - settings -
  observe({
    toggle(input$geo_switch)
    output$plot <- renderPlot({
      get_plot(input)
    })
  }) |> bindEvent(input$geo_switch)
}

shinyApp(ui = ui, server = server)
