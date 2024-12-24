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
aac_dataset_geo <- aac_dataset |> filter(lat != -1 & lon != -1)

toggle <- function(switch) {
  if (switch) {
    aac_dataset <<- aac_dataset_full
  } else {
    aac_dataset <<- aac_dataset_geo
  }
}

get_plot <- function(input) {
  if (is.null(input$plotName)) return(ggplot() + default_theme)

  switch(
    input$plotName,

    "Intake over Time" = {
      filtered <- aac_dataset |>
        filter(
          inDateTime >= ymd(input$xTimeRange[1]) &
            inDateTime <= ymd(input$xTimeRange[2])
        )

      in_tbl <- table(as.Date(filtered$inDateTime)) |>
        as.data.frame() |>
        setNames(c("inDateTime", "Count")) |>
        mutate(inDateTime = as.Date(inDateTime))

      if (input$xTimeFrame == "Weekly") {
        in_tbl <- in_tbl |>
          mutate(inDateTime = week(inDateTime) + (
            52 * (year(inDateTime) - year(ymd(input$xTimeRange[1])))
          )) |>
          aggregate(Count ~ inDateTime, sum) |>
          mutate(inDateTime = ymd(input$xTimeRange[1]) + (7 * inDateTime - 1))
      }

      in_tbl |>
        ggplot(aes(inDateTime, Count, group = 1)) +
        ggtitle("Intake Count over Time") +
        xlab("Time") +
        default_theme +
        geom_line() +
        scale_x_date(breaks = scales::pretty_breaks(n = 10))
    },

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

    "Most Common Colors" = {
      color_tbl <- head(
        sort(table(aac_dataset$color), decreasing = TRUE),
        input$colorCount
      )

      color_tbl |>
        as.data.frame() |>
        setNames(c("Color", "Count")) |>
        ggplot(aes(Color, Count)) +
        ggtitle("Most Common Colors") +
        default_theme +
        geom_col()
    },

    "Animal Types over Time Range" = {
      filtered <- aac_dataset |>
        filter(
          inDateTime >= ymd(input$pieTimeRange[1]) &
            inDateTime <= ymd(input$pieTimeRange[2])
        )

      type_tbl <- sort(table(filtered$animalType), decreasing = TRUE)

      custom_theme <- theme(
        plot.background = element_rect(fill = "grey", color = "black"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12)
      )

      type_tbl |>
        as.data.frame() |>
        setNames(c("Type", "Amount")) |>
        ggplot(aes(x = "", y = Amount, fill = Type)) +
        theme_void() +
        ggtitle("Animal Types over Time Range") +
        custom_theme +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0)
    },

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
    leafletOutput("map", width = "100%", height = "93vh")
  ),

  tabPanel(
    title = "Visualizations",
    fluidRow(
      column(3,
        wellPanel(
          h3("Select a Plot"),
          radioButtons(
            "plotName", "Options:",
            choices = c(
              "Intake over Time", "Most Common Names",
              "Most Common Colors", "Animal Types over Time Range"
            ),
            selected = character(0)
          )
        ),
        conditionalPanel(
          "input.plotName",
          wellPanel(
            h3("Plot Options"),
            conditionalPanel(
              "input.plotName == 'Intake over Time'",
              dateRangeInput(
                "xTimeRange", "Time Range to Measure",
                start = min(aac_dataset$inDateTime),
                min = min(aac_dataset$inDateTime),
                end = max(aac_dataset$inDateTime),
                max = max(aac_dataset$inDateTime)
              ),
              radioButtons(
                "xTimeFrame", "Time Frame",
                choices = c("Daily", "Weekly")
              )
            ),
            conditionalPanel(
              "input.plotName == 'Most Common Names'",
              sliderInput(
                "nameCount", "How many names should be charted?",
                min = 3, max = 25, value = 10
              )
            ),
            conditionalPanel(
              "input.plotName == 'Most Common Colors'",
              sliderInput(
                "colorCount", "How many colors should be charted?",
                min = 3, max = 25, value = 10
              )
            ),
            conditionalPanel(
              "input.plotName == 'Animal Types over Time Range'",
              dateRangeInput(
                "pieTimeRange", "Time Range to Measure",
                start = min(aac_dataset$inDateTime),
                min = min(aac_dataset$inDateTime),
                end = max(aac_dataset$inDateTime),
                max = max(aac_dataset$inDateTime)
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
    title = "Models",

    tags$style(
      "
      .model-outer {
        display: flex;
        align-items: center;
        flex-direction: column;
        margin: auto;
      }
      "
    ),

    div(
      class = "model-outer",
      h1("- work in progress -")
    )
  ),

  tabPanel(
    title = "Settings",
    wellPanel(
      h3("Dataset Settings"),
      input_switch(
        "geo_switch", "Allow data with missing geolocation info?", value = TRUE
      )
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
  output$map <- renderLeaflet({
    leaflet(aac_dataset_geo) |>
      addTiles(group = "OSM") |>
      addProviderTiles("CartoDB.Voyager", group = "Simplified") |>
      addProviderTiles("Esri.WorldTopoMap", group = "Topographic") |>
      addHeatmap(
        group = "Heatmap", lat = ~lat, lng = ~lon,
        max = 0.95, blur = 30, radius = 20
      ) |>
      addAwesomeMarkers(
        group = "Points", lat = ~lat, lng = ~lon,
        clusterOptions = markerClusterOptions(),
        popup = ~paste(
          "<h4><strong>", name, " (", AID, ")</strong></h4>",
          "<hr/>",
          "Type: ", animalType, " - ", breed, "<br/>",
          "Age: ", inAge, "<br/>",
          "Intake Datetime: ", inDateTime, "<br/>",
          "Intake Type: ", inType, "<br/>",
          "Intake Condition: ", inCond, "<br/>"
        )
      ) |>
      addLayersControl(
        baseGroups = c("OSM", "Simplified", "Topographic"),
        overlayGroups = c("Heatmap", "Points")
      ) |>
      addMiniMap(toggleDisplay = TRUE) |>
      setView(lat = 30.26, lng = -97.745, zoom = 11)
  })

  # - visualizations -
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
