library(bslib)
library(DT) # data explorer
library(ggvis) # visualizations
library(leaflet) # interactive map
library(lubridate)
library(shiny)

load("dat/aac_dataset.rda")

get_plot <- function(input) {
  switch(
    input$plot,
    # - line -
    # - scatter -
    # - bar -
    "Most Common Names" = {
      name_tbl <- head(
        sort(table(aac_dataset$name), decreasing = TRUE),
        input$nameCount + 1
      )[-1]

      name_tbl |> as.data.frame() |> 
        setNames(c("name", "count")) |> 
        ggvis(~name, ~count) #|>
        #mark_rect(props(y2 = 0, width = band())) |>
        

      #aac_dataset |> dplyr::count(name) |> dplyr::top_n(10) |> ggvis
    },
    # - pie -
    # - box -
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
      )
    ),
    hr(),
    DTOutput("aac_dataset")
  ),

  tabPanel(
    title = "Interactive Map"
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
              "plot", "Select Plot",
              choices = NULL,
              selected = character(0)
            )
          )
        ),
        conditionalPanel(
          "input.plot",
          wellPanel(
            h3("Plot Options"),
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
    df <- aac_dataset |> dplyr::filter(
      inAge %in% input$age[1]:input$age[2],
      outAge %in% input$age[1]:input$age[2],
      input$animalType == "" | animalType %in% input$animalType,
      is.null(input$breed) | breed %in% input$breed,
      is.null(input$inType) | inType %in% input$inType,
      is.null(input$outType) | outType %in% input$outType
    )

    return(df)
  })

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
      session, inputId = "plot",
      choices = plots, selected = character(0)
    )
  })

  output$plot <- renderPlot({ # resizable plots? ggvis? ggplot2? standard plots?
    #if (is.null(input$plot)) ggvis() else get_plot(input)
    ggvis()
  })

  # - models -
}

shinyApp(ui = ui, server = server)
