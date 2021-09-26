#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rgdal)
library(viridis)
library(fillmap)

data <- read.csv("data/wpd_arrests_2010-2018_clean.csv")
effects <- read.csv("data/fixedeffects.csv")[,-1]
NHtracts <- read.csv("data/tl_2016_37_129_tract.shp")

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("WPD Arrests (2010-2018)"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("year",
        "Year",
        min = 2010,
        max = 2018,
        value = 2010
      )
    ),
    radioButtons(
      "data",
      "Data",
      c(
        "Total Arrests",
        "Black Only Arrests",
        "White Only Arrests"
      )
    ),
    radioButtons(
      "adjustments",
      "Adjustments",
      c(
        "None",
        "Percent of Total Population",
        "Percent of Total Arrests",
        "SIR/MIR",
        "Poisson Regression"
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      textOutput("text"),
      plotOutput("map"),
      tableOutput("table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  data_indexer <- seq(input$year - 2009, dim(data)[1], 9)

  output$text <- renderText()

  output$map <- renderPlot({
    if (input$adjustments == "None") {
      switch(input$data,
        "Total Arrests" = {
          map_data <- data$arrests_total[data_indexer]
          map_scale <- data$arrests_total
        },
        "Black Only Arrests" = {
          map_data <- data$arrests_B[data_indexer]
          map_scale <- data$arrests_B
        },
        "White Only Arrests" = {
          map_data <- data$arrests_W[data_indexer]
          map_scale <- data$arrests_W
        }
      )
    } else if (input$data == "Total Arrests") {
      switch(input$adjustments,
        "Percent of Total Population" = {
          map_data <- data$pct_arrests[data_indexer]
          map_scale <- data$pct_arrests * 100
        },
        "Percent of Total Arrests" = {
          map_data <- (
            data$arrests_total[data_indexer] / (data$arrests_total[data_indexer] + .1)
          ) * 100
          map_scale <- seq(0, 100, .1)
        },
        "SIR/MIR" = {
          map_data <- data$SIR[data_indexer]
          map_scale <- data$SIR
        },
        "Poisson Regression" = {}
      )
    } else if (input$data == "Black Only Arrests") {
      switch(input$adjustments,
        "Percent of Total Population" = {
          map_data <- data$pct_black_arrests[data_indexer]
          map_scale <- data$pct_black_arrests * 100
        },
        "Percent of Total Arrests" = {
          map_data <- data$arrests_B_pct[data_indexer]
          map_scale <- seq(0, 100, .1)
        },
        "SIR/MIR" = {
          map_data <- data$SIR_B[data_indexer]
          map_scale <- data$SIR_B
        },
        "Poisson Regression" = {}
      )
    } else {
      switch(input$adjustments,
        "Percent of Total Population" = {
          map_data <- data$pct_white_arrests[data_indexer]
          map_scale <- data$pct_white_arrests * 100
        },
        "Percent of Total Arrests" = {
          map_data <- data$arrests_W_pct[data_indexer]
          map_scale <- seq(0, 100, .1)
        },
        "SIR/MIR" = {
          map_data <- data$SIR_W[data_indexer]
          map_scale <- data$SIR_W
        },
        "Poisson Regression" = {}
      )
    }

    map_caption <- paste(input$year, input$data)

    fillmap2(
      NHtracts,
      map_caption,
      map_data,
      y.scl = map_scale,
      map.lty = 0,
      leg.loc = "below",
      leg.rnd = 2
    )
  })

  output$table <- renderTable({
    if (input$adjustments == "Poisson Regression") {
      switch(
        input$data,
        "Total Arrests" = {

        }
      )
    }
  })

  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = "darkgray", border = "white")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
