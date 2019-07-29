# FINISHED APP

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)

# Import the csv file
library(readr)
maples <- read_csv("Maple Monitoring Clean Data 2014.csv")

# Make circumference a numeric variable
maples$Circumference <- as.numeric(maples$Circumference)
# change species to a factor
maples$Species <- as.factor(maples$Species)
# change urbanization to a factor
maples$Urbanization <- as.factor(maples$Urbanization)
# change habitat to a factor
maples$Habitat <- as.factor(maples$Habitat)
# change shading to a factor
maples$Shading <- as.factor(maples$Shading)
# change date to a date
maples$Date <- as.Date(maples$Date, "%m/%d/%Y")

ui <- fluidPage(
  img(src = "MMM.png", height = 287, width = 900),
  titlePanel("Maple Tree Trunk Circumferences"),
  sidebarLayout(
    sidebarPanel(
      h4("North country volunteers have been monitoring maple trees in our region since fall 2013, providing", a("Nature Up North", href = "http://natureupnorth.org/"), "with records of maple trees in the area.  One of the variables we have been monitoring is tree trunk circumference."),
      h4("Here are the results, showing frequency of trees of different sizes for different maples species, in different locations, and under varying shade conditions."),
      h4("Where are the biggest trees?"),
      img(src = "Maple Leaf.gif", height = 108, width = 144, style = "display: block; margin-left: auto; margin-right: auto;"),
      br(),
      h4("Click on the tabs above the plot to see different histograms, and make selections in the corresponding options below to change the output accordingly.", style = "color:grey"),
      helpText("Note: Options below will only change the output of the plot in the corresponding tab."),
      fluidRow(
        strong("Species", style = "color:orange"),
        selectInput("species",
                    label = NULL,
                    choices = c("Norway Maple",
                                "Red Maple",
                                "Silver Maple",
                                "Sugar Maple"),
                    selected = "Norway Maple")
      ),
      fluidRow(
        strong("Habitat Type", style = "color:darkgreen"),
        selectInput("habitat",
                    label = NULL,
                    choices = c("Developed Area",
                                "Home Lawn",
                                "Natural Setting",
                                "School Garden",
                                "School Lawn",
                                "School Paved Area"),
                    selected = "Natural Setting")
      ),
      fluidRow(
        strong("Shading", style = "color:#165970"),
        selectInput("shading",
                    label = NULL,
                    choices = c("Shaded",
                                "Partially Shaded",
                                "Open"),
                    selected = "Shaded")
      ),
      fluidRow(
        strong("Urban Area", style = "color:#543b1f"),
        helpText("Is the tree within 100 feet of a building, concrete, or asphalt?"),
        selectInput("urbanization",
                    label = NULL,
                    choices = c("Yes",
                                "No"),
                    selected = "Yes")
      ),
      br(),
      br(),
      helpText("Copyright 2019, Nature Up North.  Created by Laura Kleist.")
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("All", plotOutput("allhist"), strong("Summary Statistics For This Histogram"), verbatimTextOutput("allstats")),
        tabPanel("Species", plotOutput("sphist"), strong("Summary Statistics For This Histogram"), verbatimTextOutput("astats")),
        tabPanel("Habitat Type", plotOutput("hahist"), strong("Summary Statistics For This Histogram"), verbatimTextOutput("bstats")),
        tabPanel("Shading", plotOutput("shhist"), strong("Summary Statistics For This Histogram"), verbatimTextOutput("dstats")),
        tabPanel("Urbanization", plotOutput("urhist"), strong("Summary Statistics For This Histogram"), verbatimTextOutput("cstats"))
      ),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      img(src = "NUN is a project of SLU.png", height = 86, width = 986, style = "display: block; margin-left: auto; margin-right: auto;")
    )
  )
)

server <- function(input, output) {
  a <- reactive({
    switch(input$species,
           "Norway Maple" = filter(maples, Species == "Norway Maple"),
           "Red Maple" = filter(maples, Species == "Red Maple"),
           "Silver Maple" = filter(maples, Species == "Silver Maple"),
           "Sugar Maple" = filter(maples, Species == "Sugar Maple"))
  })
  b <- reactive({
    switch(input$habitat,
           "Developed Area" = filter(maples, Habitat == "City or Community Park (developed)"),
           "Home Lawn" = filter(maples, Habitat == "Home lawn"),
           "Natural Setting" = filter(maples, Habitat == "Natural Setting (non-developed park, refuge, nature center, open space, forest)"),
           "School Garden" = filter(maples, Habitat == "School garden"),
           "School Lawn" = filter(maples, Habitat == "School lawn"),
           "School Paved Area" = filter(maples, Habitat == "School paved area"))
  })
  d <- reactive({
    switch(input$urbanization,
           "Yes" = filter(maples, Urbanization == "Yes"),
           "No" = filter(maples, Urbanization == "No"))
  })
  c <- reactive({
    switch(input$shading,
           "Shaded" = filter(maples, Shading == "Shaded (less than 2hr per day of direct sun)"),
           "Partially Shaded" = filter(maples, Shading == "Partially Shaded (2-5hr per day of direct sun)"),
           "Open" = filter(maples, Shading == "Open (more than 5hr per day of direct sun)"))
  })
  output$allhist <- renderPlot({ ggplot(maples, aes(Circumference)) + 
      geom_histogram(stat = "count", fill = "dark green") +
      xlab("Circumference (inches)") +
      ylab("Number of Trees") +
      theme_minimal() +
      ggtitle("All Trees") +
      theme(plot.title = element_text(size = 30, face = "bold"))
  })
  output$allstats <- renderPrint({summary(maples$Circumference)})
  output$sphist <- renderPlot({ ggplot(a(), aes(Circumference)) + 
      geom_histogram(stat = "count", fill = "orange") +
      ggtitle("Maple Species") +
      xlab("Circumference (inches)") +
      ylab("Number of Trees") +
      theme_minimal() +
      theme(plot.title = element_text(size = 30, face = "bold"))
  })
  output$astats <- renderPrint({summary(a()$Circumference)})
  output$hahist <- renderPlot({ ggplot(b(), aes(Circumference)) + 
      geom_histogram(stat = "count", fill = "dark green") +
      ggtitle("Habitat Type") +
      xlab("Circumference (inches)") +
      ylab("Number of Trees") +
      theme_minimal() +
      theme(plot.title = element_text(size = 30, face = "bold"))
  })
  output$bstats <- renderPrint({summary(b()$Circumference)})
  output$urhist <- renderPlot({ ggplot(d(), aes(Circumference)) + 
      geom_histogram(stat = "count", fill = "#543b1f") +
      ggtitle("Urban Area") +
      xlab("Circumference (inches)") +
      ylab("Number of Trees") +
      theme_minimal() +
      theme(plot.title = element_text(size = 30, face = "bold"))
  })
  output$dstats <- renderPrint({summary(c()$Circumference)})
  output$shhist <- renderPlot({ ggplot(c(), aes(Circumference)) + 
      geom_histogram(stat = "count", fill = "#165970") +
      ggtitle("Shade Conditions") +
      xlab("Circumference (inches)") +
      ylab("Number of Trees") +
      theme_minimal() +
      theme(plot.title = element_text(size = 30, face = "bold"))
  })
  output$cstats <- renderPrint({summary(d()$Circumference)})
}

shinyApp(ui = ui, server = server)