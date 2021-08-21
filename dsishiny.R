source("dsiproject.R") # link to data file
library(tidyverse)
library(modelr)
library(ggplot2)
library(shiny)

# UI

ui <- (fluidPage(
  titlePanel("Food Security"),
    sidebarPanel(
      #Making filter choice bar
    selectInput("filters","Select a Filter", c("None",
                                            "Citizenship",
                                            "Disability",
                                            "Education",
                                            "Food Security",
                                            "Food Stamps",
                                            "Income",
                                            "Industry",
                                            "Number of Jobs",
                                            "Race",
                                            "Sex"), 
                selected = "None"), #Default is no filters
    #Making state choice bar
    selectInput("states", "Select a State", c("All",
                                              "AL", 
                                              "AK",
                                              "AZ",
                                              "AR",
                                              "CA",
                                              "CO",
                                              "CT",
                                              "DE",
                                              "DC",
                                              "FL",
                                              "GA",
                                              "HI",
                                              "ID",
                                              "IL",
                                              "IN",
                                              "IA",
                                              "KS",
                                              "KY",
                                              "LA",
                                              "ME",
                                              "MD",
                                              "MA",
                                              "MI",
                                              "MN",
                                              "MS",
                                              "MO",
                                              "MT",
                                              "NE",
                                              "NV",
                                              "NH",
                                              "NJ",
                                              "NM",
                                              "NY",
                                              "NC",
                                              "ND",
                                              "OH",
                                              "OK",
                                              "OR",
                                              "PA",
                                              "RI",
                                              "SC",
                                              "SD",
                                              "TN",
                                              "TX",
                                              "UT",
                                              "VT",
                                              "VA",
                                              "WA",
                                              "WV",
                                              "WI",
                                              "WY"), 
                selected = "All")), #Default is national-level view
  
  mainPanel(
    plotOutput("plot")
    )
  ))

# Server

server <- (function(input, output){
  output$plot <- renderPlot({
    
    #Filters 
    if(input$filters != "None"){
      data <- filter(data, "Select a Filter" == input$filters)
    }
    
    #States
    if(input$states != "All"){
      data <- filter(data, "Select a State" == input$states)
    }
    
    barplot(table(data$FSecurity)) #Making actual plot
})})

shinyApp(ui = ui, server = server)