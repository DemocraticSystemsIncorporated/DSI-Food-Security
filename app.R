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
    
    #No filters leaves a dataset so big it won't graph unless a state filter is used.
    
    #Filters by case
    if(input$filters == "Citizenship"){
      data <- select(data, FSecurity, Cit)
    }
    
    if(input$filters == "Disability"){
      data <- select(data, FSecurity, Dis)
    }
    
    if(input$filters == "Education"){
      data <- select(data, FSecurity, edu)
    }
    
    if(input$filters == "Food Stamps"){
      data <- select(data, FSecurity, Food)
    }
    
    if(input$filters == "Income"){
      data <- select(data, FSecurity, inc)
    }
    
    if(input$filters == "Industry"){
      data <- select(data, FSecurity, Ind)
    }
    
    if(input$filters == "Number of Jobs"){
      data <- select(data, FSecurity, jobs)
    }
    
    if(input$filters == "Race"){
      data <- select(data, FSecurity, races)
    }
    
    if(input$filters == "Sex"){
      data <- select(data, FSecurity, sexes)
    }

#---------------------------------------------
    
    #States
    #AL-CT are all missing any entries so they just get a missing message.
    if(input$states == "AL"){
      showModal(modalDialog(
        title = "Missing Data",
        paste0("No available data from Alabama."),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    if(input$states == "AK"){
      showModal(modalDialog(
        title = "Missing Data",
        paste0("No available data from Alaska."),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    if(input$states == "AZ"){
      showModal(modalDialog(
        title = "Missing Data",
        paste0("No available data from Arizona."),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    if(input$states == "AR"){
      showModal(modalDialog(
        title = "Missing Data",
        paste0("No available data from Arkansas."),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    if(input$states == "CA"){
      showModal(modalDialog(
        title = "Missing Data",
        paste0("No available data from California."),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    if(input$states == "CO"){
      showModal(modalDialog(
        title = "Missing Data",
        paste0("No available data from Colorado."),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    if(input$states == "CT"){
      showModal(modalDialog(
        title = "Missing Data",
        paste0("No available data from Connecticut."),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    #The rest of the states actually have data so they get real filters.
    
    if(input$states == "DE"){
      temp <- filter(data, states == "DE")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "FL"){
      temp <- filter(data, states == "FL")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "GA"){
      temp <- filter(data, states == "GA")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "HI"){
      temp <- filter(data, states == "HI")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "ID"){
      temp <- filter(data, states == "ID")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "IL"){
      temp <- filter(data, states == "IL")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "IN"){
      temp <- filter(data, states == "IN")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "IA"){
      temp <- filter(data, states == "IA")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "KS"){
      temp <- filter(data, states == "KS")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "KY"){
      temp <- filter(data, states == "KY")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "LA"){
      temp <- filter(data, states == "LA")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "ME"){
      temp <- filter(data, states == "ME")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "MD"){
      temp <- filter(data, states == "MD")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "MA"){
      temp <- filter(data, states == "MA")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "MI"){
      temp <- filter(data, states == "MI")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "MN"){
      temp <- filter(data, states == "MN")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "MS"){
      temp <- filter(data, states == "MS")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "MO"){
      temp <- filter(data, states == "MO")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "MT"){
      temp <- filter(data, states == "MT")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "NE"){
      temp <- filter(data, states == "NE")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "NV"){
      temp <- filter(data, states == "NV")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "NH"){
      temp <- filter(data, states == "NH")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "NJ"){
      temp <- filter(data, states == "NJ")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "NM"){
      temp <- filter(data, states == "NM")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "NY"){
      temp <- filter(data, states == "NY")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "NC"){
      temp <- filter(data, states == "NC")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "ND"){
      temp <- filter(data, states == "ND")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "OH"){
      temp <- filter(data, states == "OH")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "OK"){
      temp <- filter(data, states == "OK")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "OR"){
      temp <- filter(data, states == "OR")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "PA"){
      temp <- filter(data, states == "PA")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "RI"){
      temp <- filter(data, states == "RI")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "SC"){
      temp <- filter(data, states == "SC")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "SD"){
      temp <- filter(data, states == "SD")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "TN"){
      temp <- filter(data, states == "TN")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "TX"){
      temp <- filter(data, states == "TX")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "UT"){
      temp <- filter(data, states == "UT")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "VT"){
      temp <- filter(data, states == "VT")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "VA"){
      temp <- filter(data, states == "VA")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "WA"){
      temp <- filter(data, states == "WA")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "WV"){
      temp <- filter(data, states == "WV")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "WI"){
      temp <- filter(data, states == "WI")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "WY"){
      temp <- filter(data, states == "WY")
      data <- select(temp, FSecurity, states)
    }
    
#--------------------------------------------
    
   barplot(table(data)) #Making actual plot
   
})})

shinyApp(ui = ui, server = server)