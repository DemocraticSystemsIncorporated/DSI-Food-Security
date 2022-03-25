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
                selected = "All"),#Default is national-level view
    selectInput("display", "Display Type", c("None Selected",
                                             "Percentage", 
                                             "Counts"),
                selected = "None Selected"),),#default is no view (no chart)
  
  
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
      #useLabels <- unique(g)
      #categories <- useLabels$Cit
      #print(categories)
      data <- select(data, FSecurity, Cit)
    }
    
    if(input$filters == "Disability"){
      #useLabels <- unique(h)
      #categories <- useLabels$Dis
      #print(categories)
      data <- select(data, FSecurity, Dis)
    }
    
    if(input$filters == "Education"){
      #useLabels <- unique(c)
      #categories <- useLabels$edu
      #print(categories)
      data <- select(data, FSecurity, edu)
    }
    
    if(input$filters == "Food Stamps"){
      #useLabels <- unique(j)
      #categories <- useLabels$Food
      #print(categories)
      data <- select(data, FSecurity, Food)
    }
    
    if(input$filters == "Income"){
      #useLabels <- unique(a)
      #categories <- useLabels$inc
      #print(categories)
      data <- select(data, FSecurity, inc)
    }
    
    if(input$filters == "Industry"){
      #useLabels <- unique(i)
      #categories <- useLabels$Ind
      #print(categories)
      data <- select(data, FSecurity, Ind)
    }
    
    if(input$filters == "Number of Jobs"){
      #useLabels <- unique(f)
      #categories <- useLabels$jobs
      #print(categories)
      data <- select(data, FSecurity, jobs)
    }
    
    if(input$filters == "Race"){
      #these were used to try to get categories to work in ggplot
      #useLabels <- unique(e)
      #categories <- useLabels$races
      #print(categories)
      
      data <- select(data, FSecurity, races)
      
      #these were used to investigate why certain bars didn't exist
      #number <- count(data, races, FSecurity)
      #print(as_tibble(number), n=100)
    }
    
    if(input$filters == "Sex"){
      #useLabels <- unique(d)
      #categories <- useLabels$sexes
      #print(categories)
      data <- select(data, FSecurity, sexes)
    }
    
    #---------------------------------------------
    #States
    #AL-CT are all missing any entries so they just get a missing message.
    # ^no they aren't single digits was just reading in weird
    if(input$states == "AL"){
      #num <- count(State,states)
      #print(num)
      #print(State)
      #test <- count(data,states)
      #print(test)
      
      temp <- filter(data, states == "AL")
      data <- select(temp, FSecurity, states)
      
      # showModal(modalDialog(
      #   title = "Missing Data",
      #   paste0("No available data from Alabama."),
      #   easyClose = TRUE,
      #   footer = NULL
      #))
    }
    
    if(input$states == "AK"){
      # showModal(modalDialog(
      #   title = "Missing Data",
      #   paste0("No available data from Alaska."),
      #   easyClose = TRUE,
      #   footer = NULL
      # ))
      temp <- filter(data, states == "AK")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "AZ"){
      # showModal(modalDialog(
      #   title = "Missing Data",
      #   paste0("No available data from Arizona."),
      #   easyClose = TRUE,
      #   footer = NULL
      # ))
      temp <- filter(data, states == "AZ")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "AR"){
      # showModal(modalDialog(
      #   title = "Missing Data",
      #   paste0("No available data from Arkansas."),
      #   easyClose = TRUE,
      #   footer = NULL
      # ))
      temp <- filter(data, states == "AR")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "CA"){
      # showModal(modalDialog(
      #   title = "Missing Data",
      #   paste0("No available data from California."),
      #   easyClose = TRUE,
      #   footer = NULL
      # ))
      temp <- filter(data, states == "CA")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "CO"){
      # showModal(modalDialog(
      #   title = "Missing Data",
      #   paste0("No available data from Colorado."),
      #   easyClose = TRUE,
      #   footer = NULL
      # ))
      temp <- filter(data, states == "CO")
      data <- select(temp, FSecurity, states)
    }
    
    if(input$states == "CT"){
      # showModal(modalDialog(
      #   title = "Missing Data",
      #   paste0("No available data from Connecticut."),
      #   easyClose = TRUE,
      #   footer = NULL
      # ))
      temp <- filter(data, states == "CT")
      data <- select(temp, FSecurity, states)
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
    #legend strategy from: 
    # https://statisticsglobe.com/move-position-of-barplot-legend-in-r
    #axis labels strategy from:
    # https://statisticsglobe.com/rotate-axis-labels-in-r 
    #percentage creation strategy from:
    # https://r-graph-gallery.com/211-basic-grouped-or-stacked-barplot.html
    
    if (input$display == "Percentage"){
      data_percentage <- apply(table(data), 2, function(x){x*100/sum(x,na.rm=T)})
      par(mar=c(10,4,4,10), xpd=TRUE) #reset margins to make text/legend fit
      #bottom left top right
      barplot(data_percentage,#Making actual plot
              col = 3:nrow(data),#the number in front sets a pallette
              border= NA,
              las = 2, #sets angling, must be whole number
              cex.names = 0.5, #text size for axes labels
              legend.text = TRUE,
              args.legend = list(x = "right",
                                 inset = c(-0.4,0))) #positions legend 
      title(main = input$filters)
    }
    if (input$display == "Counts"){
      par(mar=c(10,4,4,10), xpd=TRUE) #reset margins to make text/legend fit
      #bottom left top right
      barplot(table(data),#Making actual plot
              col = 3:nrow(data),#the number in front sets a pallette
              border= NA,
              las = 2, #sets angling, must be whole number
              cex.names = 0.5, #text size for axes labels
              legend.text = TRUE,
              args.legend = list(x = "right",
                                 inset = c(-0.4,0))) #positions legend 
      title(main = input$filters)
    }
    
    
    #start for ggplot code, not super useful with current data structure
    #ashes_df %>% #commented out bc it
    
    # count(data,input$filters,FSecurity)  %>% 
    #   group_by(input$filters)  %>%#group by first thing?
    #   mutate(pct= prop.table(n) * 100)  %>% 
    # ggplot()+ 
    #   print (aes(x=categories, y=pct,
    #               #needs to be filter category relative to FSecurity                  
    #              fill=FSecurity)) + 
    #   geom_bar(position="fill", stat="identity")
    
    
    
    # geom_bar(stat="identity") +
    # ylab("Number of Participants") +
    # geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%")),
    #           position=position_stack(vjust=0.5)) +
    # ggtitle("England & Australia Team Make Up") +
    # theme_bw()
    
  })})

shinyApp(ui = ui, server = server)
