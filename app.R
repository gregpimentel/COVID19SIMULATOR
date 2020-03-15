library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(shiny)
library(leaflet.extras)
library(tidyverse)

options(scipen = 999)

#read in project data for all states 
allState <- read.csv('data/allState.csv') 

ui <-  navbarPage(
  "COVID-19 USA Simulator",
  id = "nav",
  tabPanel(
    "Interactive map",
    div(
      class = "outer",
      tags$head(# Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")),
      #map
      leafletOutput("map", width = "100%", height ="100%"),
      absolutePanel(
        id = "display",
        class = "panel panel-default",
        fixed = TRUE,
        draggable = TRUE,
        top = 60,
        left = "auto",
        right = 20,
        bottom = "auto",
        width = 330,
        height = "auto",
      #  h2("Predicted Infections"),
        tags$div(HTML("<center><h2>Predicted Infections</h2></center>")),
        wellPanel(uiOutput("totalSick")) ,
        wellPanel(uiOutput("totalDead")) ,
      tags$div(HTML("<center><h5>Predicted Infections:</h5></center>")),
        plotOutput("totalInfections")
      ), 
      absolutePanel(
        id = "slider",
        class = "panel panel-default",
        fixed = TRUE,
        draggable = TRUE,
        top = "auto",
        left = 20,
        right = "auto",
        bottom = 10,
        width = 330,
        height = "auto",
       # h5("Select Date:"),
        tags$div(HTML("<center><h5>Move slider to advance time:</h5></center>")),
         sliderInput(
          "days",
      label = NULL, #   "Select Days From Today:",
          min = Sys.Date(),
          max = Sys.Date() + 300,
          value = Sys.Date(),
          timeFormat = "%Y-%m-%d"
        ),
      )
    )
  ),
  tabPanel("About Page",
           fluidRow(
             column(width = 8,
           includeMarkdown("covid-19_aboutpage.rmd")
           )
        )
  )
)


server <- function(input, output, session) {
  ## Interactive Map ###########################################
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      setView(lng = -83.85,
              lat = 37.45,
              zoom = 4)
  })
  
  output$totalInfections <-renderPlot(width = "auto", height = 220,{

    allState %>% 
      filter(time <= 150) %>% 
      mutate(time_dt = Sys.Date() + time ) %>% 
      group_by(time_dt) %>% 
      summarize(current_sick = round(sum(Y)) ) %>%
      ggplot(aes(x=time_dt,y=current_sick))  +
      geom_area( fill = "red") +
      scale_y_continuous(name="Projected Cases",labels = comma)+ theme_classic() + 
      xlab(" ") + ylab(" ")
      
    
  
  })
  
  # Precalculate the breaks we'll need for the two histograms
  #centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks
  output$totalSick <- renderUI({
    timeSelect = ifelse(is.null(input$days), 0, as.numeric(input$days - Sys.Date()))
    selectedState <- allState %>%
      filter(time ==  timeSelect & !is.na(Y)) %>%
      summarize(current_sick_PCT = round(sum(Y)))
    #number of total sick
    tagList(
      tags$h5("Current sick as of ", as.character(format(
        input$days, "%m/%d/%y"
      )), ":", align = "center"),
      tags$h1(tags$span(
        style = "color:red", tags$span(
          style = "font-size: 50px",
          format(
            selectedState$current_sick,
            scientific = FALSE,
            big.mark = ","
          )
        )
      ), align = "center")
    )
    
  })
  
  output$totalDead <- renderUI({
    timeSelect = ifelse(is.null(input$days), 0, as.numeric(input$days - Sys.Date()))
    selectedState <- allState %>%
      filter(time ==  timeSelect & !is.na(Y)) %>%
      filter(!is.na(total_dead)) %>%
      summarize(total_dead = round(sum(total_dead)))
    
    #number of total dead
    tagList(
      tags$h5("Total dead as of ", as.character(format(
        input$days, "%m/%d/%y"
      )), ":", align = "center"),
      tags$h1(tags$span(
        style = "color:red", tags$span(
          style = "font-size: 50px",
          format(
            selectedState$total_dead,
            scientific = FALSE,
            big.mark = ","
          )
        )
      ), align = "center")
    )
    
  })
  
  
  # This observer is responsible for maintaining the circles and map
  observe({
    OneHundredDays <- allState %>%
      filter(time ==  as.numeric(input$days - Sys.Date()) &
               !is.na(Y))
    
    radius <-
      ((OneHundredDays$Y) / max(OneHundredDays$Y)) * 60000 * log(max(OneHundredDays$Y))
    
    colorData <- OneHundredDays$Y
    
    pal <-
      colorBin(
        palette = c('#fc9272', '#fb6a4a', '#ef3b2c', '#cb181d', '#a50f15'),
        domain = colorData,
        5,
        pretty = TRUE
      )
    
    leafletProxy("map", data = OneHundredDays) %>%
      clearShapes() %>%
      addCircles(
        ~ longitude,
        ~ latitude,
        radius = radius,
        layerId =  ~ Province.State,
        stroke = FALSE,
        fillOpacity = 0.6,
        fillColor = pal(colorData)
      )
    
  })
  
  # Show a popup at the given location
  showCountyPopup <- function(Province.State, lat, lng) {
    timeSelect = ifelse(is.null(input$days), 0, as.numeric(input$days - Sys.Date()))
    selectedState <- allState %>%
      filter(time ==  timeSelect & !is.na(Y))
    
    selectedState <- selectedState[selectedState$Province.State == Province.State, ]
    
    content <- as.character(tagList(
      tags$h4("State: ", selectedState$Province.State),
      tags$h4("As of : ", input$days),
      tags$h4(
        "Current Sick: ",
        format(
          round(selectedState$Y),
          scientific = FALSE,
          big.mark = ","
        )
      ) ,
      tags$h4(
        "Total Dead: ",
        format(
          round(selectedState$total_dead),
          scientific = FALSE,
          big.mark = ","
        )
      )
    ))
    
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = Province.State)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showCountyPopup(event$id, event$lat, event$lng)
    })
  })
  
  
  ## About Page ###########################################
  
  
}


# Run the application
shinyApp(ui = ui, server = server)