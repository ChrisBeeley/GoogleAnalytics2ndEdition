
library(dplyr)
library(ggplot2)
library(rgdal)
library(RColorBrewer)
library(ggvis)
library(shiny)
library(DT)
library(rmarkdown)
library(knitr)

load("gadf.Rdata")

options(shiny.launch.browser = TRUE)

shinyServer(function(input, output, session) {
  
  # reactive data
  
  passData <- reactive({
    
    firstData <- filter(gadf, date >= input$dateRange[1] & date <= input$dateRange[2])
    
    if(!is.null(input$domainShow)){
      
      firstData <- filter(firstData, networkDomain %in% input$domainShow)
      
    }
    
    return(firstData)
    
  })
  
  # parse the query string
  
  observe({
    
    searchString <- parseQueryString(session$clientData$url_search)
    
    # update inputs according to query string
    
    if(length(searchString) > 0){ # if the searchString exists
      
      # deal with first query which indicates the audience
      
      if(searchString[[1]] == "nhs"){ # for NHS users do the following
        
        updateCheckboxGroupInput(session, "domainShow",
                                 choices = list("NHS users" = "nhs.uk",
                                                "Other" = "Other"),
                                 selected = c("nhs.uk")
        )
        
      }
      
      # do they want a smooth?
      
      if(searchString[[2]] == "yes"){
        
        updateTabsetPanel(session, "theTabs", selected = "trend")
        
        updateCheckboxInput(session, inputId = "smooth",
                            value = TRUE)
        
      }
    }
  })
  
  # text summary
  
  output$textDisplay <- renderText({
    paste(
      length(seq.Date(input$dateRange[1], input$dateRange[2], by = "days")),
      " days are summarised. There were", sum(passData()$users),
      "users in this time period."
    )
  })
  
  # place graph in function for call by Report.Rmd file
  
  trendGraph = reactive({
    
    if(!is.null(input$theCountries)){
      
      graphData <- filter(passData(), country %in% input$theCountries)
      
    } else {
      
      graphData <- passData()
    }
    
    groupByDate <- group_by(graphData, YearMonth, networkDomain) %>%
      summarise(meanSession = mean(sessionDuration, na.rm = TRUE), 
                users = sum(users),
                newUsers = sum(newUsers), sessions = sum(sessions))
    
    groupByDate$Date <- as.Date(paste0(groupByDate$YearMonth, "01"), format = "%Y%m%d")
    
    thePlot <- ggplot(groupByDate, 
                      aes_string(x = "Date", y = input$outputRequired, 
                                 group = "networkDomain", colour = "networkDomain")) +
      geom_line()
    
    if(input$smooth){
      
      thePlot <- thePlot + geom_smooth()
    }
    
    print(thePlot)
    
  })
  
  # print the graph
  
  output$trend <- renderPlot({
    
    trendGraph()
  })
  
  output$animated <- renderPlot({
    
    groupByDate <- group_by(passData(), YearMonth, networkDomain) %>%
      summarise(meanSession = mean(sessionDuration, na.rm = TRUE), 
                users = sum(users),
                newUsers = sum(newUsers), sessions = sum(sessions))
    
    groupByDate$Date <- as.Date(paste0(groupByDate$YearMonth, "01"),
                                format = "%Y%m%d")
    
    smoothData <- groupByDate[groupByDate$Date %in%
                                quantile(groupByDate$Date,
                                         input$animation / 100,
                                         type = 1):
                                quantile(groupByDate$Date,
                                         (input$animation + 20) / 100,
                                         type = 1), ]
    
    ggplot(groupByDate, 
           aes_string(x = "Date",
                      y = input$outputRequired, 
                      group = "networkDomain",
                      colour = "networkDomain")) +
      geom_line() +
      geom_smooth(data = smoothData,
                  method = "lm",
                  colour = "black")
    
  })
  
  
  # produce table https://rstudio.github.io/DT/
  
  output$countryTable <- DT::renderDataTable ({
    
    groupCountry <- group_by(passData(), country)
    
    groupByCountry <- summarise(groupCountry,
                                meanSession = mean(sessionDuration), 
                                users = log(sum(users)),
                                sessions = log(sum(sessions)))
    
    datatable(groupByCountry)
    
  })
  
  # filter by country
  
  output$reactCountries <- renderUI({
    
    countryList = unique(as.character(passData()$country))
    
    selectInput("theCountries", "Choose country", countryList,
                multiple = TRUE)
    
  })
  
  # control the checkboxes when the map is selected
  
  observe({
    
    if(input$theTabs == "map"){
      
      updateCheckboxGroupInput(session, "domainShow",
                               choices = list("NHS users" = "nhs.uk",
                                              "Other" = "Other"),
                               selected = c("nhs.uk", "Other")
      )
      
    }
  })
  
  # produce map
  
  output$ggplotMap <- renderPlot ({
    
    input$drawMap # dependency on actionButton
    
    withProgress(message = 'Please wait',
                 detail = 'Drawing map...', value = 0, {
                   
                   if(length(unique(as.character(isolate(passData()$country)))) < 2){
                     
                     return()
                   }
                   
                   groupCountry <- isolate( # avoid dependency on data
                     
                     group_by(passData(), country)
                   )
                   
                   groupByCountry <- summarise(groupCountry,
                                               meanSession = mean(sessionDuration), 
                                               users = log(sum(users)),
                                               sessions = log(sum(sessions)))
                   
                   world <- readOGR(dsn = ".",
                                    layer = "world_country_admin_boundary_shapefile_with_fips_codes")
                   
                   incProgress(1/3)
                   
                   countries <- world@data
                   
                   countries <- cbind(id = rownames(countries),
                                      countries)
                   
                   countries <- merge(countries, groupByCountry,
                                      by.x = "CNTRY_NAME",
                                      by.y = "country", all.x = TRUE)
                   
                   map.df <- fortify(world)
                   
                   incProgress(1/3)
                   
                   map.df <- merge(map.df, countries, by = "id")
                   
                   checkInputs <- "No"
                   
                   incProgress(1/3)
                   
                   ggplot(map.df, aes(x = long, y = lat, group = group)) +
                     geom_polygon(aes_string(fill = input$outputRequired)) +
                     geom_path(colour = "grey50") +
                     scale_fill_gradientn(colours = rev(brewer.pal(9, "Spectral")),
                                          na.value = "white") +
                     coord_fixed() + labs(x = "", y = "")
                   
                 })
  })
  
  # download report
  
  output$downloadDoc <-
    
    downloadHandler(filename = "Report.html",
                    content = function(file){
                      
                      knit2html("Report.Rmd", envir = environment())
                      
                      # copy document to 'file'
                      
                      file.copy("Report.html", file, overwrite = TRUE)
                      
                    }
                    
    )
  
  # download graphics
  
  output$downloadData.trend <- downloadHandler(
    
    filename <- function() {
    
      paste("Trend_plot", Sys.Date(),".png",sep="")},
    
      content <- function(file) {
        png(file, width = 980, height = 400,
            units = "px", pointsize = 12,
            bg = "white", res = NA)
        
        trend.plot <- trendGraph()
        
        print(trend.plot)
        
        dev.off()}
  )
  
})