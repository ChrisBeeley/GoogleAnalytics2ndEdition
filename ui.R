library(shiny)

shinyUI(fluidPage( # flexible UI setup
  
  # Application title
  titlePanel("Google Analytics"),
  
  sidebarLayout( # simple setup, controls on left, output on right
    
    sidebarPanel( # sidebar layout
      
      dateRangeInput(inputId = "dateRange", label = "Date range", 
                     start = "2013-05-01"), # select date
      
      checkboxGroupInput(inputId = "domainShow", # select network domain
                         label = "Show NHS/ other domain?",
                         choices = list("NHS users" = "nhs.uk",
                                        "Other" = "Other"),
                         selected = c("nhs.uk", "Other")
      ),
      
      hr(),
      
      radioButtons(inputId = "outputRequired", label = "Output required", 
                   choices = list("Average session" = "meanSession", 
                                  "Users" = "users", "Sessions" = "sessions")),
      
      uiOutput("reactCountries"),
      
      conditionalPanel(
        condition = "input.theTabs == 'trend'",
        checkboxInput("smooth", label = "Add smoother?", # add smoother 
                      value = FALSE)
      ),
      
      conditionalPanel(
        condition = "input.theTabs == 'animated'",
        sliderInput("animation", "Trend over time",
                    min = 0, max = 80, value = 0, step = 5,
                    animate = animationOptions(interval = 1000, loop = TRUE))
      ),
      
      hr(),
      
      actionButton("drawMap", "Update map"),
      
      # download custom report
      
      downloadButton("downloadDoc", "Download report")
      
    ),
    
    mainPanel(     # main panel section
      tabsetPanel(id = "theTabs", # give tabsetPanel a name
                  tabPanel("Summary", textOutput("textDisplay"),
                           value = "summary"),
                  tabPanel("Trend", plotOutput("trend"),
                           downloadButton("downloadData.trend", "Download graphic"),
                           value = "trend"),
                  tabPanel("Animated", plotOutput("animated"),
                           value = "animated"),
                  tabPanel("Map", plotOutput("ggplotMap"),
                           value = "map"),
                  tabPanel("Table", DT::dataTableOutput("countryTable"),
                           value = "table")
      )
    )
  )
))