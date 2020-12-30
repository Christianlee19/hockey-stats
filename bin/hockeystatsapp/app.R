#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

## load data
df = read.csv("~/Documents/hockey-stats/data/basic_stats_2020.csv",
                  header=T, stringsAsFactors = F, skip = 1)

## get team drop downs
team_list = unique(df$Tm)
team_list = team_list[order(team_list)]
team_list = team_list[team_list != "TOT"]


## ---- UI ---- ##
ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Hockey Stats"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    selectInput("variable", 
                label = "NHL Team:",
                choices = team_list)
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    h3(textOutput("selected_var"))
    
    #plotOutput("teamPlot")
    
    
  )
)



## ---- Server ---- ##
server <- function(input, output) {
  
  # Subset data
  #######df2 = input[input$Tm == ,]
  
  formulaText <- reactive({
    paste("mpg ~", input$variable)
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  
  # output$teamPlot <- renderPlot({
  #   
  #   ggplot(df2, aes(x=G, y=A)) +
  #     geom_point()
  # })
  
}

## test
mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))
server <- function(input, output) {
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste("mpg ~", input$variable)
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  output$mpgPlot <- renderPlot({
    boxplot(as.formula(formulaText()),
            data = mpgData,
            outline = input$outliers,
            col = "#75AADB", pch = 19)
  })
  
}

# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Miles Per Gallon"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for variable to plot against mpg ----
      selectInput("variable", "Variable:",
                  c("Cylinders" = "cyl",
                    "Transmission" = "am",
                    "Gears" = "gear")),
      
      # Input: Checkbox for whether outliers should be included ----
      checkboxInput("outliers", "Show outliers", TRUE)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against mpg ----
      plotOutput("mpgPlot")
      
    )
  )
)
## end test






# Run the application 
shinyApp(ui = ui, server = server)

