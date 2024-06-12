

# Packages ----
library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(agridat)  # The package where the data comes from

# Data ----
Barley <- as.data.frame(beaven.barley)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel(title = "Barley Yield"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(inputId = "gen",
                  label = "1. Select genotype",
                  choices = c("A" = "a", "B" = "b", "C" = "c", "D" = "d",
                              "E" = "e","F" = "f","G" = "g","H" = "h"),
                  selected = "a"),
      
      selectInput(inputId = "color",
                  label = "2. Select color",
                  choices = c("blue","green","red","purple","grey"),
                  selected = "green"),
      
      sliderInput(inputId = "bin",
                  label = "3. Select number of histogram bins",
                  min = 1, max = 25, value = c(10)),
      
      textInput(inputId = "text",
                label = "4. Enter some text to be displayed", 
                value = "",
                placeholder = "abc")
      
    ),
    
    mainPanel(
      plotOutput(outputId = "myplot"),
      tags$br(),
      textOutput(outputId = "mytext"),
      tags$br(),
      tableOutput(outputId = "mytable")
    )
    
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$myplot <- renderPlot(ggplot(Barley, aes(x=yield)) +
                              
                              geom_histogram(bins = input$bin,
                                             fill = input$color,
                                             color = "black",
                                             data = Barley[Barley$gen == input$gen,],
                                             group = input$gen)
                            
                            ) 
  
  output$mytext <- renderText(input$text)
  
  output$mytable <- renderTable(Barley %>%
                                  filter(gen == input$gen) %>%
                                  summarise("Mean" = mean(yield), 
                                            "Median" = median(yield),
                                            "STDEV" = sd(yield), 
                                            "Min" = min(yield),
                                            "Max" = max(yield)))
}

# Run the application 
shinyApp(ui = ui, server = server)
