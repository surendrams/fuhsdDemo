library(tidyverse)
library(gapminder)
library(dplyr)
library(shiny)
library(shinythemes)

minYear <- min(gapminder["year"])
maxYear <- max(gapminder["year"])

ui <- fluidPage(
  theme = shinythemes::shinytheme('cerulean'),
  title = "FUHSD R Class Project",
  
  navbarPage(
    "Shiny and Gapminder Example",
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "year", label = "Choose Year", 
                    min = minYear, max = maxYear, value = minYear,step = 1),
        radioButtons(inputId = "continent", label = "Continent", 
                     choiceNames = list("All","Asia","Europe","Africa","Americas","Oceania"), 
                     choiceValues= list("All","Asia","Europe","Africa","Americas","Oceania"), 
                     inline = FALSE)
      ),
      mainPanel(
        plotOutput(outputId = "plot")
      )
    )
  )
)

server <- function(input, output, session) {
  ds <- reactive({
    if (input$continent == "All") {
      gapminder %>% filter(year == input$year)
    } else {
      gapminder %>% filter(year == input$year, continent == input$continent)
    }
    
  })
  
  
  output$plot <- renderPlot({
    
    p <- ggplot(ds(), aes(x = pop, y = lifeExp, color = continent, size = gdpPercap )) + geom_point() 
    p <- p + scale_x_log10()
    print(p)
    
  })
  
}

shinyApp(ui, server)
