library(shiny)
library(tidyverse)
library(plotly)
library(DT)
theme_set(theme_classic())
data_long <- read_csv("data/gapminder_life_expectancy_years_parsed.csv")

all_countries <- unique(data_long$country)

ui <- fluidPage(
  titlePanel("Life Expectancy Shiny App"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId="year_range", label="Year range", value=c(1900, 2020), min=1800, max=2030, step=10),
      selectInput(inputId="countries", label="Countries", choices = all_countries, multiple = TRUE, selected=c("Sweden", "Denmark", "Norway", "Iceland", "Finland"))
    ),
    mainPanel(
      plotlyOutput(outputId="year_range_plot"),
      plotlyOutput(outputId="year_range_boxplot"),
      DTOutput(outputId="table_display")
    )
  )
)
server <- function(input, output) {
  
  data_long_filtered <- reactive({
    data_long %>% 
      filter(country %in% input$countries) %>%
      filter(year >= input$year_range[1] & year <= input$year_range[2])
  })
  
  output$year_range_plot <- renderPlotly({
    
    plt <- ggplot(data_long_filtered(), aes(x=year, y=life_expectancy, color=country)) +
      geom_line()
    ggplotly(plt) %>% layout(xaxis=list(title="Year"), yaxis=list(title="Life expectancy (years)"))
  })
  
  output$year_range_boxplot <- renderPlotly({
    plt <- ggplot(data_long_filtered(), aes(x=country, y=life_expectancy, fill=country, text=year)) + 
      geom_boxplot()
    ggplotly(plt, tooltip="text") %>% layout(xaxis=list(title="Country"), yaxis=list(title="Life expectancy (years)"))
  })
  
  output$table_display <- DT::renderDataTable({
    data_long_filtered() %>% pivot_wider(names_from="year", values_from="life_expectancy")
  })
}
shinyApp(ui=ui, server=server)