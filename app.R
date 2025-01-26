# an example of app.R
library(tidyverse)
library(shiny)

source("data_exploration.R")

ui <- fluidPage(
  tags$style(HTML("
    #text {
      font-size: 24px;
      font-weight: bold;
      color: #333;
    }
  ")),
  
  # Application title
  titlePanel("Global Health Trends"),
  
  # Sidebar with a slider input widget
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "country2_id",
        label = "Select first country",
        choices = unique(who_data_clean$country),
        selected = "Germany",
        multiple = FALSE
      ),
      selectInput(
        inputId = "country_id",
        label = "Select second country",
        choices = unique(who_data_clean$country),
        selected = "Ghana",
        multiple = FALSE
      ),
      selectInput(
        inputId = "indicator",
        label = "select indicator",
        choices = c("life_expect", "maternal_mortality", "neonatal_mortality",
                    "infant_mortality", "under_5_mortality"),
        selected = "life_expect",
        multiple = F
      ),
      
      checkboxInput(inputId = "summ_stats", 
                    label = "Show summary statistics", 
                    value = FALSE, 
                    width = NULL),
      
      checkboxInput(inputId = "show_data", 
                    label = "See dataframe", 
                    value = FALSE, 
                    width = NULL),
      
      # sliderInput("input_1", min = 1,value = 30, max = 50, label = "life expectancy"),
      
    ),
    
    # Show a plot 
    mainPanel(
      textOutput("text", container = span),
      plotOutput("my_plot"),
      tableOutput("summary_table"),
      tableOutput("filter_data"),
      
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data <- who_data
  
  countries <-reactive({c(input$country_id,input$country2_id)})
  
  filter_data <- reactive({data %>% filter(country %in% countries())})


  plot_1 <- reactive({
    ggplot(filter_data(), aes(x = year, color = country)) +
      geom_line(aes_string(y = input$indicator)) +
      geom_vline(xintercept = 2019, linetype = "dashed", color = "black", size = 0.5) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
      scale_x_continuous(breaks = seq(1999, 2023, by = 2)) +
      scale_color_manual(values=c("darkblue", "#E69F00"))
      #labs(title = "Life expectancy per country over time",) 

  })
  
  txt <- reactive({paste("visualisation of", input$indicator, "over time in", input$country2_id, "and", input$country_id)})
  
  # Correlation between key indicators
  d <- reactive({
    tibble(
      cor_hiv_mal = cor.test(filter_data()$prev_hiv, filter_data()$prev_undernourishment)$estimate,
      cor_heal_life = cor.test(filter_data()$health_exp, filter_data()$life_expect)$estimate,
    )
  })

  output$my_plot <- renderPlot({
    plot_1()
  })
  
  output$summary_table <- renderTable({
    if (input$summ_stats == TRUE){
    d()
    }
  })
  
  output$text <- renderText({
    txt()
  })
  
  output$filter_data <- renderTable({
    if (input$show_data == TRUE){
      filter_data()
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)