# Libraries ----
library(shiny)
library(ggplot2)
library(tidyr)

# Source ----
# This file contains 
# 1) query_states_fn, which takes a vector of US state postal codes
# 2) query_countries_fn, which takes a vector of country names as they appear in COVID19-Tracker API
# 3) get_states_fn, which takes no input, and returns a list of all valid state postal codes from COVID tracking project API
# 3) get_countries_fn, which takes no input, and returns a list of all valid country names from COVID-Tracker API

source("/ci/ljackman/covid_test/query_covid_apis_fn.R")

#pull list of states to populate select input
states_lst <- get_states_fn()

#check no errors
if (is.null(states_lst$error_msg)) {
  state_selections <- states_lst$state_lst
  
}else {
  state_selections <- states_lst$error_msg
}

#pull list of countries to populate select input
countries_lst <- get_countries_fn()

#check no errors
if (is.null(countries_lst$error_msg)) {
  country_selections <- countries_lst$country_lst
  
}else {
  country_selections <- countries_lst$error_msg
}

# UI ----
ui <- fluidPage(
  fluidRow(
    column(
      width = 3,
      selectInput(inputId = "country_select",
                  label = "Select Country",
                  choices = country_selections,
                  selected = NULL),
      uiOutput(outputId = "show_hide_state_selector")

    ),
    column(
      width = 9,
      plotOutput(outputId = "covid_plot")
    ),
    fluidRow(
      column(
        width = 12,
        textOutput(outputId = "death_rate")
      )
    )
  )
)

# Server ----
server <- function(input, output){
  
  # Reactives ----
  #processed data. should have report_date, infection_num, death_num
  reac_data <- reactiveValues(df = NULL)
  
  # Observers ----
  # > Country Selector ----
  observeEvent(input$country_select, {
    if (!input$country_select == "united-states") {
      req_data <- query_countries_fn(input$country_select)
      
      reac_data$df <- req_data$data_df %>%
        select(Date, Status, Cases) %>%
        mutate(Date = as.Date(Date))
    }
  })
  
  # > State Selector ----
  observeEvent(input$state_select, {
    if (input$country_select == "united-states") {
      req_data <- query_states_fn(input$state_select)
      
      reac_data$df <- req_data[[1]]$data_df %>%
        select(Date = date, positive, death) %>%
        gather(key = "Status", "value" = Cases, positive, death) %>%
        mutate(Status = case_when(Status == "positive" ~ "confirmed",
                                  Status == "death" ~ "deaths"),
               Date = lubridate::ymd(Date),
               ) %>%
        replace_na(list(Cases = 0))
        
      
    }

  })
  
  # Renderers ----
  output$covid_plot <- renderPlot({

    reac_data$df %>%
      ggplot(aes(x = Date, y = Cases, group = Status)) +
      geom_line(aes(linetype = Status))
    
    
  })
  
  output$death_rate <- renderText({
    max_dead <- reac_data$df %>%
      filter(Status == "deaths") %>%
      select(Cases) %>%
      max()
    
    max_infected <- reac_data$df %>%
      filter(Status == "confirmed") %>%
      select(Cases) %>%
      max()
    
    dead_percent <- round(max_dead/max_infected * 100, 2)

    
    text_output <- paste0("Death Rate: ", dead_percent, "%")
    
    text_output
  })
  
  output$show_hide_state_selector <- renderUI({
    if (input$country_select == "united-states") {
      ui <- selectInput(inputId = "state_select",
                  label = "Select State",
                  choices = state_selections,
                  selected = NULL)
    }else {
      ui <- p()
    }
    ui
    
  })

}

shinyApp(ui, server)
