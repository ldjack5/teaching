# Libraries -----
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(stringr)
library(dplyr)
library(echarts4r)
library(data.table)
library(tidyverse)
library(leaflet)

# UI ------
ui <- dashboardPagePlus(
  title = "COVID-19",
  header = dashboardHeaderPlus(title = "COVID-19 BeiGene"),
  sidebar = dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Upload Data",
               tabName = "data_tab",
               icon = icon("table")
      ),
      menuItem("Plots",
               tabName = "tables_tab",
               icon = icon("chart-line")),
      menuItem("Map",
               tabName = "map_tab",
               icon = icon("globe")
      ))
  ),
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "data_tab",
              # > Drag and Drop File ------------------------------------------------------
              fileInput(
                inputId = "input_file",
                label = "Drag and drop here",
                accept = c("csv", "xlsx"),
                multiple = FALSE,
                buttonLabel = "Browse...",
                placeholder = "No file selected"
              )
              ),
      tabItem(tabName = "tables_tab",
              infoBoxOutput(outputId = "infection_rate"),
              infoBoxOutput(outputId = "death_rate"),
              infoBoxOutput(outputId = "mortality_rate"),
              fluidRow(
                box(
                  status = "primary",
                  title = NULL,
                  width = 12,
                  selectizeInput(inputId = "country", 
                                 label = "Select Countries to View",
                                 choices = NULL,
                                 selected = NULL,
                                 multiple = TRUE),

                  echarts4rOutput(outputId = "graphics_output"),
                  p("Countries without 2018 population estimates have been removed from analysis.")
                ),
                column(
                  width = 3
                )
              )
              ),
      tabItem(tabName = "map_tab")
    ))
)

# Server ----
server <- shinyServer(function(input, output, session) {
  
  # Reactive Variables ----
  reac_data <- reactiveValues(df = NULL,
                              filtered_df = NULL)
  
  # Observers -----
  # > File Input -------------
  observeEvent(input$input_file, {
    #check file extension
    file_ext <- str_split(input$input_file$name, "\\.", simplify = TRUE)[ , 2]
    #read in file
    if (file_ext == "csv") {
      data_df <- fread(input$input_file$datapath, 
                       header = TRUE,
                       sep = ",") %>%
        select(dateRep, cases, deaths, countriesAndTerritories, popData2018, countryterritoryCode)

    }else {
      data_df <- readxl::read_xlsx(input$input_file$datapath ) %>%
        select(dateRep, cases, deaths, countriesAndTerritories, popData2018, countryterritoryCode)
      
    }

    #add "All" Country to data frame
    all_countries_df <- data_df %>%
      group_by(dateRep) %>%
      summarize(cases = sum(cases),
                deaths = sum(deaths)) %>%
      ungroup() %>%
      mutate(countriesAndTerritories = "All",
             countryterritoryCode = "All")
    
    #get total population
    all_countries_df$popData2018 <- data_df %>%
      filter(!is.na(popData2018)) %>%
      group_by(countriesAndTerritories) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(popData2018 = sum(popData2018)) %>%
      select(popData2018) %>%
      distinct() %>%
      pull()
    
    #bind together to create full data frame
    reac_data$df <- rbind(data_df, all_countries_df)
    
    #activate analysis tab
    updateTabItems(session, inputId = "sidebar_menu", selected = "tables_tab")
  })
  
  # > Update Country Selector ----
  observe({
    req(reac_data$df)
    
    #create list of unique countries in data source
    country_choices <- reac_data$df %>%
      filter(!is.na(popData2018)) %>%
      select(countriesAndTerritories) %>%
      distinct() %>%
      pull()
    
    updateSelectizeInput(session,
                         inputId = "country",
                         choices = country_choices,
                         selected = "All")
  })
  
  # > Filtered Data -----
  observeEvent(input$country, {
    req(reac_data$df)

    filtered_df <- reac_data$df %>%
      filter(!is.na(popData2018) & countriesAndTerritories == input$country)

    reac_data$filtered_df <- filtered_df
  })
  
  # Renderers ------
  # > Infection Rate ------
  output$infection_rate <- renderInfoBox({

    req(reac_data$filtered_df)
    
    #get total population
    total_population <- reac_data$filtered_df %>%
      group_by(countriesAndTerritories) %>%
      slice(1) %>%
      ungroup() %>%
      select(popData2018) %>%
      distinct() %>%
      pull()
    
    #calculate percent of all people that are infected
    infect_df <- reac_data$filtered_df %>%
      group_by(countriesAndTerritories) %>%
      mutate(total_cases = sum(cases),
             last_date = max(dateRep)) %>%
      ungroup() %>%
      select(countriesAndTerritories, total_cases, last_date, countryterritoryCode) %>%
      distinct() %>%
      mutate(infect_rate = round((total_cases/total_population) * 100, 3))
    
    info_msg <- ""
    for (i in 1:nrow(infect_df)) {
      info_msg <- paste0(info_msg,
                         infect_df$countryterritoryCode[[i]],
                         ": ",
                         infect_df$infect_rate[[i]], 
                         "% as of ", 
                         infect_df$last_date[[i]], 
                         "<br/>")
    }

    infoBox(
      title = "Current Est. Infection Rate",
      value = HTML(info_msg)
    )
    
  })
  
  # > Mortality Rate -----
  output$mortality_rate <- renderInfoBox({
    
    req(reac_data$filtered_df)
    
    #get total population
    total_population <- reac_data$filtered_df %>%
      group_by(countriesAndTerritories) %>%
      slice(1) %>%
      ungroup() %>%
      select(popData2018) %>%
      distinct() %>%
      pull()
    
    #calculate percent of all people that have died
    mortality_df <- reac_data$filtered_df %>%
      group_by(countriesAndTerritories) %>%
      mutate(total_deaths = sum(deaths),
             last_date = max(dateRep)) %>%
      ungroup() %>%
      select(countriesAndTerritories, total_deaths, last_date, countryterritoryCode) %>%
      distinct() %>%
      mutate(mortality_rate = round((total_deaths/total_population) * 100, 3))
    
    info_msg <- ""
    for (i in 1:nrow(mortality_df)) {
      info_msg <- paste0(info_msg,
                         mortality_df$countryterritoryCode[[i]],
                         ": ",
                         mortality_df$mortality_rate[[i]], 
                         "% as of ", 
                         mortality_df$last_date[[i]], 
                         "<br/>")
    }
    
      infoBox(
      title = "Current Est. Mortality Rate",
      value = HTML(info_msg)
    )
    
  })

  # > Death Rate -----
  output$death_rate <- renderInfoBox({
    
    req(reac_data$filtered_df)

    #calculate percent of infected that die
    death_df <- reac_data$filtered_df %>%
      group_by(countriesAndTerritories) %>%
      mutate(total_cases = sum(cases),
             total_deaths = sum(deaths),
             last_date = max(dateRep)) %>%
      ungroup() %>%
      select(countriesAndTerritories, total_cases, total_deaths, last_date, countryterritoryCode) %>%
      distinct() %>%
      mutate(death_rate  = round((total_deaths/total_cases) * 100, 3))

    info_msg <- ""
    for (i in 1:nrow(death_df)) {
      info_msg <- paste0(info_msg,
                         death_df$countryterritoryCode[[i]],
                         ": ",
                         death_df$death_rate[[i]], 
                         "% as of ", 
                         death_df$last_date[[i]], 
                         "<br/>")
    }

    infoBox(
      title = "Current Est. Death Rate",
      value = HTML(info_msg)
    )
    
  })
  # > Plot Output ------------
  output$graphics_output <- renderEcharts4r({
   req(reac_data$filtered_df)

    data_df <- reac_data$filtered_df %>%
      group_by(countriesAndTerritories, dateRep) %>%
      summarize(cases = sum(cases),
                deaths = sum(deaths)) %>%
      ungroup() %>%
      arrange(countriesAndTerritories, dateRep) %>%
      group_by(countriesAndTerritories) %>%
      mutate(total_cases = cumsum(cases),
             total_deaths = cumsum(deaths)) %>%
      ungroup() %>%
      select(Country = countriesAndTerritories, Date = dateRep, Cases = total_cases, Deaths = total_deaths)

    data_df %>%
      # group_by(Type) %>%
      group_by(Country) %>%
      e_charts(Date) %>%
      e_line(Cases, showSymbol = FALSE) %>%
      e_line(Deaths, showSymbol = FALSE, lineStyle = list(type = "dashed")) %>%
      e_tooltip(trigger = "axis") %>%
      e_datazoom() %>%
      e_grid(bottom = "20%") %>%
      e_title("Current Trajectory", "Cases & Deaths") %>%
      e_x_axis(name = "Date", nameLocation = "center", nameGap = 30,
               axisLabel = list(formatter = JS("function formatDate(date) {
        var monthNames = ['Jan','Feb','Mar','Apr','May','Jun', 'Jul','Aug','Sep','Oct','Nov','Dec'];
        var date = new Date(date);
        var day = date.getDate();
        var monthIndex = date.getMonth();
        var year = date.getFullYear();
        return day + '' + monthNames[monthIndex] + ' ' + year;}"))) %>%
      e_y_axis(name = "Cumulative Sum", nameLocation = "center", nameGap = 30, nameTextStyle = list(padding = list(0, 0, 25, 0))) %>%
      e_legend(show = TRUE, top = 35, right = 0, type = "scroll") %>%
      e_toolbox() %>%
      e_toolbox_feature(feature = "saveAsImage") %>%
      e_show_loading()
  })
  
  })

shinyApp(ui = ui, server = server)
