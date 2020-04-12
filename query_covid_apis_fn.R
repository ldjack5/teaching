library(httr)
library(jsonlite)
library(purrr)
library(dplyr)
#https://documenter.getpostman.com/view/8854915/SzS8rjHv?version=latest State Level Data
#https://documenter.getpostman.com/view/10808728/SzS8rjbc?version=latest Contry Level Data
#https://covid-19-apis.postman.com/ Many more APIs associated with COVID

query_countries_fn <- function(country_lst){
  
  return_lst <- list(data_df = NULL,
                     error_msg = NULL,
                     status_code = NULL)
  
  query_country_fn <- function(country, status){
    
    return_lst <- list(data_df = NULL,
                       status_code = NULL,
                       error_msg = NULL)

    #set a GET request
    covid_country_req <- GET(url = paste0("https://api.covid19api.com/country/", country, "/status/", status))
    
    return_lst$status_code <- covid_country_req$status_code
    
    #"good" request when status = 200. 
    if (return_lst$status_code == 200) {
      #get content of request
      data_df <- jsonlite::fromJSON(content(covid_country_req, as = "text"))
      
      #"good" means correct format of request, but might still be issue with query itself
      #if issue with query, return error message
      if ("error" %in% names(return_lst$data_df)) {
        return_lst$error_msg <- return_lst$data_df$message
        return_lst$data_df <- NULL
        
      }else {
        return_lst$data_df <- data_df
      }
    }
    
    return(return_lst)
  }

  #status possibilities are confirmed, deaths, recovered
  for (country in country_lst) {
    confirmed_lst <- query_country_fn(country, status = "confirmed")
    death_lst <- query_country_fn(country, status = "deaths")
    
    status_code_lst <- list(confirmed = confirmed_lst$status_code,
                                   deaths = death_lst$status_code)
    
    error_msg_lst <- list(confirmed = confirmed_lst$error_msg,
                                 deaths = death_lst$error_msg)
    
    #only return data if no errors and no status code other than 200
    if (!all(is.null(error_msg_lst))) {
      if (all(status_code_lst == 200)) {
        #account for possibility of no data
        if (length(confirmed_lst$data_df) != 0) {
          if (length(death_lst$data_df != 0)) {
            bind_df <- confirmed_lst$data_df %>%
              full_join(death_lst$data_df)
            
          }else {
            bind_df <- confirmed_lst$data_df
            
          }
        }else {
          if (length(death_lst$data_df != 0)) {
            bind_df <- death_lst$data_df
          }else {
            bind_df <- NULL
          }
        }
        
        #bind all of the rows if there's anything to bind
        if (!is.null(bind_df)) {
          return_lst$data_df <- return_lst$data_df %>%
            bind_rows(bind_df)
          }
        }
    }
    
    #concatenate error messages and status coddes
    return_lst$error_msg <- c(return_lst$error_msg, error_msg_lst) 
    return_lst$status_code <- c(return_lst$status_code, status_code_lst) 

  }
  
  return(return_lst)
  
}

query_states_fn <- function(state_lst){
  
  query_state_fn <- function(state){
    
    return_lst <- list(data_df = NULL,
                       status_code = NULL,
                       error_msg = NULL)
    
    #set a GET request
    covid_states_req <- GET(url = "https://covidtracking.com/api/states/daily", query = list("state" = state))
    
    return_lst$status_code <- covid_states_req$status_code
    
    #"good" request when status = 200. 
    if (return_lst$status_code == 200) {
      #get content of request
      data_df <- jsonlite::fromJSON(content(covid_states_req, as = "text"))
      
      #"good" means correct format of request, but might still be issue with query itself
      #if issue with query, return error message
      if ("error" %in% names(return_lst$data_df)) {
        return_lst$error_msg <- return_lst$data_df$message
        return_lst$data_df <- NULL
        
      }else {
        return_lst$data_df <- data_df
      }
    }
    
    return(return_lst)
  }
  return_lst <- map(.x = state_lst, .f = query_state_fn)
  
  return(return_lst)

}

get_states_fn <- function(){
  
  #initialize values to return
  return_lst <- list(state_lst = NULL,
                     status_code = NULL,
                     error_msg = NULL)
  #set a GET request
  covid_states_req <- GET(url = "https://covidtracking.com/api/states")
  
  return_lst$status_code <- covid_states_req$status_code 
  
  #"good" request when status = 200. 
  if (return_lst$status_code == 200) {
    #get content of request
    data_df <- jsonlite::fromJSON(content(covid_states_req, as = "text"))
    
    #"good" means correct format of request, but might still be issue with query itself
    #if issue with query, return error message
    if ("error" %in% names(data_df)) {
      return_lst$error_msg <- data_df$message
      
    }else {
      #pull the states from the returned data fram
      return_lst$state_lst <- data_df %>%
        select(state) %>%
        arrange(state) %>%
        pull()
    }
  }
  
  return(return_lst)
}

get_countries_fn <- function(){
  
  #initialize values to return
  return_lst <- list(country_lst = NULL,
                     status_code = NULL,
                     error_msg = NULL)
  #set a GET request
  covid_countries_req <- GET(url = "https://api.covid19api.com/countries")
  
  return_lst$status_code <- covid_countries_req$status_code 
  
  #"good" request when status = 200. 
  if (return_lst$status_code == 200) {
    # ADD PAGINATION TO GET ALL RESULTS
    
    #get content of request
    data_df <- jsonlite::fromJSON(content(covid_countries_req, as = "text"))
    
    #"good" means correct format of request, but might still be issue with query itself
    #if issue with query, return error message
    if ("error" %in% names(data_df)) {
      return_lst$error_msg <- data_df$message
      
    }else {
      #pull the states from the returned data fram
      return_lst$country_lst <- data_df %>%
        select(Slug) %>%
        arrange(Slug) %>%
        pull()
    }
  }
  
  return(return_lst)
}





