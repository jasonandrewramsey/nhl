# !diagnostics off

# packages & dependencies -------------------------------------------------

library(tidyverse)
library(rvest)
library(lubridate)


# site navigation functions -----------------------------------------------
# sub function to structure the date into a query string
structure.query.url <- function(date) {
  require(lubridate)
  
  if(missing(date)) { 
    print('Provide a date to to extract the boxscore URLs') 
    return()
  }
  
  if(class(date) != 'Date') { 
    date <-
      tryCatch({
        as.Date(date)
      }, error = function(e) {
      })
  }
  
  if(is.null(date)) {
    print('Provide a date or value coercible to a date')
    return()
  }

  cat(paste0('Date: ', date, '\n'))
  return(paste0('https://www.hockey-reference.com/boxscores/index.fcgi?month=', month(date), '&day=', mday(date), '&year=', year(date)))
}

# sub function to extract the game urls from the query url
sub.extract.urls <- function(url) {
  require(rvest)
  require(stringr)
  
  # the query url that is generated will have many urls contained on it (ads/other site links/) but the criteria
  # for our target are those containing '/boxscore/' and will have a 8 or higher number of digits (date)
  # ex: /boxscores/202103080ANA.html
  boxscore_urls <- 
    url %>%
    read_html() %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    as.data.frame %>%
    setNames('game_url') %>%
    filter(str_detect(game_url, '[/]boxscores[/][0-9]{6,}')) %>%
    .$game_url %>%
    paste
  
  num_of_boxscores <- length(boxscore_urls)
  
  cat(paste0('Games: ', num_of_boxscores, '\n'))
  
  if(num_of_boxscores > 0) {
    return(paste0('https://www.hockey-reference.com', boxscore_urls))
  }
}

# function to extract all of the boxscore urls on hockey-reference for a specific date
extract.boxscore.urls <- function(date) {
  # website https://www.hockey-reference.com/
  # specific branch /boxscores/
  url <- structure.query.url(date)
  
  if(is.null(url)) { return() }
  
  return(
    list(
      GAME_DATE = date,
      QUERY_URL = url,
      BOXSCORE_URL = sub.extract.urls(url)
    )
  )
}

# data extraction preparation functions -----------------------------------------------

# function to perform the extraction on each underlying url
extract.xpaths <- function(url) {
  require(stringr)
  require(purrr)
  require(dplyr)
  require(rvest)
  
  # we need to scrape xpath data
  # there are 2 fixed xpaths: scoring & penalty
  # there are up to 18 other dynamic xpaths (9 for 1 team/corresponding 9 for opposition)
  # we will use regex to find them
  
  xpaths <-
    url %>%
    read_html() %>%
    html_nodes("*") %>%
    html_attr("id") %>%
    unique() %>%
    as.data.frame %>%
    na.omit() %>%
    setNames('xpath') %>%
    filter(str_detect(xpath, '(_(skaters|goalies|adv(([_]?)))|(penalty|scoring))'),
           str_detect(xpath, '(_(link|div|sh)|(div|all|switcher)_)') == FALSE) %>%
    .$xpath %>%
    paste
  
  return(
    list(
      BOXSCORE_URL = url,
      BOXSCORE_XPATHS = paste0('//*[@id="', xpaths, '"]')
    ) %>% 
      invoke(cbind.data.frame, .)
  )
}

# function to take a date and prepare a data frame that has all the instructions for the raw data scrape to occurr (for the entire day)
# returns a character data frame of 4 columns -- BOXSCORE_URL/GAME_DATE/XPATH/QUERY_URL
prepare.extraction.data.frame <- function(date) {
  # extract the initial boxscore URLs
  index <- extract.boxscore.urls(date)
  
  # if there are no games, stop
  if(is.null(index$BOXSCORE_URL)) { return() }
  
  # add the xpaths
  index$XPATHS <-   
    index$BOXSCORE_URL %>%
    map(., ~extract.xpaths(.)) %>%
    invoke(rbind, .)
  
  # drop the BOXSCORE_URL and combine into a data frame
  index <- 
    index[which(names(index) != 'BOXSCORE_URL')] %>%
    invoke(cbind.data.frame, .) %>%
    mutate_all(., ~paste(.))
  
  return(index)
  
}

# data extract ------------------------------------------------------------

# function to extract the data from nhl ref for the given date
extract.nhl.ref.raw.data <- function(date) {
  # prepare the instructions (URL/XPATH)
  # returns a character data frame of 4 columns -- BOXSCORE_URL/GAME_DATE/XPATH/QUERY_URL
  index_df <- prepare.extraction.data.frame(date)
  
  if(!is.null(index_df)) {
    index_df$RAW_DATA <-
      lapply(1:nrow(index_df), function(i) {
        tryCatch({
          index_df$XPATHS.BOXSCORE_URL[i] %>%
            read_html() %>%
            html_nodes(xpath = index_df$XPATHS.BOXSCORE_XPATHS[i]) %>%
            html_table(fill = T)
        }, error = function(e) {
          list()
        })
      })
    return(index_df)
  }
}


# data storage ------------------------------------------------------------

# im not going to overcomplicate this
# save the outputted data frame (with all info) for each date
extract.save.nhl.ref.data <- function(date) {
  # extract data
  data <- extract.nhl.ref.raw.data(date)
  
  if(!is.null(data)) {
    file_name <- gsub('-', '', unique(data$GAME_DATE))
    file_full_path <- paste0('./nhl_ref/raw_data/', file_name, '.rds')
    saveRDS(data, file_full_path)
    cat(paste0('Result: Stored\n\n\n'))
  }
}