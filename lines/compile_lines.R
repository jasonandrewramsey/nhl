# !diagnostics off

# packages & dependencies -------------------------------------------------

library(tidyverse)
library(rvest)
library(lubridate)
library(readxl)
library(zoo)



# data load ---------------------------------------------------------------

data.load <- function() {
  path <- './lines/historical'
  
  # file index frame
  file_index <- 
    list(
      file_name = list.files(path),
      file_path = list.files(path, full.names = T)
    ) %>%
    invoke(cbind.data.frame, .) %>%
    mutate_all(., ~paste(.)) %>%
    mutate(file_name = str_replace_all(file_name, c('.xlsx'='', 'nhl odds '='')))
  
  suppressWarnings(
    file_index %>%
      .$file_path %>%
      map(., ~read_excel(., sheet = 1, col_names = F, skip = 1)) %>%
      setNames(file_index$file_name)
  )
  
}

# data prep ---------------------------------------------------------------

set.col.names <- function(.data) {
  cnames <- c('date', 'rot', 'vh', 'team', 'Q1', 'Q2', 'Q3', 'TOTAL', 'open_ml', 'close_ml', 'puck_line', 'puck_line_odds', 'open_total', 'open_total_odds', 'close_total', 'close_total_odds')
  idf <- .data
  
  if(ncol(idf)==14) {
    colnames(idf) <- cnames[cnames %in% c('puck_line', 'puck_line_odds') == F]
  } else {
    idf <- idf[,1:length(cnames)]
    colnames(idf) <- cnames
  }
  return(idf)
}

## add month/day
add.month.day <- function(.data) {
  .data %>%
    rowwise() %>%
    mutate(month = substr(paste(date), start = 1, stop = 2-(4-nchar(paste(date)))),
           day = substr(paste(date), start = nchar(paste(date))-1, stop = nchar(paste(date)))) %>%
    as.data.frame
  
}

### structure this bullshit
combine.lists.add.year <- function(.data) {
  lst <- .data
  
  lapply(1:length(lst), function(i) {
    szn <- names(lst)[i]
    yr <- szn
    multi_yr <- str_detect(yr, '[-]')
    if(multi_yr) {
      yr <- str_split(yr, '[-]') %>% unlist()
      yr[2] <- paste0('20', yr[2])
    } else {
      yr[2] <- yr[1]
    }
    
    lst[[i]] %>% 
      left_join(
        unique(lst[[i]]$month) %>%
          as.data.frame() %>%
          setNames('month') %>%
          mutate(id = row_number()) %>%
          mutate(year = ifelse(id == 1, yr[1], ifelse(month == 1, yr[2], NA)),
                 year = na.locf(year, na.rm = F)) %>%
          dplyr::select(-id) %>%
          as.data.frame %>%
          mutate(SEASON = szn) %>%
          mutate_all(., ~paste(.)),
        by = 'month'
      )
  }) %>%
    invoke(plyr::rbind.fill, .)
}

# map to nhl reference
map.team.name.abbr <- function(.data) {
  .data %>%
    left_join(
      read.csv('./lines/team_abbr_mapping.csv') %>%
        dplyr::select(team=HLINES_TEAM_NAME, TEAM_ABBR=NHL_REF_TEAM_NAME),
      by = 'team'
    )
}

# final cleanup
convert.date.fields <- function(.data) {
  .data %>%
    rowwise() %>%
    mutate(
      month = ifelse(nchar(month) != 2, paste0('0', month), month),
      GAME_DATE = as.Date(paste(year, month, day, sep = '-'))
      ) %>%
    as.data.frame %>%
    mutate(HOME_AWAY = ifelse(vh == 'H', 'H', 'A')) %>%
    dplyr::select(-date, -rot, -month, -year, -day, -vh, -team)
}





