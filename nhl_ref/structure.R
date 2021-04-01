# !diagnostics off

# packages & dependencies -------------------------------------------------

library(tidyverse)
library(rvest)
library(lubridate)
library(reshape2)


# data load ---------------------------------------------------------------

data.load <- function() {
  path <- './nhl_ref/raw_data/'
  
  # file index frame
  file_index <- 
    list(
    file_name = list.files(path),
    file_path = list.files(path, full.names = T)
  ) %>%
    invoke(cbind.data.frame, .) %>%
    mutate_all(., ~paste(.)) %>%
    mutate(file_name = str_replace_all(file_name, c('.rds'='')))
  
  
  file_index %>%
    .$file_path %>%
    map(., ~readRDS(., .)) %>%
    setNames(file_index$file_name) %>%
    invoke(rbind, .)
}


# data prep ---------------------------------------------------------------

# GAME_TABLE creation
create.team.to.game.id.df <- function(.data) {
  temp <-
    .data %>%
    filter(XPATH %in% c('scoring', 'penalty')==F) %>%
    mutate(TEAM_ABBR = str_replace_all(XPATH, c('_(skaters|goalies|adv(([_]?)))'='',
                                                '(ALL|CL)([5]v[5]|All|PP|SH|EV)'=''))) %>%
    dplyr::select(GAME_ID, TEAM_ABBR) %>%
    distinct()
  
  temp %>%
    left_join(
      temp %>%
        dplyr::select(GAME_ID, OPP_ABBR=TEAM_ABBR),
      by = 'GAME_ID',
    ) %>%
    filter(TEAM_ABBR != OPP_ABBR)
}

# Add GAME_TABLE field (where applicable!)
add.team.abbr.fields <- function(.data, game_table) {
  list(
    # without scoring/penalty
    .data %>%
      filter(XPATH %in% c('scoring', 'penalty')==F) %>%
      mutate(TEAM_ABBR = str_replace_all(XPATH, c('_(skaters|goalies|adv(([_]?)))'='',
                                                  '(ALL|CL)([5]v[5]|All|PP|SH|EV)'=''))) %>%
      left_join(
        game_table,
        by = c('GAME_ID', 'TEAM_ABBR')
      ),
    # one scoring/penalty
    .data %>%
      filter(XPATH %in% c('scoring', 'penalty'))
  ) %>%
    invoke(plyr::rbind.fill, .)
}


# function to combine the process, adding two critical pieces of team info to the df
add.team.abbrs <- function(.data) {
  # 1. GAME_ID to TEAM_ABBR (With Opponent)
  game_id_team_abbr <- 
    .data %>% 
    create.team.to.game.id.df()
  
  # 2. Join that back with the original data (excluding scoring/penalty as these are shared)
  output_tbl <- .data %>% add.team.abbr.fields(., game_table = game_id_team_abbr)
  
  # Perform check
  if(
    nrow(.data) == nrow(output_tbl)
  ) {
    return(output_tbl)
  }
  
}

add.id.cols <- function(.data) {
  .data %>%
    mutate(GAME_ID = XPATHS.BOXSCORE_URL %>% str_replace_all(., c('https://www.hockey-reference.com/boxscores/'='', '.html'='')),
           XPATH = XPATHS.BOXSCORE_XPATHS %>% str_replace_all(., c('[/]{2}[*][\\[][@]id[=]"'='', '"[]]'=''))) %>%
    dplyr::select(GAME_DATE, GAME_ID, XPATH, RAW_DATA) %>%
    add.team.abbrs()
}




# data structuring on the line item level ---------------------------------------------------------------


#Single line data frame into a named list (names being the columns)
df.to.named.list <-
  function(.data) {
    .data %>%
      t %>%
      as.list() %>%
      setNames(names(.data))
  }


#Function to extract the allowed points
alwd <- function(...) {
  sum(...) - (...)
}

set.first.row.to.cnames <- function(.data) {
  idf <- .data
  cnames <- idf[1,] %>% as.list() %>% invoke(c, .) %>% unname() %>% paste
  cnames <- str_replace_all(cnames, c('[%]'='_PCT')) %>% str_to_upper()
  colnames(idf) <- cnames
  return(idf[-1,])
}



# structure scoring indivial item
structure.individual.scoring.xpath <- function(param_list, game_teams) {
  print(param_list$GAME_ID)
  
  #Period index
  nhl_period_index <- 
    data.frame(
      html_name = c('1st Period', '2nd Period', '3rd Period', 'Shootout', '1st OT Period', '2nd OT Period', '3rd OT Period', '4th OT Period', '5th OT Period'),
      standardized_name = c('Q1', 'Q2', 'Q3', 'SHTOUT', 'OT', 'OT', 'OT', 'OT', 'OT'),
      stringsAsFactors = F)
  
  #Shootout function
  shootout_goal_function <- function(.data, score_data) {
    .data %>%
      cbind.data.frame(., 
                       data.frame(SHTOUT_UNS = grepl('unsuccessful', c(colnames(score_data)[4], score_data[[4]])))) %>%
      mutate_all(~paste(.))
  }
  
  #Rename function
  rename.columns <- function(.data) {
    name_conversion_index <- 
      data.frame(
        old_names = c('Team', 'Q1', 'Q2', 'Q3', 'OT', 'SHTOUT', 'total'),
        new_names = c("Team", "Q1_Points", "Q2_Points", "Q3_Points", "OT_Points", 'Q4_Points', 'Total_Points'),
        stringsAsFactors = F)
    
    colnames(.data) <-
      lapply(1:length(.data), function(x) {
        name_conversion_index$new_names[match(colnames(.data)[x], name_conversion_index$old_names)]
      }) %>%
      invoke(c, .)
    return(.data)
  }
  
  param_list$TEAMS <-
    dat %>%
    filter(GAME_ID == param_list$GAME_ID) %>%
    dplyr::select(TEAM_ABBR) %>%
    distinct() %>%
    na.omit()
  
  #Full join for both competitors and periods
  full_info_index <- 
    nhl_period_index %>%
    mutate(dummy = 1) %>%
    full_join(
      param_list$TEAMS %>%
        mutate(dummy = 1),
      by = 'dummy'
    ) %>%
    mutate(PERIOD = standardized_name) %>%
    dplyr::select(PERIOD, TEAM_ABBR) %>%
    distinct()
  
  c(colnames(param_list$RAW_DATA[[1]])[2], param_list$RAW_DATA[[1]][[2]]) %>%
    as.data.frame %>%
    setNames('TEAM_ABBR') %>%
    mutate_all(~paste(.)) %>%
    mutate(PERIOD = ifelse(TEAM_ABBR %in% nhl_period_index$html_name, paste(TEAM_ABBR), NA),
           PERIOD = na.locf(PERIOD, na.rm = F)) %>%
    shootout_goal_function(., param_list$RAW_DATA[[1]]) %>%
    filter(TEAM_ABBR != PERIOD) %>%
    mutate(PERIOD = nhl_period_index$standardized_name[match(PERIOD, nhl_period_index$html_name)],
           KEEP = ifelse(PERIOD != 'SHTOUT', TRUE, ifelse(SHTOUT_UNS == F, TRUE, FALSE))) %>%
    filter(KEEP == T) %>%
    dplyr::select(-KEEP) %>%
    group_by(TEAM_ABBR, PERIOD, .groups = 'drop') %>%
    summarise(
      team_goals = n()
    ) %>%
    as.data.frame %>%
    full_join(
      full_info_index,
      by = c('TEAM_ABBR', 'PERIOD')
    ) %>%
    mutate_at(vars(-TEAM_ABBR, -PERIOD), ~ifelse(is.na(.), 0, .)) %>%
    group_by(PERIOD) %>%
    mutate(total_goals = sum(team_goals)) %>%
    as.data.frame %>%
    mutate(goals_allowed = total_goals - team_goals) %>%
    mutate(team_shootout_goals = ifelse(PERIOD == 'SHTOUT', ifelse(team_goals == 0 & goals_allowed == 0, 0, ifelse(team_goals > goals_allowed, 1, 0)), NA),
           opp_shootout_goals = ifelse(PERIOD == 'SHTOUT', ifelse(team_goals == 0 & goals_allowed == 0, 0, ifelse(team_goals > goals_allowed, 0, 1)), NA)) %>%
    mutate(team_goals = ifelse(PERIOD == 'SHTOUT', team_shootout_goals, team_goals),
           goals_allowed = ifelse(PERIOD == 'SHTOUT', opp_shootout_goals, goals_allowed),
           total_goals = ifelse(PERIOD == 'SHTOUT', ifelse(total_goals > 0, 1, 0), total_goals)) %>%
    dplyr::select(-team_shootout_goals, -opp_shootout_goals) %>%
    dcast(., TEAM_ABBR~PERIOD, value.var = 'team_goals') %>%
    mutate(TOTAL_GOALS = Q1+Q2+Q3+OT+SHTOUT) %>%
    mutate_at(vars(-TEAM_ABBR), ~as.numeric(paste(.))) %>%
    #Add in the allowed stats by applying the alwd function
    mutate_at(vars(-TEAM_ABBR),
              list(ALWD = alwd)) %>%
    rowwise() %>%
    mutate(HOME_AWAY = ifelse(grepl(TEAM_ABBR, param_list$GAME_ID), 'H', 'A')) %>%
    as.data.frame %>%
    mutate(SHOOTOUT_FLAG = ifelse(sum(SHTOUT)>0, 'Y', 'N'),
           GAME_DATE = param_list$GAME_DATE,
           GAME_ID = param_list$GAME_ID)
}


structure.individual.goalies.xpath <- function(param_list) {
  print(param_list$GAME_ID)
  
  param_list$RAW_DATA[[1]] %>%
    set.first.row.to.cnames() %>%
    separate(TOI, into = c('TOI_MIN', 'TOI_SEC'), sep = '[:]') %>%
    mutate_at(vars(-PLAYER, -DEC), ~as.numeric(paste(.))) %>%
    mutate(TOI = TOI_MIN+(TOI_SEC/60)) %>%
    dplyr::select(-TOI_MIN, -TOI_SEC) %>%
    mutate(GAME_DATE = param_list$GAME_DATE,
           GAME_ID = param_list$GAME_ID,
           TEAM_ABBR = param_list$TEAM_ABBR)
}


# data structuring at the aggregate level ---------------------------------

structure.aggregate.scoring.xpaths <- function(.data) {
  # create teams for each game id (requirement of individual level)
  game_teams <- 
    .data %>%
    dplyr::select(GAME_ID, TEAM_ABBR) %>%
    distinct() %>%
    na.omit() %>%
    split(., .$GAME_ID) %>%
    map(., ~.$TEAM_ABBR %>% paste)
  
  
  # only care about the scoring xpath
  scoring_df <- .data %>% filter(XPATH == 'scoring')
  
  scoring_df %>%
    mutate(row_id = row_number()) %>%
    split(., .$row_id) %>%
    map(., ~df.to.named.list(.)) %>%
    map(., ~structure.individual.scoring.xpath(., game_teams)) %>%
    invoke(rbind, .)
}

structure.aggregate.goalie.xpaths <- function(.data) {
  # only care about the scoring xpath
  goalies_df <- .data %>% filter(str_detect(XPATH, '_goalies'))
  
  goalies_df %>%
    mutate(row_id = row_number()) %>%
    split(., .$row_id) %>%
    map(., ~df.to.named.list(.)) %>%
    map(., ~structure.individual.goalies.xpath(.)) %>%
    invoke(rbind, .)
}



# execution ---------------------------------------------------------------

# Load / prep data
dat <- 
  data.load() %>%
  add.id.cols()


data_bin <-
list(
  GAME_TABLE = 
    dat %>%
    dplyr::select(GAME_ID, GAME_DATE, TEAM_ABBR, OPP_ABBR) %>%
    distinct() %>%
    na.omit(),
  
  SCORING_TABLE =
    dat %>%
    structure.aggregate.scoring.xpaths(),
  
  GOALIE_TABLE =
    dat %>%
    structure.aggregate.goalie.xpaths()
)


# save each individual table as a csv for the plebberoonies
for(i in 1:length(data_bin)) {
  write.csv(data_bin[[i]], paste0('./nhl_ref/data/', names(data_bin)[i], '.csv'), row.names = F)
  print(paste0('saved: ', names(data_bin)[i]))
}

# save the object for me
saveRDS(data_bin, './nhl_ref/data/all_objects.rds')




