# !diagnostics off

# packages & dependencies -------------------------------------------------

library(tidyverse)
library(rvest)
library(lubridate)



# data upload -------------------------------------------------------------

load.data <- function() {
  # load nhl ref data
  db <- readRDS('./nhl_ref/data/all_objects.rds')
  db$LINES <- read.csv('./lines/compiled_historical_lines_df.csv', stringsAsFactors = F)
  return(db)
}

#Function to convert american odds to numeric
american_to_numeric_odds <- function(odds) {
  odds <- as.numeric(odds)
  if(odds<0) {
    ((abs(odds))+100)/(abs(odds))
  } else {
    1+(odds/100)
  }
}



# create scores w/ odds table ---------------------------------------------

create.scores.odds.tbl <- function(db) {
  days_off <-
    db$SCORING_TABLE %>%
    mutate(GAME_DATE = as.Date(GAME_DATE)) %>%
    dplyr::select(TEAM_ABBR, GAME_ID, GAME_DATE) %>%
    distinct() %>%
    group_by(TEAM_ABBR) %>%
    arrange(GAME_DATE) %>%
    mutate(TEAM_DAYS_OFF = as.numeric(difftime(GAME_DATE, lag(GAME_DATE), unit = "days"))) %>%
    na.omit() %>%
    as.data.frame %>%
    dplyr::select(-GAME_DATE)
  
  SCORES_ODDS <-
    db$SCORING_TABLE %>%
    left_join(
      db$GAME_TABLE %>%
        dplyr::select(-GAME_DATE) %>%
        na.omit(),
      by = c('GAME_ID', 'TEAM_ABBR')
    ) %>%
    left_join(
      days_off,
      by = c('GAME_ID', 'TEAM_ABBR')
    ) %>%
    left_join(
      days_off %>%
        dplyr::select(GAME_ID, OPP_ABBR = TEAM_ABBR, OPP_DAYS_OFF = TEAM_DAYS_OFF),
      by = c('GAME_ID', 'OPP_ABBR')
    ) %>%
    left_join(
      db$LINES %>%
        dplyr::select(SEASON, TEAM_ABBR, GAME_DATE, ML_ODDS_AMER=close_ml, TOT_HANDICAP = close_total, TOT_ODDS_AMER = close_total_odds, PL_HANDICAP = puck_line, PL_ODDS_AMER = puck_line_odds),
      by = c('TEAM_ABBR', 'GAME_DATE')
    ) %>%
    na.omit() %>%
    rowwise() %>%
    mutate(ML_ODDS_NUMERIC = american_to_numeric_odds(ML_ODDS_AMER),
           TOT_ODDS_NUMERIC = american_to_numeric_odds(TOT_ODDS_AMER),
           PL_ODDS_NUMERIC = american_to_numeric_odds(PL_ODDS_AMER),
           GOALS_GAME_TOTAL = TOTAL_GOALS + TOTAL_GOALS_ALWD,
           PUCK_DIFFERENCE = TOTAL_GOALS-TOTAL_GOALS_ALWD) %>%
    as.data.frame %>%
    mutate(
      WIN_LOSS_RES_BINARY = ifelse(TOTAL_GOALS>TOTAL_GOALS_ALWD, 1, 0),
      PUCK_LINE_RES_BINARY = ifelse((PUCK_DIFFERENCE + PL_HANDICAP)>0, 1, 0),
      ML_NET_RET = (WIN_LOSS_RES_BINARY*ML_ODDS_NUMERIC)-1,
      PL_NET_RET = (PUCK_LINE_RES_BINARY*PL_ODDS_NUMERIC)-1
    ) 
  
}



# load the data base (hehe)
db <- load.data()

# create the odds 
scores_odds_tbl <- create.scores.odds.tbl(db)


mean(scores_odds_tbl$PL_NET_RET)


scores_with_odds %>%
  summarise(
    avg = mean(ML_NET_RET)
  )


scores_with_odds %>%
  group_by(TEAM_DAYS_OFF, OPP_DAYS_OFF) %>%
  #filter(TEAM_DAYS_OFF == 3, OPP_DAYS_OFF == 1) %>%
  summarise(
    total_games = n(),
    avg_roi = mean(ML_NET_RET)
  ) %>%
  filter(total_games > 10) %>%
  arrange(desc(avg_roi)) %>%
  as.data.frame


db$GOALIE_TABLE %>%
  head



mod <- glm(WIN_LOSS_BINARY~TEAM_DAYS_OFF+OPP_DAYS_OFF+ML_NUM, data = scores_with_odds, family="binomial")
model_frame <- scores_with_odds
model_frame$EST <- predict(mod, scores_with_odds, type = "response")

model_frame %>%
  mutate(VEGAS = 1/ML_NUM,
         VALUE = EST-VEGAS) %>%
  filter(VALUE > 0.01) %>%
  summarise(
    games = n(),
    roi = mean(ML_NET_RET)
  )





db$GOALIE_TABLE %>%
  filter(PLAYER != 'Empty Net') %>%
  group_by(PLAYER) %>%
  summarise(
    SA = sum(SA),
    GA = sum(GA),
    SV_PCT = (SA-GA)/SA,
    GAMES = length(unique(GAME_ID))
  ) %>%
  as.data.frame %>%
  mutate(AGG_SV_PCT = (sum(SA)-sum(GA))/sum(SA)) %>%
  arrange(desc(GAMES))
  group_by(GAME_ID, TEAM_ABBR) %>%
  summarise(GOALIE_CNT = n()) %>%
  filter(cnt > 1) %>%
  head(1) %>%
  inner_join(db$GOALIE_TABLE, by = c('GAME_ID', 'TEAM_ABBR'))
  
  
  
  
temp <-  
db$GOALIE_TABLE %>%
  filter(year(GAME_DATE)==2021,
         #TEAM_ABBR == 'BOS',
         PLAYER != 'Empty Net') %>%
  group_by(GAME_ID, TEAM_ABBR) %>%
  mutate(GOALIE_CNT = length(unique(PLAYER))) %>%
  filter(GOALIE_CNT == 1) %>%
  inner_join(
    scores_odds_tbl %>%
      dplyr::select(GAME_ID, TEAM_ABBR, ML_ODDS_NUMERIC, ML_NET_RET, PL_NET_RET),
    by = c('GAME_ID', 'TEAM_ABBR')
  ) %>%
  #filter(PLAYER == 'Adin Hill')
  group_by(PLAYER, TEAM_ABBR) %>%
  summarise(
    SA = sum(SA),
    GA = sum(GA),
    SV_PCT = (SA-GA)/SA,
    GAMES = length(unique(GAME_ID)),
    ROI_ML = mean(ML_NET_RET),
    ROI_PL = mean(PL_NET_RET)
  ) %>%
  as.data.frame %>%
  group_by(TEAM_ABBR) %>%
  mutate(TEAM_GOALIE_MAX_ST = max(GAMES)) %>%
  as.data.frame %>%
  mutate(STARTER_BACKUP = ifelse(GAMES == TEAM_GOALIE_MAX_ST, 'START', 'BACK'))
  
  
  
  filter(GAMES > 10) %>%
  arrange(desc(ROI_PL))




temp %>%
  group_by(T)