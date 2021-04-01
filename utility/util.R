# !diagnostics off

source('./nhl_ref/main.R')

# establish dates you want to scrape
dates <- seq.Date(from = as.Date('2013-04-11'), to = as.Date('2021-01-01'), by = "days")

dates %>%
  map(., ~extract.save.nhl.ref.data(.))


