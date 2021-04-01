source('./lines/compile_lines.R')

df <-
  data.load() %>%
  map(., ~set.col.names(.)) %>%
  map(., ~add.month.day(.)) %>%
  combine.lists.add.year(.) %>%
  map.team.name.abbr(.) %>%
  convert.date.fields(.)


write.csv(df, './lines/compiled_historical_lines_df.csv', row.names = F)