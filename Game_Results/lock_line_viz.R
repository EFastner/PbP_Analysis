require(ggplot2)
require(plyr)
require(dplyr)
require(readr)

df.lock_line <- read_csv("~/Data_Sets/Hockey/Enhanced PbP/Summarized_Seasons/lock_line.csv", col_names = TRUE)
df.game_summaries <- read_csv("~/Data_Sets/Hockey/Enhanced PbP/Summarized_Seasons/all_seasons.csv", col_names = TRUE)

df.reg_season_summaries <- filter(df.game_summaries, session == "R")

team_name <- c("COL", "MIN")
season_ID <- "20162017"

df.team_summaries <- filter(df.reg_season_summaries, team == team_name & season == season_ID) %>% arrange(team_game)

viz.season_compare <- 
  ggplot() +
  geom_line(data = df.lock_line, na.rm = TRUE, mapping = aes(x = team_game, y = lock_line), color = "green", linetype = "longdash") +
  geom_line(data = df.lock_line, na.rm = TRUE, mapping = aes(x = team_game, y = no_return), color = "red", linetype = "longdash") +
  geom_line(data = df.team_summaries, mapping = aes(x = team_game, y = team_point_total, color = team))
