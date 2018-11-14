require(ggplot2)
require(plyr)
require(dplyr)
require(readr)

df.lock_line <- read_csv("~/Data_Sets/Hockey/Enhanced PbP/Summarized_Seasons/lock_line.csv", col_names = TRUE)
#df.game_summaries <- read_csv("~/Data_Sets/Hockey/Enhanced PbP/pbp_20182019.csv", col_names = TRUE)

df.reg_season_summaries <- filter(df.team_results, session == "R")

team_name <- c("MIN", "NSH")
season_ID <- "20182019"

df.team_summaries <- filter(df.reg_season_summaries, team %in% team_name & season %in% season_ID) %>% arrange(team_game)
df.lock_line <- filter(df.lock_line, team_game <= max(df.team_summaries$team_game))

viz.season_compare <- 
  ggplot() +
  geom_line(data = df.lock_line, na.rm = TRUE, mapping = aes(x = team_game, y = lock_line), color = "green", linetype = "longdash") +
  geom_line(data = df.lock_line, na.rm = TRUE, mapping = aes(x = team_game, y = no_return), color = "red", linetype = "longdash") +
  geom_line(data = df.team_summaries, mapping = aes(x = team_game, y = team_point_total, color = team))
