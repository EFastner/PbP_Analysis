require(ggplot2)
require(plyr)
require(dplyr)
require(readr)

df.lock_line <- read_csv("~/Data_Sets/Hockey/Enhanced PbP/Summarized_Seasons/lock_line.csv", col_names = TRUE)
#df.game_summaries <- read_csv("~/Data_Sets/Hockey/Enhanced PbP/pbp_20182019.csv", col_names = TRUE)

df.reg_season_summaries <- filter(df.team_results, session == "R")

team_name <- c("L.A", "CHI")
season_ID <- "20182019"

df.team_summaries <- filter(df.reg_season_summaries, team %in% team_name & season %in% season_ID) %>% arrange(team_game)

df.lock_line <- filter(df.lock_line, team_game <= max(df.team_summaries$team_game))

df.line_labels <- 
  df.team_summaries %>% 
  group_by(team) %>%
  summarise(points = max(team_point_total),
            games = max(team_game))

viz.season_compare <- 
  ggplot() +
  ggtitle("Lock Line and Point of No Return") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  labs(x = "Game Number", y = "Total Points", color = "Team") +
  geom_line(data = df.lock_line, na.rm = TRUE, mapping = aes(x = team_game, y = lock_line), color = "blue", linetype = "longdash") +
  geom_line(data = df.lock_line, na.rm = TRUE, mapping = aes(x = team_game, y = no_return), color = "red", linetype = "longdash") +
  geom_line(data = df.team_summaries, mapping = aes(x = team_game, y = team_point_total, color = team)) +
  geom_text(data = df.line_labels, mapping = aes(x = games, y = points, label = team, color = team), nudge_x = .75)
