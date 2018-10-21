require(readr)
require(plyr)
require(dplyr)

summary_files <- read_csv("~/Data_Sets/Hockey/Enhanced PbP/Summarized_Seasons/all_seasons.csv", col_names = TRUE)

postseason_games <- filter(summary_files, session == "P")

postseason_teams <-
  postseason_games %>%
  select(season, conference, division, team) %>%
  unique()

write_csv(postseason_teams, "~/Data_Sets/Hockey/Enhanced PbP/Summarized_Seasons/playoff_teams.csv")