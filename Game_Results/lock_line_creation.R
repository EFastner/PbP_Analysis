require(readr)
require(plyr)
require(dplyr)

summary_files <- read_csv("~/Data_Sets/Hockey/Enhanced PbP/Summarized_Seasons/all_seasons.csv", col_names = TRUE)
postseason_teams <- read_csv("~/Data_Sets/Hockey/Enhanced PbP/Summarized_Seasons/playoff_teams.csv", col_names = TRUE)


reg_season <- filter(summary_files, session == "R")

reg_season$playoff_team <- NA

for (i in 1:nrow(reg_season)) {
  team_name <- as.character(reg_season[i, "team"])
  season_ID <- reg_season[i, "season"]
  
  if(team_name %in% filter(postseason_teams, season == season_ID)$team) {
    reg_season[i, "playoff_team"] <- "Y"
  } else {
    reg_season[i, "playoff_team"] <- "N"
  }
}

playoff_teams_summary <- filter(reg_season, playoff_team == "Y")
nonplayoff_teams_summary <- filter(reg_season, playoff_team == "N")

lock_table <- 
  playoff_teams_summary %>% 
  group_by(team_game) %>% 
  summarise(min_playoff = min(team_point_total),
            max_playoff = max(team_point_total))

no_return_table <- 
  nonplayoff_teams_summary %>%
  group_by(team_game) %>%
  summarise(min_nonplayoff = min(team_point_total),
            max_nonplayoff = max(team_point_total))

final_list <- join(lock_table, no_return_table, by = "team_game")
  