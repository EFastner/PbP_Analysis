require(readr)
require(plyr)
require(dplyr)

#Read in previously generated summary files and list of playoff teams
summary_files <- read_csv("~/Data_Sets/Hockey/Enhanced PbP/Summarized_Seasons/all_seasons.csv", col_names = TRUE)
postseason_teams <- read_csv("~/Data_Sets/Hockey/Enhanced PbP/Summarized_Seasons/playoff_teams.csv", col_names = TRUE)

#Grab only regular season
reg_season <- filter(summary_files, session == "R")

#reg_season$playoff_team <- NA

#Cycle through each row and assign a "Y" or "N" based on if the team made the playoffs or not
for (i in 1:nrow(reg_season)) {
  team_name <- as.character(reg_season[i, "team"])
  season_ID <- as.integer(reg_season[i, "season"])
  
  if(team_name %in% filter(postseason_teams, season == season_ID)$team) {
    reg_season[i, "playoff_team"] <- "Y"
  } else {
    reg_season[i, "playoff_team"] <- "N"
  }
}

#Filter summaries into playoff and non-playoff teams
playoff_teams_summary <- filter(reg_season, playoff_team == "Y")
nonplayoff_teams_summary <- filter(reg_season, playoff_team == "N")

#Grab min/max points for each game of the season for playoff teams
playoff_table <- 
  playoff_teams_summary %>% 
  group_by(team_game) %>% 
  summarise(min_playoff = min(team_point_total),
            max_playoff = max(team_point_total))

#Grab min/max points for each game of the season for nonplayoff teams
nonplayoff_table <- 
  nonplayoff_teams_summary %>%
  group_by(team_game) %>%
  summarise(min_nonplayoff = min(team_point_total),
            max_nonplayoff = max(team_point_total))

#Join playoff and non-playoff lists
final_list <- join(playoff_table, nonplayoff_table, by = "team_game")

#Cycle through each row and calculate the lock line and point of no return
for (i in 1:82) {

  #Lock Line
  if(final_list[i, "max_nonplayoff"] + 1 <= final_list[i, "team_game"] * 2) {
    final_list[i, "lock_line"] <- final_list[i, "max_nonplayoff"] + 1
  } else {
    final_list[i, "lock_line"] <- NA
  }
  
  #Point of No Return
  if(final_list[i, "min_playoff"] - 1 <= final_list[i, "team_game"] * 2 & final_list[i, "min_playoff"] - 1 > 0) {
    final_list[i, "no_return"] <- (final_list[i, "min_playoff"] - 1)
  } else {
    final_list[i, "no_return"] <- NA
  }
}

#Grab final results
results <- 
  final_list %>%
  select(team_game, lock_line, no_return)

#Write Final Results
write_csv(results, "~/Data_Sets/Hockey/Enhanced PbP/Summarized_Seasons/lock_line.csv")