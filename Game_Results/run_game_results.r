require(readr)

season_IDs <- c("20072008", "20082009", "20092010", "20102011", "20112012", "20122013", "20132014", "20142015", "20152016", "20162017", "20172018")

for (i in 1:length(season_IDs)){
  
  raw_season <- read_csv(paste0("~/Data_Sets/Hockey/Enhanced PbP/pbp_", season_IDs[i], ".csv"), col_names = TRUE)
  
  season_summary <- fun.game_result_summary(raw_season) 
  
  team_results <- fun.results_by_team(season_summary)
  
  write_csv(team_results, path = paste0("~/Data_Sets/Hockey/Enhanced PbP/Summarized_Seasons/summary_", season_IDs[i], ".csv"))
  
}

#team_points <- team_results %>% group_by(season, team) %>% summarise(points = max(team_point_total)) %>% arrange(points)

#team_align_check <- unique(team_results$team) %in% unique(row.names(df.alignment_20112013))
