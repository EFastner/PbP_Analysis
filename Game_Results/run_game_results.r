raw_season <- read_csv("~/Data_Sets/Hockey/Enhanced PbP/pbp_20122013.csv", col_names = TRUE)

season_summary <- fun.game_result_summary(raw_season) 

team_results <- fun.results_by_team(season_summary)

team_points <- team_results %>% group_by(season, team) %>% summarise(points = max(team_point_total)) %>% arrange(points)

team_align_check <- unique(team_results$team) %in% unique(row.names(df.alignment_20112013))
