fun.game_count <- function(data_set, gdate, team) {
  #DESCRIPTION - Grabs the cumulative game count for a given team, used in fun.game_result_summary
  #ARGUMENTS - data_set = a list of game results created by fun.game_endings, gdate = a given date, team = a given team
  
  #Uses sum to build an array of games up to a given data that includes a set team
  game_count <- 
    sum(data_set$game_date <= gdate & 
          (data_set$home_team == team | data_set$away_team == team))
  
  return(game_count)
}

fun.point_total <- function(data_set, gdate, team) {
  #DESCRIPTION - Calculates a team's total points earned through a given date
  #ARGUMENTS - data_set = a list of game results created by fun.game_endings, gdate = a given date, team = a given team
  
  point_count <- 
    sum((data_set$game_date <= gdate) * 
          (((data_set$home_team == team) * (data_set$home_points)) + 
             ((data_set$away_team == team) * (data_set$away_points))))
  
  return(point_count) 
}

fun.game_endings <- function(data_set) {
  #DESCRIPTION - Identifies the final ending of each game in a data set
  #ARGUMENTS - data_set = a raw PBP data frame, regular season only
  
  #Attempt to grab the final score of each game based on the period that it ends in
  game_endings <- 
    data_set %>%
    filter((data_set$event_type == "GEND" & data_set$game_period == 3) | 
             (data_set$event_type == "GOAL" & data_set$game_period == 4) |
             (data_set$event_type == "GEND" & data_set$game_period == 5 )) %>%
    mutate(end_state = ifelse(game_period == 3, "Reg", "OT"))
  
  #Identifies if a game ended in OT or a shootout and calculates the final scores
  for (i in 1:nrow(game_endings)) {
    if (game_endings[i, "game_period"] == 5) { 
      so_result <- fun.shootout_winners(data_set, game_endings[i, "game_id"])
      
      ifelse(so_result == "home", 
             game_endings[i, "home_score"] <- game_endings[i, "home_score"] + 1, 
             game_endings[i, "away_score"] <- game_endings[i, "away_score"] + 1)
      
    } else if (game_endings[i, "game_period"] == 4 & (game_endings[i, "home_score"] == game_endings[i, "away_score"])) {
      ot_result <- fun.OT_winners(data_set, game_endings[i, "game_id"])
      game_endings[i, "home_score"] <- ot_result[[1]]
      game_endings[i, "away_score"] <- ot_result[[2]]
    }
  }
  return(game_endings)
}

fun.shootout_winners <- function(data_set, gameID) {
  #DESCRIPTION - totals the number of goals by the home team and total by the away team to find the game winner
  #ARGUMENTS - data_set = a raw PBP file, gameID = a specified game_id
  
  home_goals <- nrow(filter(data_set, game_id == as.integer(gameID) & event_type == "GOAL" & event_team == as.character(home_team)))
  away_goals <- nrow(filter(data_set, game_id == as.integer(gameID) & event_type == "GOAL" & event_team == as.character(away_team)))
  
  return(ifelse(home_goals > away_goals, "home", "away"))
}

fun.OT_winners <- function(data_set, gameID) {
  #DESCRIPTION - finds the max home score and the max away score in an OT period. Sometimes necessary as the score state data from the raw PBP does not always behave as expected
  #ARGUMENTS - data_set = a raw PBP file, gameID = a specified game_id
  
  home_score <- max(filter(data_set, game_id == as.integer(gameID))$home_score)
  away_score <- max(filter(data_set, game_id == as.integer(gameID))$away_score)
  
  return(c(home_score, away_score))
}

fun.game_result_summary <- function(raw_data) {
  #DESCRIPTION - take raw PbP data and summarize each game by score, games played, points earned, and cumulative points
  #ARGUMENTS - expects raw PbP data frame
  
  require(readr)
  require(dplyr)
  require(magrittr)
  
  reg_season <- filter(raw_data, session == "R")
  
  end_games <- fun.game_endings(reg_season)
  
  #Filter to game end event of all regular season games, add columns for the number of points earned by each team
  game_results <- 
    end_games %>% 
    arrange(game_date) %>% 
    select(season, game_date, game_id, game_period, home_team, away_team, home_score, away_score) %>%
    mutate(
      home_points = ifelse(home_score > away_score, 2, ifelse(game_period == 3, 0, 1)),
      away_points = ifelse(away_score > home_score, 2, ifelse(game_period == 3, 0, 1)),
      home_game_num = NA,
      home_point_total = NA,
      away_game_num = NA,
      away_point_total = NA)
  
  #Loop through each line to apply game and point count functions
  for (i in 1:nrow(reg_season_game_count)) {
    game_results[i, "home_game_num"] <- 
      fun.game_count(data_set = game_results, 
                     gdate = game_results[i, "game_date"], 
                     team = as.character(game_results[i, "home_team"]))
    
    game_results[i, "home_point_total"] <- 
      fun.point_total(data_set = game_results, 
                     gdate = game_results[i, "game_date"], 
                     team = as.character(game_results[i, "home_team"])) 
    
    game_results[i, "away_game_num"] <- 
      fun.game_count(data_set = game_results, 
                     gdate = game_results[i, "game_date"], 
                     team = as.character(game_results[i, "away_team"]))
    
    game_results[i, "away_point_total"] <- 
      fun.point_total(data_set = game_results, 
                     gdate = game_results[i, "game_date"], 
                     team = as.character(game_results[i, "away_team"])) 
  }
  
  return(game_results)
}