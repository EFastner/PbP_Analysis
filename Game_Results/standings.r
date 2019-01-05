fun.team_standings <- function(raw_season, alignments, con = NA, div = NA){
  #DESCRIPTION - Takes a raw PBP file and summarizes game results by team
  #ARGUMENTS - raw_season is a raw PBP file, most likely a full season. 
  #            Alignments is the team alignments for the given season, with team as the row name, "conference", and "division"
  #            con specifies a specific conference
  #            div specifies a specific division
  
  require(readr)
  require(plyr)
  require(dplyr)
  
  season_summary <- fun.game_result_summary(raw_season) 
  
  team_results <- 
    fun.results_by_team(season_summary) %>% 
    merge(alignments, by.x = "team", by.y = 0)
    
  if(!is.na(con)){
    team_results <- filter(team_results, conference == con)
  }
  
  if(!is.na(div)){
    team_results<- filter(team_results, division == div)
  }
  
  team_points <- 
    team_results %>% 
    group_by(season, team, conference, division) %>% 
    summarise(GP = n(),
              W = sum(result == "W"),
              L = sum(result == "L"),
              OTL = sum(result == "OTL"),
              PTS = max(team_point_total),
              "PT%" = PTS / (GP*2),
              ROW = sum(result == "W" & game_period < 5),
              GF = sum(team_score),
              GA = sum(opp_score),
              DIFF = GF - GA) %>%
    arrange(desc(PTS),
            GP,
            desc(ROW)) %>%
    ungroup() %>%
    mutate(rank = row_number())
  
  return(team_points)
}

fun.standings_tiebreaker <- function(results, teams){
  #DESCRIPTION - Breaks a tie between teams with identical records in the NHL
  #ARGUMENTS - results is a team_results summary created by fun.game_results_summary
  #            teams is a list of teams to break a tie
  
  #Find out if the tie is between 2 or more teams
  if(length(teams) == 2){
    game_list <-
      results %>%
      filter(team == teams[1] & opp == teams[2]) %>%
      arrange(game_date)
    
    #Count number of home and away Games
    home_count <- sum(game_list$side == "home")
    away_count <- sum(game_list$side == "away")
    
    #If the number of games is uneven, subtract the earliest of the games
    if(home_count != away_count){
      if(home_count > away_count){
        remove_game <- 
          filter(game_list, side == "home")[1,"game_id"]
        
        game_list <-
          filter(game_list, game_id != remove_game)
      } else {
        remove_game <- 
          filter(game_list, side == "away")[1,"game_id"]
        
        game_list <-
          filter(game_list, game_id != remove_game)
      }
    }
    
    #Find out which team had the most points
    points_diff <- 
      sum(game_list$team_points) - sum(game_list$opp_points)
    
    if(points_diff > 0){
      return(c(teams[1],teams[2]))
    } else if (points_diff <0) {
      return(teams[2], teams[1])
    } else {
      return(NA)
    }
  }
  
  #For more than 2 teams
  game_list <- 
    results %>%
    filter(team %in% teams & opp %in% teams & side == "home")
  
  #DEVELOPMENT - Even game totals. Group by team, sum points vs GP * 2
}