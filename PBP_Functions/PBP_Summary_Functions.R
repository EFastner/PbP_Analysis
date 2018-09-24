readfiles <- function(dir, sep = "|") {
  #DESCRIPTION - Load all files in a specified directory into one frame, separator is "|" unless specified
  filelist <- list.files(dir, 
                         all.files = FALSE, 
                         full.names = TRUE)
  
  df.combined_files <- do.call("rbind",lapply(filelist, read.delim, sep = sep, header = TRUE))
  
  return(df.combined_files)
  
}

fun.corsi_table <- function(rawdata, v.corsi_events = v.corsi_events) {
  #DESCRIPTION - Modify raw_data to get corsi data frame
  
  #Create Corsi Only Table for Chart Data
  df.corsi_table <- filter(rawdata, rawdata$event_type %in% names(v.corsi_events)) %>% 
    group_by(event_team) %>% 
    mutate(team_corsi = row_number())
  
  return(df.corsi_table)
}

fun.team_summary <- function(rawdata) {
  #DESCRIPTION - Modify raw_data to get team summary data frame  
  
  require(tidyr)
  
  #Create Data for Team Summary Graph
  team_graphsummary <- rawdata %>% 
    filter(!is.na(event_team)) %>% 
    group_by(event_team) %>% 
    summarise(Goals = sum(event_type == "GOAL"), 
              Hits = sum(event_type == "HIT"),
              Blocks = sum(event_type == "BLOCK"),
              Corsi = sum(event_type %in% names(v.corsi_events)),
              Shots = sum(event_type %in% c("SHOT","GOAL")), 
              Faceoffs = sum(event_type == "FAC")) %>%
    gather(Event, Value, -event_team)
  
}

fun.skater_stats <- function(dataset, team_indicator) {
  #DESCRIPTION - Create Summary of stats for all skaters on each team. Specific to home/away  
  
  if(team_indicator == "home") {
    output <- 
      subset(dataset, !player %in% goalie) %>% group_by(player, team) %>% 
      summarise(
        TOI = sum(event_length)/60,
        G = sum(event_type == "GOAL" & event_player_1 == as.character(player)),
        A1 = sum(event_type == "GOAL" & event_player_2 == as.character(player) & !is.na(event_player_2)),
        A2 = sum(event_type == "GOAL" & event_player_3 == as.character(player) & !is.na(event_player_3)),
        A = A1 + A2,
        P = G + A1 + A2,
        P1 = G + A1,
        FOW = sum(event_type == "FAC" & is_home == 1 & (event_player_1 == as.character(player) | event_player_2 == as.character(player))),
        FOT = sum(event_type == "FAC" & (event_player_1 == as.character(player) | event_player_2 == as.character(player))),
        FOL = FOT - FOW,
        SOG = sum(event_type %in% c("SHOT", "GOAL") & event_player_1 == as.character(player)),
        iCF = sum(event_type %in% names(v.corsi_events) & event_player_1 == as.character(player)),
        HITS = sum(event_type == "HIT" & event_player_1 == as.character(player)),
        BLK = sum(event_type == "BLOCK" & event_player_1 == as.character(player)),
        PEND = sum(event_type == "PENL" & event_player_2 == as.character(player) & !is.na(event_player_2)),
        PENT = sum(event_type == "PENL" & event_player_1 == as.character(player)),
        CF = sum(event_type %in% names(v.corsi_events) & is_home == 1),
        CA = sum(event_type %in% names(v.corsi_events) & is_home != 1),
        CF_5v5 = sum(event_type %in% names(v.corsi_events) & is_home == 1 & game_strength_state == "5v5"),
        CA_5v5 = sum(event_type %in% names(v.corsi_events) & is_home == 0 & game_strength_state == "5v5"),
        GF_5v5 = sum(event_type == "GOAL" & is_home == 1 & game_strength_state == "5v5"),
        GA_5v5 = sum(event_type == "GOAL" & is_home == 0 & game_strength_state == "5v5"))
  } else {
    output <- 
      subset(dataset, !player %in% goalie) %>% group_by(player, team) %>% 
      summarise(
        TOI = sum(event_length)/60,
        G = sum(event_type == "GOAL" & event_player_1 == as.character(player)),
        A1 = sum(event_type == "GOAL" & event_player_2 == as.character(player) & !is.na(event_player_2)),
        A2 = sum(event_type == "GOAL" & event_player_3 == as.character(player) & !is.na(event_player_3)),
        A = A1 + A2,
        P = G + A1 + A2,
        P1 = G + A1,
        FOW = sum(event_type == "FAC" & is_home == 0 & (event_player_1 == as.character(player) | event_player_2 == as.character(player))),
        FOT = sum(event_type == "FAC" & (event_player_1 == as.character(player) | event_player_2 == as.character(player))),
        FOL = FOT - FOW,
        SOG = sum(event_type %in% c("SHOT", "GOAL") & event_player_1 == as.character(player)),
        iCF = sum(event_type %in% names(v.corsi_events) & event_player_1 == as.character(player)),
        HITS = sum(event_type == "HIT" & event_player_1 == as.character(player)),
        BLK = sum(event_type == "BLOCK" & event_player_1 == as.character(player)),
        PEND = sum(event_type == "PENL" & event_player_2 == as.character(player) & !is.na(event_player_2)),
        PENT = sum(event_type == "PENL" & event_player_1 == as.character(player)),
        CF = sum(event_type %in% names(v.corsi_events) & is_home != 1),
        CA = sum(event_type %in% names(v.corsi_events) & is_home == 1),
        CF_5v5 = sum(event_type %in% names(v.corsi_events) & is_home == 0 & game_strength_state == "5v5"),
        CA_5v5 = sum(event_type %in% names(v.corsi_events) & is_home == 1 & game_strength_state == "5v5"),
        GF_5v5 = sum(event_type == "GOAL" & is_home == 0 & game_strength_state == "5v5"),
        GA_5v5 = sum(event_type == "GOAL" & is_home == 1 & game_strength_state == "5v5"))
    
  }
  
  output$player <-  as.character(output$player)
  
  return(output)
}

fun.skater_summary <- function(dataset) {
  #DESCRIPTION - Utilize fun.skater_stats function to create stats summary for all player in PBP frame
  
  if (!("is_home" %in% colnames(dataset))) {
    dataset <- ds.enhancedPBP(dataset, v.corsi_events = v.corsi_events)
  }
  
  #Run Skater_Stats function for all 6 skater slots on PBP file
  for (runcount in c(1:6)) {
    assign(paste("home_player_summary", runcount, sep = ""), fun.skater_stats(rename(dataset, goalie = home_goalie, player = paste("home_on_", runcount, sep = ""), team = home_team), "home"))
    assign(paste("away_player_summary", runcount, sep = ""), fun.skater_stats(rename(dataset, goalie = away_goalie, player = paste("away_on_", runcount, sep = ""), team = away_team), "away"))
  }
  
  #Aggregate function output files to create full list of skater stats
  summary.home_skaters <- bind_rows(home_player_summary1, 
                                    home_player_summary2, 
                                    home_player_summary3, 
                                    home_player_summary4, 
                                    home_player_summary5, 
                                    home_player_summary6) %>% 
    group_by(player, team) %>%
    summarise_all(funs(sum)) %>%
    mutate("FO%" = ifelse(FOT == 0, 0, FOW/FOT))
  
  for (i in 1:nrow(summary.home_skaters)) {
    gs_cats <- summary.home_skaters[i,] %>% select(player, G, A1, A2, SOG, BLK, PENT, PEND, FOW, FOL, CF_5v5, CA_5v5, GF_5v5, GA_5v5)
    summary.home_skaters[i, "GS"] <- fun.gamescore(gs_cats[,2:14])
  }
  
  summary.away_skaters <- bind_rows(away_player_summary1, 
                                    away_player_summary2, 
                                    away_player_summary3, 
                                    away_player_summary4, 
                                    away_player_summary5, 
                                    away_player_summary6) %>% 
    group_by(player, team) %>%
    summarise_all(funs(sum)) %>%
    mutate("FO%" = ifelse(FOT == 0, 0, FOW/FOT))
  
  for (i in 1:nrow(summary.away_skaters)) {
    gs_cats <- summary.away_skaters[i,] %>% select(player, G, A1, A2, SOG, BLK, PENT, PEND, FOW, FOL, CF_5v5, CA_5v5, GF_5v5, GA_5v5)
    summary.away_skaters[i, "GS"] <- fun.gamescore(gs_cats[,2:14])
  }
  
  return(list(summary.home_skaters, summary.away_skaters))
}

fun.combine_skater_stats <- function(dataset) {
  #DESCRIPTION: Combines the home and away skater summary lists created by fun.skater_summary
  #ARGUMENTS: dataset = A list object with home and away stats in separate lists
  
  output <- dataset %>% 
    bind_rows() %>%
    group_by(player, team) %>%
    summarise_all(funs(sum)) %>%
    mutate("FO%" = ifelse(FOT == 0, 0, FOW/FOT))
  
  return(output)
}

fun.gamescore <- function(stats, gs_weights = st.game_score_weights) {
  #DESCRIPTION - Outputs the gamescore of a given stat line
  #ARGUMENTS - stats = a list of values corresponding to each item in order: G, A1, A2, iSF, iBLK, iPent, iPend, iFOW, iFOL, CF, CA, GF, GA
  
  return(sum(stats * gs_weights))
}

fun.add_roster_data <- function(skater_stats, roster) {
  #DESCRIPTION - Join player positions and numbers to skater summary
  #ARGUMENTS - skater_stats will accept either a list of player stats separated by team or one combined list. roster accepts a raw roster output
  
  merge_roster <- raw_roster %>% select(player_name, player_position, player_number)
  
  if (class(skater_stats[[1]]) != "list") {
    return(merge(skater_stats, merge_roster, by.x = "player", by.y = "player_name"))
  } else {
    for (i in 1:length(skater_stats)) {
      skater_stats[[i]] <- merge(skater_stats[[i]], merge_roster, by.x = "player", by.y = "player_name")
    }
    return(skater_stats)
  }
}