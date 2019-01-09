ds.date_compile <- function(season_start, #Year that season begins on. 2017 - 2018 season would be 2017
                            game_type = c("PR", "R", "P"), #PR = Preseason Games, R = Regular Season Games, P = Postseason Games
                            startdate = paste0(season_start,"-09-01"), #Starting date to scrape
                            enddate = paste0(season_start+1, "-07-31")) #Ending date to screap
  {
  ##DESCRIPTION: Pull a list of games played during specified date range and return PBP and roster data
  ##ARGUMENTS: Specified above
  ##DEPENDENCIES: Manny Perry's dryscrape functions, as well as packages called below
  
  require(dplyr)
  require(magrittr)
  require(RCurl)
  require(rjson)
  require(lubridate)
  require(doMC)
  require(rvest)
  
  #Create the seasonID for dates selected
  seasonID <- paste0(season_start, season_start+1) #Starting year plus ending year
  
  df.game_list <- character(0)
  
  #Scrape all of the gameIDs for the specified dates
  df.game_list <- data.frame(ds.scrape_schedule(start = startdate, 
                                                end = enddate))
  
  #If there were no games, return NA
  if (length(df.game_list) == 0) {
    return(NA)
  }
  
  df.game_numbers <- 
    filter(df.game_list, session %in% game_type) %>%
    mutate(gameID = as.integer(substr(game_id, nchar(game_id)-4, nchar(game_id)))) %>%
    select(gameID) %>%
    arrange(gameID)
  
  list.game_items <- ds.compile_games(games = game, season = seasonID)
  
  return(list.game_items)

}

ds.save_game_scrape <- function(game_list, output.name, dir.output = '~/Data Sets/Hockey/Season_Sets') {
  #DESCRIPTION: Parse PBP and Roster data and save in a delimmed file using "|" as separator
  #ARGUMENTS: game_list = a list containing PBP and Roster data, typically output from ds.date_compile, output.name = name to save file as, dir.output = target directory for output
  
  #Create a folder for the output
  dir.create(paste(dir.output, seasonID, sep = "/"))
  dir.create(paste(dir.output, seasonID, "PBP", sep = "/"))
  dir.create(paste(dir.output, seasonID, "Rosters", sep = "/"))
  
  #Parse game list into PBP and Rosters
  df.pbp <- game_list[[1]]
  df.roster <- game_list[[2]]
  
  write_delim(df.pbp, paste(dir.output, seasonID, "PBP", output.name, sep = "/"), delim = "|")
  write_delim(df.roster, paste(dir.output, seasonID, "Rosters", output.name, sep = "/"), delim = "|")  
  
}

ds.enhancedPBP <- function(rawdata, corsi_events = v.corsi_events) {
  #DESCRIPTION - Add various additional columns to scraped PBP data
  #ARGUMENTS - rawdata = a PBP data frame scraped with Manny Perry's dryscrape functions, v.corsi_events = a vector listing corsi events in names
  #DEPENDENCIES - None
  
  #Add Dummy Column for Home Team
  rawdata$is_home <- ifelse(rawdata$event_team == as.character(rawdata$home_team),1,0)
  
  #Add Columns for Game Minutes
  rawdata$game_mins <- rawdata$game_seconds/60
  
  #Add side_coords to move all game events to one side of the ice, home on left & away on right
  rawdata$side_coordsx <- ifelse(rawdata$is_home == 1, -abs(rawdata$coords_x), abs(rawdata$coords_x))
  rawdata$side_coordsy <- ifelse((rawdata$is_home == 1 & rawdata$coords_x > 0) | (rawdata$is_home == 0 & rawdata$coords_x < 0), -rawdata$coords_y, rawdata$coords_y)
  
  #Add Dummy Column for Corsi
  rawdata$is_corsi <- ifelse(rawdata$event_type %in% names(corsi_events), 1, 0)
  
  return(rawdata)
}

ds.daily_scrape <- function(season, scrape_date = today() -1 ) {
  #DESCRIPTION: Scrape the previous day's game data and compile in a master file. For use with Chron job to automatically pull the previous day's results  
  
  seasonID <- paste0(season, season + 1)
  game_output <- ds.date_compile(season, startdate = scrape_date, enddate = scrape_date)
  
  if (!is.na(game_output)) {
    
    
  }  
}