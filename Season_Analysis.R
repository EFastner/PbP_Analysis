raw_data <- read.csv("~/Data Sets/Hockey/Enhanced PbP/pbp_20172018.csv", header = TRUE)

wild_games <- filter(raw_data, home_team == "MIN" | away_team == "MIN")

wild_games %>% 
  group_by(home_team, away_team) %>%
  summarize(games_played = n_distinct(game_id)) %>%
  arrange(desc(games_played))

opponents <- 
  wild_games %>% 
  mutate(opp = ifelse(home_team == "MIN", as.character(away_team), as.character(home_team))) %>%
  group_by(opp) %>%
  summarize(games_played = n_distinct(game_id)) %>%
  arrange(desc(games_played))

top_scorers_for <- 
  wild_games %>%
  filter(event_type == "GOAL" & game_period < 5 & event_team == "MIN") %>%
  group_by(event_player_1) %>%
  summarize(goals = n()) %>%
  arrange(desc(goals))

top_scorers_against <- 
  wild_games %>%
  filter(event_type == "GOAL" & game_period < 5 & event_team != "MIN") %>%
  group_by(event_player_1) %>%
  summarize(goals = n()) %>%
  arrange(desc(goals))
