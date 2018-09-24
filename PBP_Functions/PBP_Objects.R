#Assigns Colors for graphing
df.team_colors <- data.frame(row.names = c("ANA", "ARI", "BOS", "BUF", "CGY", 
                                           "CAR", "CHI", "COL", "CBJ", "DAL", 
                                           "DET", "EDM", "FLA", "L.A", "MIN", 
                                           "MTL", "NSH", "N.J", "NYI", "NYR", 
                                           "OTT", "PHI", "PIT", "STL", "S.J", 
                                           "T.B", "TOR", "VAN", "VGK", "WSH", "WPG"), 
                             Primary = c("#FC4C02", "#8C2633", "#FFB81C", "#041E42", "#C8102E",
                                         "#CC0000", "#C8102E", "#6F263D", "#041E42", "#006341",
                                         "#C8102E", "#041E42", "#041E42", "#000000", "#154734",
                                         "#A6192E", "#FFB81C", "#CE1126", "#00539b", "#0038A8",
                                         "#C8102E", "#FA4616", "#CFC493", "#003087", "#006272",
                                         "#00205B", "#002868", "#00205B", "#B9975B", "#041E42", "#041E42"),
                             Secondary = c("#B09862", "#E2D6B5", "#000000", "#FFB81C", "#F1BE48",
                                           "#000000", "#000000", "#236192", "#C8102E", "#A2AAAD", 
                                           "#FFFFFF", "#FC4C02", "#C8102E", "#A2AAAD", "#A6192E",
                                           "#001E62", "#041E42", "#000000", "#F47D30", "#C8102E",
                                           "#C69214", "#000000", "#000000", "#FFB81C", "#E57200",
                                           "#000000", "#FFFFFF", "#00843D", "#333F48", "#C8102E", "#53565A"),
                             Tiertiary = c("#A4A9AD", "#111111", NA, "#A2AAAD", "#111111", 
                                           "#A2A9AF", "#FFFFFF", "#A2AAAD", "#A2AAAD", "#000000",
                                           "#000000", NA, "#B9975B", "#FFFFFF", "#DDCBA4",
                                           "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF",
                                           "#000000", "#FFFFFF", "#FFB81C", "#041E42", "#000000",
                                           "#FFFFFF", NA, "#97999B", "#C8102E", "#FFFFFF", "#004C97"),
                             Quaternary = c("#111111", NA, NA, "#C8102E", "#FFFFFF",
                                            "#70222C", "#FFD100", "#000000", NA, NA,
                                            NA, NA, NA, NA, "#EAAA00",
                                            NA, NA, NA, NA, NA,
                                            "#FFFFFF", NA, "#FFFFFF", "#FFFFFF", "#FFFFFF",
                                            NA, NA, "#FFFFFF", "#000000", NA, "#A2AAAD"))

#Assign Shapes for Corsi Shot Types
v.corsi_events <- c(16, 17, 1, 0)
names(v.corsi_events) <- c("SHOT", "GOAL", "MISS", "BLOCK")

#Game Score Ranks
c("G" = 0.75,
  "A1" = 0.7,
  "A2" = 0.55,
  "SOG" = 0.075,
  "BLK" = 0.05,
  "PENT" = -0.15,
  "PEND" = 0.15,
  "FOW" = 0.01,
  "FOL" = -0.01,
  "CF_5v5" = 0.05,
  "CA_5v5" = -0.05,
  "GF_5v5" = 0.15,
  "GA_5v5" = -0.15
) ->
  st.game_score_weights

#Assign Shapes for Corsi Shot Types
v.positions <- c(16, 17, 18, 15)
names(v.positions) <- c("C", "L", "R", "D")
