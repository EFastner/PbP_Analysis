viz.corsi_graph <- function(rawdata, team_colors = df.team_colors, corsi_table = v.corsi_events) {
  #DESCRIPTION - use fun.corsi_table to create a vizualization of corsi during a specified game
  
  #Grab Primary Colors
  df.primary_colors <- as.character(team_colors$Primary)
  names(df.primary_colors) <- row.names(team_colors)
  
  #Utilize enhancedPBP and corsi_table functions to add more columns
  df.enhanced_pbp <- ds.enhancedPBP(rawdata)
  df.corsi_table <- fun.corsi_table(df.enhanced_pbp)
  
  #Set Home and Away Teams
  home_team <- df.corsi_table[[1,"home_team"]]
  away_team <- df.corsi_table[[1,"away_team"]]
  
  viz.corsi_graph <- 
    ggplot(data = df.corsi_table, mapping = aes(x = game_mins, y = team_corsi)) +
    ggtitle(paste("Team Corsi by Minute\n",home_team, " ", max(df.corsi_table$home_score), " vs. ", away_team, " ", max(df.corsi_table$away_score), sep = "")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x = "Game Minute", y = "Corsi", shape = "Shot Type", color = "Team") +
    geom_line(mapping = aes(color = event_team)) +
    geom_point(mapping = aes(shape = event_type, color = event_team), size = 3) +
    geom_text(data = subset(df.corsi_table, event_type == "GOAL"), vjust = -1, hjust = 1, nudge_x = 0, mapping = aes(color = event_team, label = event_player_1)) +
    
    #IN DEVELOPMENT - Need to adjust function to end PP after a PP goal and remove Offsetting Penalties
    #geom_rect(data = df.penalties, mapping = aes(xmin = game_mins, xmax = pen_end, ymin = -Inf, ymax = Inf, color = team_adv), alpha = .05) +
    
    geom_vline(mapping = aes(xintercept = 20), alpha = .5, linetype = "longdash") + 
    geom_vline(mapping = aes(xintercept = 40), alpha = .5, linetype = "longdash") +
    scale_color_manual(values = df.primary_colors) +
    scale_shape_manual(values = corsi_table) +
    scale_fill_manual(values = df.primary_colors)
  
  return(viz.corsi_graph)
}

viz.team_summary <- function(rawdata, corsi_table) {
  #DESCRIPTION - Use fun.team_summary to create a vizualization of team game performance
  
  require(ggplot2)
  
  #Set Home and Away Teams
  home_team <- rawdata[[1,"home_team"]]
  away_team <- rawdata[[1,"away_team"]]
  
  #Grab Primary Colors
  df.primary_colors <- as.character(df.team_colors$Primary)
  names(df.primary_colors) <- row.names(df.team_colors)
  
  df.team_summary <- fun.team_summary(rawdata)
  
  chart.team_summary <- 
    ggplot(data = df.team_summary, mapping = aes(x = factor(Event, levels = c("Blocks", "Hits", "Faceoffs", "Corsi", "Shots", "Goals")), 
                                                 fill = factor(event_team, levels = c(as.character(away_team),as.character(home_team))), 
                                                 y = Value)) +
    
    ggtitle(paste("Team Summary")) +
    labs(fill = "Team") +
    theme(plot.title = element_text(hjust = 0.5), 
          panel.grid = element_blank(), 
          panel.background = element_blank(), 
          axis.ticks = element_blank(), 
          axis.text.y = element_text(size = 11, color = "black"), 
          axis.line.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.title = element_blank()) +
    geom_col(position = "fill") + 
    geom_text(data = subset(df.team_summary, event_team == as.character(home_team)), position = position_fill(vjust = -0.025), mapping = aes(label = Value)) + 
    geom_text(data = subset(df.team_summary, event_team == as.character(away_team)), position = position_fill(vjust = 1.025), mapping = aes(label = Value)) + 
    geom_hline(yintercept = 0.5, linetype = "longdash", alpha = .75) +
    coord_flip() +
    scale_fill_manual(values = df.primary_colors)

  return(chart.team_summary)  
}

viz.corsi_positions <- function(rawdata) {

  #Grab Primary Colors
  df.primary_colors <- as.character(df.team_colors$Primary)
  names(df.primary_colors) <- row.names(df.team_colors)
  
  #Utilize enhancedPBP and corsi_table functions to add more columns
  df.enhanced_pbp <- ds.enhancedPBP(rawdata, v.corsi_events)
  df.corsi_table <- fun.corsi_table(df.enhanced_pbp, v.corsi_events)
  
  #Draw a blank rink and fix coordinates
  rink <- fun.draw_rink() + coord_fixed()
  
  #Add Corsi Points
  chart.corsi_positions <- 
    rink +
    labs(color = "Team", shape = "Shot Type") + 
    geom_point(data = filter(df.corsi_table, !is.na(side_coordsx) & !is.na(side_coordsy)), 
               mapping = aes(x = side_coordsx, 
                             y = side_coordsy,
                             color = event_team,
                             shape = event_type), 
               size = 3) +
    scale_color_manual(values = df.primary_colors) +
    scale_shape_manual(values = v.corsi_events)
  
  return(chart.corsi_positions)
  
}

viz.player_efficiency <- function(skater_summary) {
  #DESCRIPTION - Plot gamescore and TOI by player
  #ARGUMENTS - Function expects a data frame with players summarized with fun.combined_skater_stats() and rosters added with fun.add_roster_data()
  
  require(ggplot2)
  require(ggrepel)
  
  skater_summary$player <- gsub("\\.", " ", skater_summary$player)
  
  df.primary_colors <- as.character(df.team_colors$Primary)
  names(df.primary_colors) <- row.names(df.team_colors)
  
  chart_player_efficiency <- ggplot(data = skater_summary, mapping = aes(x = skater_summary$TOI, y = skater_summary$GS)) +
    geom_point(mapping = aes(color = skater_summary$team, size = skater_summary$iCF, shape = player_position)) +
    labs(x = "Time on Ice", y = "Gamescore", color = "Team", size = "Individual Corsi", shape = "Position") +
    geom_text_repel(mapping = aes(label = skater_summary$player), point.padding = .5, segment.alpha = .3) +
    geom_vline(xintercept = mean(skater_summary$TOI), linetype = "longdash", alpha = .5) +
    geom_text(mapping = aes(x = mean(skater_summary$TOI), y = -Inf, label = round(mean(skater_summary$TOI),digits = 2)), alpha = .1, hjust = -.1, vjust = -1) + 
    geom_hline(yintercept = mean(skater_summary$GS), linetype = "longdash", alpha = .5) +
    geom_text(mapping = aes(x = -Inf, y = round(mean(skater_summary$GS),digits = 2), label = round(mean(skater_summary$GS), digits = 2)), alpha = .1, hjust = -.1, vjust = -1) + 
    scale_size_area(max_size = 10) +
    scale_color_manual(values = df.primary_colors) +
    scale_shape_manual(values = v.positions)
  
 return(chart_player_efficiency) 
}

fun.draw_rink <- function() {
  
  ################################################################################
  ##This code written by Prashanth Iyer who can be found on twitter          #####
  ##@iyer_prashanth and is a really good follow                              #####
  ################################################################################
  
  xseq <- seq(-4, 4, length = 100)
  theta1 <- seq(0, 2 * pi, length = 300)
  theta <- seq(0, 2 * pi, length = 300)
  dd <- (5 + 7 / 12) / 2
  
  ## Blank NHL Rink
  
  rink <- ggplot(data = data.frame(x = 1, y = 1), aes(x, y)) + 
    
    geom_path(data = data.frame(
      x = c(15, 87 + 13 * sin(seq(0, pi / 2, length = 20)), 
            87 + 13 * sin(seq(pi / 2, 0, length = 20)), 15), 
      y = c(-42.5, -42.5 + 15 - 15 * cos(seq(0, pi / 2, length = 20)), 
            42.5 - 15 + 15 * cos(seq(pi / 2, 0, length = 20)), 42.5))) + 
    geom_path(data = data.frame(
      x = c(15, -87 - 13 * sin(seq(0, pi / 2, length = 20)), 
            -87 - 13 * sin(seq(pi / 2, 0, length = 20)), 15), 
      y = c(-42.5, -42.5 + 15 - 15 * cos(seq(0, pi / 2, length = 20)), 
            42.5 - 15 + 15 * cos(seq(pi / 2, 0, length = 20)), 42.5))) + 
    ## Goal Lines
    geom_path(data = data.frame(x = c(89),
                                y = c(42.5 - 15 + sqrt(15^2 - (15 - 11)^2), 
                                      -(42.5 - 15 + sqrt(15^2 - (15 - 11)^2)))), 
              color = 'red') + 
    geom_path(data = data.frame(x = c(-89), 
                                y = c(42.5 - 15 + sqrt(15^2 - (15 - 11)^2), 
                                      -(42.5 - 15 + sqrt(15^2 - (15 - 11)^2)))), 
              color = 'red') +
    ## Nets
    geom_path(data = data.frame(x = c(90, 92, 92, 90)), y = c(-3, -3, 3, 3)) + 
    geom_path(data = data.frame(x = c(-90, -92, -92, -90), y = c(-3,-3, 3, 3))) +
    
    ## Restricted Area
    geom_segment(aes(x = 89, y = -11, xend = 100, yend = -14), color = 'red') + 
    geom_segment(aes(x = 89, y = 11, xend = 100, yend = 14), color = 'red') + 
    geom_segment(aes(x = -89, y = -11, xend = -100, yend = -14), color = 'red') + 
    geom_segment(aes(x = -89, y = 11, xend =-100, yend = 14), color = 'red') +
    
    ## Red Line (Center Ice)
    geom_segment(aes(x = 0, y = -42.5, xend = 0, yend = 42.5), color = 'red', size = 1) +
    
    ## Blue Lines
    geom_segment(aes(x = 25, y = -42.5, xend = 25,  yend = 42.5), color = 'blue', size = 1) + 
    geom_segment(aes(x = -25, y = -42.5, xend = -25,  yend = 42.5), color = 'blue', size = 1) +
    
    ## Crease
    geom_polygon(data = data.frame(x = 1 * c(89, 83+xseq^2 / 4^2 * 1.5, 89),
                                   y = c(-4, xseq, 4)), 
                 color = 'red', fill = 'deepskyblue2') + 
    geom_polygon(data = data.frame(x = -1 * c(89, 83 + xseq^2 / 4^2 * 1.5, 89),
                                   y = c(-4, xseq, 4)), 
                 color = 'red', fill = 'deepskyblue2') +
    
    ## Center Ice Circle
    geom_path(data = data.frame(x = 15 * sin(theta1)), 
              y = 15 * cos(theta1), color = 'deepskyblue2') +
    
    ## Faceoff Dots
    geom_polygon(data = data.frame(y = 22 + 1 * cos(theta), 
                                   x = 20 + 1 * sin(theta)), 
                 color = "red", fill = "red") + 
    geom_polygon(data = data.frame(y = 22 + 1 * cos(theta), 
                                   x = -20 + 1 * sin(theta)), 
                 color = "red", fill = 'red') + 
    geom_polygon(data = data.frame(y = -22 + 1 * cos(theta), 
                                   x = -20 + 1 * sin(theta)), 
                 color = 'red', fill = 'red') + 
    geom_polygon(data = data.frame(y = -22 + 1 * cos(theta), 
                                   x = 20 + 1 * sin(theta)), 
                 color = 'red', fill = 'red') + 
    geom_polygon(data = data.frame(y = 22 + 1 * cos(theta), 
                                   x = -69 + 1 * sin(theta)), 
                 color = 'red', fill = 'red') + 
    geom_polygon(data = data.frame(y = 22 + 1 * cos(theta), 
                                   x = 69 + 1 * sin(theta)), 
                 color = 'red', fill = 'red') + 
    geom_polygon(data = data.frame(y = -22 + 1 * cos(theta), 
                                   x = -69 + 1 * sin(theta)), 
                 color = 'red', fill = 'red') + 
    geom_polygon(data = data.frame(y = -22 + 1 * cos(theta), 
                                   x = 69 + 1 * sin(theta)), 
                 color = 'red', fill = 'red') +
    
    ## Faceoff Circles
    geom_segment(aes(y = 22 - 0.75, x = 69 - 2, 
                     yend = 22 - 0.75, xend = 69 - 6), color = 'red') + 
    geom_segment(aes(y = 22 + 0.75, x = 69 - 2, 
                     yend = 22 + 0.75, xend = 69 - 6), color = 'red') + 
    geom_segment(aes(y = 22 + 0.75, x = 69 + 2, 
                     yend = 22 + 0.75, xend = 69 + 6), color= 'red') + 
    geom_segment(aes(y = 22 - 0.75, x = 69 - 2, 
                     yend = 22 - 0.75, xend = 69 - 6), color = 'red') + 
    geom_segment(aes(y = -22 + 0.75, x = 69 - 2, 
                     yend = -22 + 0.75, xend = 69 - 6), color= 'red') + 
    geom_segment(aes(y = -22 + 0.75, x = 69 + 2, 
                     yend = -22 + 0.75, xend = 69 + 6), color= 'red') + 
    geom_segment(aes(y = -22 - 0.75, x = 69 - 2, 
                     yend = -22 - 0.75, xend = 69 - 6), color = 'red') + 
    geom_segment(aes(y = -22 - 0.75, x = 69 + 2, 
                     yend = -22 - 0.75, xend = 69 + 6), color = 'red') + 
    geom_segment(aes(y = 22 - 0.75, x = 69 + 2, 
                     yend = 22 - 0.75, xend = 69 + 6), color = 'red') + 
    geom_segment(aes(y = 22 + 0.75, x = -69 - 2, 
                     yend = 22 + 0.75, xend = -69 - 6), color = 'red') + 
    geom_segment(aes(y = 22 - 0.75, x = -69 - 2, 
                     yend = 22 - 0.75, xend = -69 - 6), color = 'red') + 
    geom_segment(aes(y = 22 + 0.75, x = -69 + 2, 
                     yend = 22 + 0.75, xend = -69 + 6), color = 'red') + 
    geom_segment(aes(y = -22 + 0.75, x = -69 - 2, 
                     yend = -22 + 0.75, xend = -69 - 6), color = 'red') + 
    geom_segment(aes(y = 22 - 0.75, x = -69 + 2, 
                     yend = 22 - 0.75, xend = -69 + 6), color = 'red') + 
    geom_segment(aes(y = -22 + 0.75, x = -69 + 2, 
                     yend = -22 + 0.75, xend = -69 + 6), color= 'red') + 
    geom_segment(aes(y = -22 - 0.75, x = -69 - 2, 
                     yend = -22 - 0.75, xend = -69 - 6), color = 'red') + 
    geom_segment(aes(y = -22 - 0.75, x = -69 + 2, 
                     yend = -22 - 0.75, xend = -69 + 6), color = 'red') + 
    geom_segment(aes(y = 22 - 15, x = 69 - dd, 
                     yend = 22 - 17, xend = 69 - dd), color = 'red') + 
    geom_segment(aes(y = 22 - 15, x = 69 + dd, 
                     yend = 22 - 17, xend = 69 + dd), color = 'red') + 
    geom_segment(aes(y = 22 + 15, x = 69 + dd, 
                     yend = 22+17, xend = 69 + dd), color = 'red') + 
    geom_segment(aes(y = 22 + 15, x = 69 - dd, 
                     yend = 22 + 17, xend = 69 - dd), color = 'red') + 
    geom_segment(aes(y = -22 + 15, x = 69 - dd, 
                     yend = -22 + 17, xend = 69 - dd), color = 'red') + 
    geom_segment(aes(y = -22 + 15, x = 69 + dd, 
                     yend = -22 + 17, xend = 69 + dd), color = 'red') + 
    geom_segment(aes(y = -22 - 15, x = 69 - dd, 
                     yend = -22 - 17, xend = 69 - dd), color= 'red') + 
    geom_segment(aes(y = -22 - 15, x = 69 + dd, 
                     yend = -22 - 17, xend = 69 + dd), color = 'red') + 
    geom_segment(aes(y = -22 + 15, x = -69 + dd, 
                     yend = -22 + 17, xend = -69 + dd), color = 'red') + 
    geom_segment(aes(y = -22 - 15, x = -69 - dd, 
                     yend = -22 - 17, xend = -69 - dd), color = 'red') + 
    geom_segment(aes(y = -22 - 15, x = -69 + dd, 
                     yend = -22 - 17, xend = -69 + dd), color = 'red') + 
    geom_segment(aes(y = -22 + 15, x = -69 - dd, 
                     yend = -22 + 17, xend = -69 - dd), color = 'red') + 
    geom_segment(aes(y = 22 - 15, x = -69 + dd, 
                     yend = 22 - 17, xend = -69 + dd), color = 'red') + 
    geom_segment(aes(y = 22 - 15, x = -69 - dd, 
                     yend = 22 - 17, xend = -69 - dd), color = 'red') + 
    geom_segment(aes(y = 22 + 15, x = -69 - dd, 
                     yend = 22 + 17, xend = -69 - dd), color = 'red') + 
    geom_segment(aes(y = 22 + 15, x = -69 + dd, 
                     yend = 22 + 17, xend = -69 + dd), color = 'red') + 
    geom_segment(aes(y = 22 + 0.75, x = 69 + 2, 
                     yend = 22 + 3.75, xend = 69 + 2), color = 'red') + 
    geom_segment(aes(y = 22 + 0.75, x = 69 - 2, 
                     yend = 22 + 3.75, xend = 69 - 2), color = 'red') + 
    geom_segment(aes(y = 22 - 0.75, x = 69 + 2, 
                     yend = 22 - 3.75, xend = 69 + 2), color = 'red') + 
    geom_segment(aes(y = 22 - 0.75, x = 69 - 2, 
                     yend = 22 - 3.75, xend = 69 - 2), color = 'red') + 
    geom_segment(aes(y = 22 + 0.75, x = -69 + 2, 
                     yend = 22 + 3.75, xend = -69 + 2), color = 'red') + 
    geom_segment(aes(y = 22 + 0.75, x = -69 - 2, 
                     yend = 22 + 3.75, xend = -69 - 2), color = 'red') + 
    geom_segment(aes(y = 22 - 0.75, x = -69 + 2, 
                     yend = 22 - 3.75, xend = -69 + 2), color = 'red') + 
    geom_segment(aes(y = 22 - 0.75, x = -69 - 2, 
                     yend = 22 - 3.75, xend = -69 - 2), color = 'red') + 
    geom_segment(aes(y = -22 - 0.75, x = -69 + 2, 
                     yend = -22 - 3.75, xend = -69 + 2), color = 'red') + 
    geom_segment(aes(y = -22 - 0.75, x = -69 - 2, 
                     yend = -22 - 3.75, xend = -69 - 2), color = 'red') + 
    geom_segment(aes(y = -22 + 0.75, x = -69 + 2, 
                     yend = -22 + 3.75, xend = -69 + 2), color = 'red') + 
    geom_segment(aes(y = -22 + 0.75, x = -69 - 2, 
                     yend = -22 + 3.75, xend = -69 - 2), color = 'red') + 
    geom_segment(aes(y = -22 + 0.75, x = 69 + 2, 
                     yend = -22 + 3.75, xend = 69 + 2), color = 'red') + 
    geom_segment(aes(y = -22 - 0.75, x = 69 - 2, 
                     yend = -22 - 3.75, xend = 69 - 2), color = 'red') + 
    geom_segment(aes(y = -22 + 0.75, x = 69 - 2, 
                     yend = -22 + 3.75, xend = 69 - 2), color = 'red') + 
    geom_segment(aes(y = -22 - 0.75, x = 69 + 2, 
                     yend = -22 - 3.75, xend = 69 + 2), color = 'red') + 
    geom_path(data = data.frame(y = 22 + 15 * cos(theta), 
                                x = 69 + 15 * sin(theta)), color = 'red') + 
    geom_path(data = data.frame(y = 22 + 15 * cos(theta), 
                                x = -69 + 15 * sin(theta)), color = 'red') + 
    geom_path(data = data.frame(y = -22 + 15 * cos(theta), 
                                x = -69 + 15 * sin(theta)), color = 'red') + 
    geom_path(data = data.frame(y = -22 + 15 * cos(theta), 
                                x = 69 + 15 * sin(theta)), color = 'red') + 
    
    theme_void()
}