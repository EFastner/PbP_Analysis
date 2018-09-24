skater_stats <- fun.skater_summary(raw_game)

combined_stats <- fun.combine_skater_stats(skater_stats)

viz.player_efficiency(combined_stats)