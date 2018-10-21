all_seasons <- readfiles("~/Data_Sets/Hockey/Enhanced PbP/Summarized_Seasons", bind = TRUE, sep = ",")

all_seasons$conference <- NA
all_seasons$division <- NA

for (i in 1:nrow(all_seasons)) {
  team = as.character(all_seasons[i, "team"])
  
  if (all_seasons[i, "season"] <= 20102011) {
    all_seasons[i, "conference"] = as.character(df.alignment_20002011[team, "conference"])
    all_seasons[i, "division"] = as.character(df.alignment_20002011[team, "division"])
  } else if (all_seasons[i, "season"] <= 20122013) {
    all_seasons[i, "conference"] = as.character(df.alignment_20112013[team, "conference"])
    all_seasons[i, "division"] = as.character(df.alignment_20112013[team, "division"])
  } else {
    all_seasons[i, "conference"] = as.character(df.alignment_present[team, "conference"])
    all_seasons[i, "division"] = as.character(df.alignment_present[team, "division"])
  }
}
