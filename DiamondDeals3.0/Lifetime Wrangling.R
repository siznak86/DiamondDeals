library(rvest)
library(dplyr)
library(tidyr)
library(formattable)
library(stringr)
library(stringi)
library(ggplot2)

# Collecting Batting Life Time of Active Players

urls_batting <- c(
"https://stathead.com/tiny/9LjPk",
"https://stathead.com/tiny/BgUPm",
"https://stathead.com/tiny/d0ZzF",
"https://stathead.com/tiny/YOkVb",
"https://stathead.com/tiny/lQHSh"
)

# Creating a list of Batting Data
list_Batting <- list()

# Iterate over each URL
for (url in urls_batting) {
  
  page <- read_html(url)
  
  Batting <- html_table(html_nodes(page, "table"))[[1]]
  # Append the data frame to the list
  list_Batting <- c(list_Batting, list(Batting))
}
# Combine all data frames
finalBatting <- do.call(rbind, list_Batting)
# Removing Duplicate WAR
finalBatting <- finalBatting[, !duplicated(names(finalBatting))]

# Collecting Pitching Life Time of Active Players

urls_pitching <- c(
  "https://stathead.com/tiny/eIzno",
  "https://stathead.com/tiny/0JXjp",
  "https://stathead.com/tiny/xEXkF",
  "https://stathead.com/tiny/OUAGN",
  "https://stathead.com/tiny/mhnby",
  "https://stathead.com/tiny/cFtz7",
  "https://stathead.com/tiny/O2E4x"
)

# Creating a list of Batting Data
list_Pitching <- list()

# Iterate over each URL
for (url in urls_pitching) {
  
  page <- read_html(url)
  
  Pitching <- html_table(html_nodes(page, "table"))[[1]]
  # Append the data frame to the list
  list_Pitching <- c(list_Pitching, list(Pitching))
}
# Combine all data frames
finalPitching <- do.call(rbind, list_Pitching)
# Removing Duplicate WAR
finalPitching <- finalPitching[, !duplicated(names(finalPitching))]


# Creating a Player ID
# Merging Names
finalPlayers <- merge(finalBatting, finalPitching, by = "Player", all = TRUE)
# Dropping all other rows
finalPlayers <- finalPlayers["Player"]
# Adding ID to all players
finalPlayers <- mutate(finalPlayers, PlayerID = seq_along(Player))

# Add PlayerId to both Data Sets
finalBatting <- merge(finalBatting, finalPlayers, by = "Player")
finalPitching <- merge(finalPitching, finalPlayers, by = "Player")
FreeAgencyBatters <- merge(FreeAgencyBatters, finalPlayers, by = "Player")
FreeAgencyPitchers <- merge(FreeAgencyPitchers, finalPlayers, by = "Player")

# Dropping Erroneous Columns
drop <- c("Rk", "From", "To", "Age", "Pos", "Team")
finalBatting = finalBatting[,!(names(finalBatting) %in% drop)]
finalPitching = finalPitching[,!(names(finalPitching) %in% drop)]

# Selecting Last Free Agency Season
FreeAgencyBatters <- FreeAgencyBatters %>%
  arrange(PlayerID, desc(Season)) %>%
  distinct(PlayerID, .keep_all = TRUE)

FreeAgencyPitchers <- FreeAgencyPitchers %>%
  arrange(PlayerID, desc(Season)) %>%
  distinct(PlayerID, .keep_all = TRUE)

# Adding Salary to Batting and Pitching Data
finalBatting <- merge(finalBatting, FreeAgencyBatters, by = c("Player", "PlayerID"))
finalPitching <- merge(finalPitching, FreeAgencyPitchers, by = c("Player", "PlayerID"))

# Save Batters
write.csv(finalBatting, "finalBatting.csv", row.names = FALSE)
# Save Pitchers
write.csv(finalPitching, "finalPitching.csv", row.names = FALSE)

# Creating a Actual vs Prediction Dataset
Batting <- data.frame(Player = finalBatting$Player, AvgSalary = finalBatting$AvgSalary)
Pitching <- data.frame(Player = finalPitching$Player, AvgSalary = finalPitching$AvgSalary)

# Dropping Ohtani
finalBatting <- subset(finalBatting, Player != "Shohei Ohtani")
Batting <- subset(Batting, Player != "Shohei Ohtani")
finalPitching <- subset(finalPitching, Player != "Shohei Ohtani")
Pitching <- subset(Pitching, Player != "Shohei Ohtani")
