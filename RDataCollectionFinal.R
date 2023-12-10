# Adding data and some light wrangling

#install.packages("stringi")
library(rvest)
library(dplyr)
library(tidyr)
library(formattable)
library(stringr)
library(stringi)
 
# Collecting Data on Batting Years of 2021 and 2022
# List of URLs
urls <- c(
   "https://stathead.com/tiny/cLIgJ", #1-200
   "https://stathead.com/tiny/XExlk", #201-400
   "https://stathead.com/tiny/oXSH8", #401-600
   "https://stathead.com/tiny/BHDe3", #601-800
   "https://stathead.com/tiny/0EzOe", #801-1000
   "https://stathead.com/tiny/Xvxi9", #1001-1200
   "https://stathead.com/tiny/ZDxdX", #1201-1400
   "https://stathead.com/tiny/Xqewv", #1401-1600
   "https://stathead.com/tiny/WMzpr", #1601-1800
   "https://stathead.com/tiny/gjoiL", #1801-2000
   "https://stathead.com/tiny/Ryao5", #2001-2200
   "https://stathead.com/tiny/yfmtl", #2201-2400
   "https://stathead.com/tiny/2NA8H", #2401-2600
   "https://stathead.com/tiny/6Pxdl", #2601-2800
   "https://stathead.com/tiny/YHNJR", #2801-3000
   "https://stathead.com/tiny/ZKaQF" #3001-3003
)
 
# Initialize an empty list to store data frames
statHead_list_Batting <- list()
 
# Iterate over each URL
for (url in urls) {
  
  page <- read_html(url)
   
  StatHeadBatting <- html_table(html_nodes(page, "table"))[[1]]
  # Append the data frame to the list
  statHead_list_Batting <- c(statHead_list_Batting, list(StatHeadBatting))
}
# Combine all data frames
finalStatHeadBatting <- do.call(rbind, statHead_list_Batting)

# Separating 2021 and 2022
split_data <- split(finalStatHeadBatting, finalStatHeadBatting$Season)
list_of_dataframes <- lapply(names(split_data), function(year) {
  data.frame(Season = as.integer(year), split_data[[year]])
})

for (year in names(split_data)) {
  assign(paste0("season_", year), split_data[[year]], envir = .GlobalEnv)
}

# Save data frame 
write.csv(season_2021, "StatHeadBatting2021.csv", row.names = FALSE)

#Find Duplicate Names
duplicate_players2021 <- season_2021$Player[duplicated(season_2021$Player)]
View(duplicate_players2021)
#Find Duplicate Names
duplicate_players2022 <- season_2022$Player[duplicated(season_2022$Player)]
View(duplicate_players2022)
# Edit players name in excel

#creating a PlayerID
StatHeadBatting2021 <- StatHeadBatting2021 %>% arrange(Player) %>% mutate(PlayerID = row_number())
#move the Player ID to first column
StatHeadBatting2021 <- StatHeadBatting2021 %>% select(PlayerID, everything())

# Save data frame 
write.csv(season_2021, "StatHeadBatting2021.csv", row.names = FALSE)
write.csv(season_2022, "StatHeadBatting2022.csv", row.names = FALSE)

#joinning for same player ID
# Combine the two datasets and create a mapping dataset
combinedDataset <- bind_rows(
  mutate(StatHeadBatting2021, isNew = FALSE),
  mutate(StatHeadBatting2022, isNew = TRUE)
)
# Duplicate PlayerID for players with the same name
combinedDataset <- combinedDataset %>%
  group_by(Player) %>%
  mutate(PlayerID = ifelse(n() > 1, rep(PlayerID[1], n()), PlayerID))
# save and add new playerids in excel
write.csv(combinedDataset, "combinedBattingDataset.csv", row.names = FALSE)

# create a dataset of just names and player ids
PlayerIDs <- combinedBattingDataset %>% distinct(PlayerID, Player)
# saving as CSV
write.csv(PlayerIDs, "PlayerIDs.csv", row.names = FALSE)

#Clean Data 
replace_special_chars <- function(text) {
  # Replace special characters with their Latin equivalents
  cleaned_text <- iconv(text, to = "ASCII//TRANSLIT")
  return(cleaned_text)
}
#Running replace special characters 
Batting_cleaned <- combinedBattingDataset %>%
  mutate(CleanedPlayer = replace_special_chars(Player))
#merging column
Batting_cleaned <- Batting_cleaned %>% mutate(Player = CleanedPlayer)
# deleting unused columns
combinedBattingDataset_cleaned <- Batting_cleaned %>% select(-isNew, -CleanedPlayer)
# saving
write.csv(combinedBattingDataset_cleaned, "combinedBattingDataset.csv", row.names = FALSE)

# replacing special characters
PlayerID_cleaned <- PlayerIDs %>%
  mutate(CleanedPlayer = replace_special_chars(Player))
#merging columns
PlayerID_cleaned <- PlayerID_cleaned %>% mutate(Player = CleanedPlayer)
# deleting column
PlayerIDs <- PlayerID_cleaned %>% select(-CleanedPlayer)
# saving
write.csv(PlayerIDs, "PlayerIDs.csv", row.names = FALSE)

# create 2021 and 2022 datasets
# filtering by seasons
BattingDataset2021 <- filter(combinedBattingDataset, Season == 2021)
BattingDataset2022 <- filter(combinedBattingDataset, Season == 2022)
#saving
write.csv(BattingDataset2021, "BattingDataset2021.csv", row.names = FALSE)
write.csv(BattingDataset2022, "BattingDataset2022.csv", row.names = FALSE)




#Collecting Data on Pitching Years of 2021 and 2022
# List of URLs
urls <- c(
   "https://stathead.com/tiny/57Jg4", #1-200
   "https://stathead.com/tiny/Od4cz", #201-400
   "https://stathead.com/tiny/h7xWs", #401-600
   "https://stathead.com/tiny/0kDfV", #601-800
   "https://stathead.com/tiny/ghaKV", #801-1000
   "https://stathead.com/tiny/KnYZj", #1001-1200
   "https://stathead.com/tiny/jQvx1", #1201-1400
   "https://stathead.com/tiny/bsXo5", #1401-1600
   "https://stathead.com/tiny/gP0J9" #1601-1780
)
 
# Initialize an empty list to store data frames
statHead_list_Pitching <- list()
 
# Iterate over each URL
for (url in urls) {
  
  page <- read_html(url)
   
  StatHeadPitching <- html_table(html_nodes(page, "table"))[[1]]
   # Append the data frame to the list
  statHead_list_Pitching <- c(statHead_list_Pitching, list(StatHeadPitching))
}
# Combine all data frames
finalStatHeadPitching <- do.call(rbind, statHead_list_Pitching)
# Save to a CSV file
write.csv(finalStatHeadPitching, "finalStatHeadPitching.csv", row.names = FALSE)

# Separating 2021 and 2022
split_data <- split(finalStatHeadPitching, finalStatHeadPitching$Season)
list_of_dataframes <- lapply(names(split_data), function(year) {
  data.frame(Season = as.integer(year), split_data[[year]])
})

for (year in names(split_data)) {
  assign(paste0("seasonp_", year), split_data[[year]], envir = .GlobalEnv)
}

#Find Duplicate Names
duplicate_playersp2021 <- seasonp_2021$Player[duplicated(seasonp_2021$Player)]
View(duplicate_playersp2021)
#Find Duplicate Names
duplicate_playersp2022 <- seasonp_2022$Player[duplicated(seasonp_2022$Player)]
View(duplicate_playersp2022)
# Edit players name in excel

# Save data frame 
write.csv(seasonp_2021, "StatHeadPitching2021.csv", row.names = FALSE)
write.csv(seasonp_2022, "StatHeadPitching2022.csv", row.names = FALSE)
 
#Joining PlayerId to Pitching
StatHeadPitching2021 <- StatHeadPitching2021 %>%
  left_join(PlayerIDs %>% select(PlayerID, Player), by = "Player")

StatHeadPitching2022 <- StatHeadPitching2022 %>%
  left_join(PlayerIDs %>% select(PlayerID, Player), by = "Player")

#move the Player ID to first column
StatHeadPitching2021 <- StatHeadPitching2021 %>% select(PlayerID, everything())
StatHeadPitching2022 <- StatHeadPitching2022 %>% select(PlayerID, everything())
#Saving
write.csv(StatHeadPitching2021, "StatHeadPitching2021.csv", row.names = FALSE)
write.csv(StatHeadPitching2022, "StatHeadPitching2022.csv", row.names = FALSE)

#combining pitching data
combinedPitchingDataset <- bind_rows(StatHeadPitching2021, StatHeadPitching2022)
#Saving
write.csv(combinedPitchingDataset, "combinedPitchingDataset.csv", row.names = FALSE)

#replacing names with cleaned names
cleanedPitching <- combinedPitchingDataset %>%
  left_join(PlayerIDs,  by = "PlayerID") 
# merging columns
cleanedPitching <- cleanedPitching %>% mutate(Player.x = Player.y)
# deleting column
combinedPitchingDataset_cleaned <- cleanedPitching %>% select(-Player.y)
# renaming column
combinedPitchingDataset_cleaned <- combinedPitchingDataset_cleaned %>% rename(Player = Player.x)
# saving
write.csv(combinedPitchingDataset_cleaned, "combinedPitchingDataset.csv", row.names = FALSE)

# create 2021 and 2022 datasets
#filtering data by season
PitchingDataset2021 <- filter(combinedPitchingDataset, Season == 2021)
PitchingDataset2022 <- filter(combinedPitchingDataset, Season == 2022)
# saving
write.csv(PitchingDataset2021, "PitchingDataset2021.csv", row.names = FALSE)
write.csv(PitchingDataset2022, "PitchingDataset2022.csv", row.names = FALSE)





# Editing Base Salaries 2022
# renaming columns
names(BaseSalaries2022) <- c("Player", "Pos", "Age", "Bats", "Throws", "BaseSalary", "Team")
# deleting the first row
BaseSalaries2022 <- BaseSalaries2022[-1,]
# adding NA
BaseSalaries2022 <- apply(BaseSalaries2022, c(1, 2), function(x) ifelse(x == "", NA, x))
# moving playerid column
BaseSalaries2022 <- BaseSalaries2022 %>% select(PlayerID, everything())
# Save to a CSV file
write.csv(BaseSalaries2022, "BaseSalaries2022.csv", row.names = FALSE)

# Cleaning Names
replace_special_chars <- function(text) {
  # Replace special characters with their Latin equivalents
  cleaned_text <- iconv(text, to = "ASCII//TRANSLIT")
  return(cleaned_text)
}
#running replace
BaseSalaries2022_cleaned <- BaseSalaries2022 %>%
  mutate(CleanedPlayer = replace_special_chars(Player))
#merging columns
BaseSalaries2022_cleaned <- BaseSalaries2022_cleaned %>% mutate(Player = CleanedPlayer)
#deleting columns
BaseSalaries2022 <- BaseSalaries2022_cleaned %>% select(-CleanedPlayer)

#Joining PlayerId
BaseSalaries2022 <- BaseSalaries2022 %>%
  left_join(PlayerIDs %>% select(PlayerID, Player), by = "Player")
#saving
write.csv(BaseSalaries2022, "BaseSalaries2022.csv", row.names = FALSE)
# Edit names and Ids in excel
# rename columns
names(BaseSalaries2022) <- c("PlayerID", "Player", "Pos", "Age", "Bats", "Throws", "BaseSalary", "Team")
#deleting first row
BaseSalaries2022 <- BaseSalaries2022[-1,]


# Editing Base Salaries 2023
# renaming columns
names(BaseSalaries2023) <- c("Player", "Pos", "Age", "Bats", "Throws", "BaseSalary", "Team")
# deleting the first row
BaseSalaries2023 <- BaseSalaries2023[-1,]
# adding NA
BaseSalaries2023 <- apply(BaseSalaries2023, c(1, 2), function(x) ifelse(x == "", NA, x))
# Save to a CSV file
write.csv(BaseSalaries2023, "BaseSalaries2023.csv", row.names = FALSE)
# running replace 
BaseSalaries2023_cleaned <- BaseSalaries2023 %>%
  mutate(CleanedPlayer = replace_special_chars(Player))
#merging data
BaseSalaries2023_cleaned <- BaseSalaries2023_cleaned %>% mutate(Player = CleanedPlayer)
#deleting column
BaseSalaries2023 <- BaseSalaries2023_cleaned %>% select(-CleanedPlayer)
# joinning playerid
BaseSalaries2023 <- BaseSalaries2023 %>%
  left_join(PlayerIDs %>% select(PlayerID, Player), by = "Player")
# moving playerid to first column
BaseSalaries2023 <- BaseSalaries2023 %>% select(PlayerID, everything())
# saving
write.csv(BaseSalaries2023, "BaseSalaries2023.csv", row.names = FALSE)
#renaming columns
names(BaseSalaries2023) <- c("PlayerID", "Player", "Pos", "Age", "Bats", "Throws", "BaseSalary", "Team")
# deleting first row
BaseSalaries2023 <- BaseSalaries2023[-1,]
# joining player id
BaseSalaries2023 <- BaseSalaries2023 %>%
  left_join(PlayerIDs %>% select(PlayerID, Player), by = "Player")
#merging and deleting columns
BaseSalaries2023 <- BaseSalaries2023 %>% mutate(PlayerID.x = PlayerID.y)
BaseSalaries2023 <- BaseSalaries2023 %>% select(-PlayerID.y)
BaseSalaries2023 <- BaseSalaries2023 %>% mutate(PlayerID = PlayerID.x)
BaseSalaries2023 <- BaseSalaries2023 %>% select(-PlayerID.x)
#moving playerid to first column
BaseSalaries2023 <- BaseSalaries2023 %>% select(PlayerID, everything())
#saving
write.csv(BaseSalaries2023, "BaseSalaries2023.csv", row.names = FALSE)



 
# Editing Free Agency
# Splitting Name and Date
FreeAgency <- separate(FreeAgency, DATE, into = c("Date", "Name2"), sep = "(?<=\\d)(?=[A-Za-z])", remove = TRUE)
# Splitting Name and Position
FreeAgency <- separate(FreeAgency, NAME, into = c("Name", "Position"), sep = ", ", remove = TRUE)
# Fixing 147
new_data <- c("NOV 11 2022", "Jose Urena", "Jose Urena", "SP", "Signed a 1 year $3.5 million contract with Colorado (COL), including a $4M club option for 2024 ($500,000 buyout)")
FreeAgency[147, ] <- new_data
# deleting extra name column
FreeAgency <- FreeAgency %>% select(-Name2)
# Save to a CSV file
write.csv(FreeAgency, "FreeAgency.csv", row.names = FALSE)


# renaming column
FreeAgency <- FreeAgency %>% rename(Player = Name)
# add playerid
FreeAgency <- FreeAgency %>%
  left_join(PlayerIDs %>% select(PlayerID, Player), by = "Player")
# moving to first column
FreeAgency <- FreeAgency %>% select(PlayerID, everything())

#Changing Position to Pos
FreeAgency <- FreeAgency %>% rename(Pos = Position)
# saving
write.csv(FreeAgency, "FreeAgency.csv", row.names = FALSE)

# Adding playerid to better free agent dataset
#renaming column
Free.Agency.2022 <- Free.Agency.2022 %>% rename(Player = PLAYER)
#joining playerid
Free.Agency.2022<- Free.Agency.2022 %>%
  left_join(PlayerIDs %>% select(PlayerID, Player), by = "Player")
#move to first column
Free.Agency.2022 <- Free.Agency.2022 %>% select(PlayerID, everything())
#save
write.csv(Free.Agency.2022, "Free.Agency.2022.csv", row.names = FALSE)
#renaming column
Free.Agency.2023 <- Free.Agency.2023 %>% rename(Player = PLAYER)
#joining player id
Free.Agency.2023<- Free.Agency.2023 %>%
  left_join(PlayerIDs %>% select(PlayerID, Player), by = "Player")
#moving to first column
Free.Agency.2023 <- Free.Agency.2023 %>% select(PlayerID, everything())
# saving
write.csv(Free.Agency.2023, "Free.Agency.2023.csv", row.names = FALSE)



# Editing 2002 Trades
# combine date
SeasonTrades2022$Date <- as.Date(paste(SeasonTrades2022$X, SeasonTrades2022$X.1, sep = "-"), format = "%d-%b-%Y")
# Delete old columns
SeasonTrades2022 <- SeasonTrades2022 %>% select(-X, -X.1, -X.2)

# Team 1 separating data
# splitting up players positions and cash 1
SeasonTrades2022 <- separate(SeasonTrades2022, Team1Acquired1, into = c("Team1Acquired1", "Team1Position1"), sep = " (?=\\()", extra = "merge")
SeasonTrades2022 <- separate(SeasonTrades2022, Team1Position1, into = c("Team1Position1", "Team1Cash1"), sep = "\\)\\(", remove = TRUE)
# remove ()
SeasonTrades2022$Team1Position1 <- gsub("\\(|\\)", "", SeasonTrades2022$Team1Position1)
SeasonTrades2022$Team1Cash1 <- gsub("[^0-9]", "", SeasonTrades2022$Team1Cash1)

# splitting up players positions and cash 2
SeasonTrades2022 <- separate(SeasonTrades2022, Team1Acquired2, into = c("Team1Acquired2", "Team1Position2"), sep = " (?=\\()", extra = "merge")
SeasonTrades2022 <- separate(SeasonTrades2022, Team1Position2, into = c("Team1Position2", "Team1Cash2"), sep = "\\)\\(", remove = TRUE)
# remove ()
SeasonTrades2022$Team1Position2 <- gsub("\\(|\\)", "", SeasonTrades2022$Team1Position2)
SeasonTrades2022$Team1Cash2 <- gsub("[^0-9]", "", SeasonTrades2022$Team1Cash2)

# splitting up players positions and cash 3
SeasonTrades2022 <- separate(SeasonTrades2022, Team1Acquired3, into = c("Team1Acquired3", "Team1Position3"), sep = " (?=\\()", extra = "merge")
SeasonTrades2022 <- separate(SeasonTrades2022, Team1Position3, into = c("Team1Position3", "Team1Cash3"), sep = "\\)\\(", remove = TRUE)
# remove ()
SeasonTrades2022$Team1Position3 <- gsub("\\(|\\)", "", SeasonTrades2022$Team1Position3)
SeasonTrades2022$Team1Cash3 <- gsub("[^0-9]", "", SeasonTrades2022$Team1Cash3)

# splitting up players positions and cash 4
SeasonTrades2022 <- separate(SeasonTrades2022, Team1Acquired4, into = c("Team1Acquired4", "Team1Position4"), sep = " (?=\\()", extra = "merge")
SeasonTrades2022 <- separate(SeasonTrades2022, Team1Position4, into = c("Team1Position4", "Team1Cash4"), sep = "\\)\\(", remove = TRUE)
# remove ()
SeasonTrades2022$Team1Position4 <- gsub("\\(|\\)", "", SeasonTrades2022$Team1Position4)
SeasonTrades2022$Team1Cash4 <- gsub("[^0-9]", "", SeasonTrades2022$Team1Cash4)

# splitting up players positions and cash 5
SeasonTrades2022 <- separate(SeasonTrades2022, Team1Acquired5, into = c("Team1Acquired5", "Team1Position5"), sep = " (?=\\()", extra = "merge")
SeasonTrades2022 <- separate(SeasonTrades2022, Team1Position5, into = c("Team1Position5", "Team1Cash5"), sep = "\\)\\(", remove = TRUE)
# remove ()
SeasonTrades2022$Team1Position5 <- gsub("\\(|\\)", "", SeasonTrades2022$Team1Position5)
SeasonTrades2022$Team1Cash5 <- gsub("[^0-9]", "", SeasonTrades2022$Team1Cash5)

# splitting up players positions and cash 6
SeasonTrades2022 <- separate(SeasonTrades2022, Team1Acquired6, into = c("Team1Acquired6", "Team1Position6"), sep = " (?=\\()", extra = "merge")
SeasonTrades2022 <- separate(SeasonTrades2022, Team1Position6, into = c("Team1Position6", "Team1Cash6"), sep = "\\)\\(", remove = TRUE)
# remove ()
SeasonTrades2022$Team1Position6 <- gsub("\\(|\\)", "", SeasonTrades2022$Team1Position6)
SeasonTrades2022$Team1Cash6 <- gsub("[^0-9]", "", SeasonTrades2022$Team1Cash6)

# Team 2 separating data
# rename column
names(SeasonTrades2022)[names(SeasonTrades2022) == "Team2.Acquired1"] <- "Team2Acquired1"
# splitting up players positions and cash 1
SeasonTrades2022 <- separate(SeasonTrades2022, Team2Acquired1, into = c("Team2Acquired1", "Team2Position1"), sep = " (?=\\()", extra = "merge")
SeasonTrades2022 <- separate(SeasonTrades2022, Team2Position1, into = c("Team2Position1", "Team2Cash1"), sep = "\\)\\(", remove = TRUE)
# remove ()
SeasonTrades2022$Team2Position1 <- gsub("\\(|\\)", "", SeasonTrades2022$Team2Position1)
SeasonTrades2022$Team2Cash1 <- gsub("[^0-9]", "", SeasonTrades2022$Team2Cash1)

# splitting up players positions and cash 2
SeasonTrades2022 <- separate(SeasonTrades2022, Team2Acquired2, into = c("Team2Acquired2", "Team2Position2"), sep = " (?=\\()", extra = "merge")
SeasonTrades2022 <- separate(SeasonTrades2022, Team2Position2, into = c("Team2Position2", "Team2Cash2"), sep = "\\)\\(", remove = TRUE)
# remove ()
SeasonTrades2022$Team2Position2 <- gsub("\\(|\\)", "", SeasonTrades2022$Team2Position2)
SeasonTrades2022$Team2Cash2 <- gsub("[^0-9]", "", SeasonTrades2022$Team2Cash2)

# splitting up players positions and cash 3
SeasonTrades2022 <- separate(SeasonTrades2022, Team2Acquired3, into = c("Team2Acquired3", "Team2Position3"), sep = " (?=\\()", extra = "merge")
SeasonTrades2022 <- separate(SeasonTrades2022, Team2Position3, into = c("Team2Position3", "Team2Cash3"), sep = "\\)\\(", remove = TRUE)
# remove ()
SeasonTrades2022$Team2Position3 <- gsub("\\(|\\)", "", SeasonTrades2022$Team2Position3)
SeasonTrades2022$Team2Cash3 <- gsub("[^0-9]", "", SeasonTrades2022$Team2Cash3)

# splitting up players positions and cash 4
SeasonTrades2022 <- separate(SeasonTrades2022, Team2Acquired4, into = c("Team2Acquired4", "Team2Position4"), sep = " (?=\\()", extra = "merge")
SeasonTrades2022 <- separate(SeasonTrades2022, Team2Position4, into = c("Team2Position4", "Team2Cash4"), sep = "\\)\\(", remove = TRUE)
# remove ()
SeasonTrades2022$Team2Position4 <- gsub("\\(|\\)", "", SeasonTrades2022$Team2Position4)
SeasonTrades2022$Team2Cash4 <- gsub("[^0-9]", "", SeasonTrades2022$Team2Cash4)

# splitting up players positions and cash 5
SeasonTrades2022 <- separate(SeasonTrades2022, Team2Acquired5, into = c("Team2Acquired5", "Team2Position5"), sep = " (?=\\()", extra = "merge")
SeasonTrades2022 <- separate(SeasonTrades2022, Team2Position5, into = c("Team2Position5", "Team2Cash5"), sep = "\\)\\(", remove = TRUE)
# remove ()
SeasonTrades2022$Team2Position5 <- gsub("\\(|\\)", "", SeasonTrades2022$Team2Position5)
SeasonTrades2022$Team2Cash5 <- gsub("[^0-9]", "", SeasonTrades2022$Team2Cash5)

# splitting up players positions and cash 6
SeasonTrades2022 <- separate(SeasonTrades2022, Team2Acquired6, into = c("Team2Acquired6", "Team2Position6"), sep = " (?=\\()", extra = "merge")
SeasonTrades2022 <- separate(SeasonTrades2022, Team2Position6, into = c("Team2Position6", "Team2Cash6"), sep = "\\)\\(", remove = TRUE)
# remove ()
SeasonTrades2022$Team2Position6 <- gsub("\\(|\\)", "", SeasonTrades2022$Team2Position6)
SeasonTrades2022$Team2Cash6 <- gsub("[^0-9]", "", SeasonTrades2022$Team2Cash6)

# converting Team*Cash* to numeric currency
# Convert the character column to numeric (remove "$" and ",")
SeasonTrades2022$Team1Cash1 <- as.numeric(gsub("[^0-9.]", "", SeasonTrades2022$Team1Cash1))

# Format the numeric column as currency
# Team 1
SeasonTrades2022$Team1Cash1 <- currency(SeasonTrades2022$Team1Cash1)

SeasonTrades2022$Team1Cash2 <- as.numeric(gsub("[^0-9.]", "", SeasonTrades2022$Team1Cash2))
SeasonTrades2022$Team1Cash2 <- currency(SeasonTrades2022$Team1Cash2)

SeasonTrades2022$Team1Cash3 <- as.numeric(gsub("[^0-9.]", "", SeasonTrades2022$Team1Cash3))
SeasonTrades2022$Team1Cash3 <- currency(SeasonTrades2022$Team1Cash3)

SeasonTrades2022$Team1Cash4 <- as.numeric(gsub("[^0-9.]", "", SeasonTrades2022$Team1Cash4))
SeasonTrades2022$Team1Cash4 <- currency(SeasonTrades2022$Team1Cash4)

SeasonTrades2022$Team1Cash5 <- as.numeric(gsub("[^0-9.]", "", SeasonTrades2022$Team1Cash5))
SeasonTrades2022$Team1Cash5 <- currency(SeasonTrades2022$Team1Cash5)

SeasonTrades2022$Team1Cash6 <- as.numeric(gsub("[^0-9.]", "", SeasonTrades2022$Team1Cash6))
SeasonTrades2022$Team1Cash6 <- currency(SeasonTrades2022$Team1Cash6)
# Format the numeric column as currency
# Team 2
SeasonTrades2022$Team2Cash1 <- as.numeric(gsub("[^0-9.]", "", SeasonTrades2022$Team2Cash1))
SeasonTrades2022$Team2Cash1 <- currency(SeasonTrades2022$Team2Cash1)

SeasonTrades2022$Team2Cash2 <- as.numeric(gsub("[^0-9.]", "", SeasonTrades2022$Team2Cash2))
SeasonTrades2022$Team2Cash2 <- currency(SeasonTrades2022$Team2Cash2)

SeasonTrades2022$Team2Cash3 <- as.numeric(gsub("[^0-9.]", "", SeasonTrades2022$Team2Cash3))
SeasonTrades2022$Team2Cash3 <- currency(SeasonTrades2022$Team2Cash3)

SeasonTrades2022$Team2Cash4 <- as.numeric(gsub("[^0-9.]", "", SeasonTrades2022$Team2Cash4))
SeasonTrades2022$Team2Cash4 <- currency(SeasonTrades2022$Team2Cash4)

SeasonTrades2022$Team2Cash5 <- as.numeric(gsub("[^0-9.]", "", SeasonTrades2022$Team2Cash5))
SeasonTrades2022$Team2Cash5 <- currency(SeasonTrades2022$Team2Cash5)

SeasonTrades2022$Team2Cash6 <- as.numeric(gsub("[^0-9.]", "", SeasonTrades2022$Team2Cash6))
SeasonTrades2022$Team2Cash6 <- currency(SeasonTrades2022$Team2Cash6)

#Adding NA to empty cells
SeasonTrades2022 <- apply(SeasonTrades2022, c(1, 2), function(x) ifelse(x == "", NA, x))

# Save to a CSV file
write.csv(SeasonTrades2022, "SeasonTrades2022.csv", row.names = FALSE)


# Editing 2023 Trades
# combine date
SeasonTrades2023$Date <- as.Date(paste(SeasonTrades2023$X, SeasonTrades2023$X.1, sep = "-"), format = "%d-%b-%Y")
# Delete old columns
SeasonTrades2023 <- SeasonTrades2023 %>% select(-X, -X.1)

# Team 1 separating data
# splitting up players positions and cash 1
SeasonTrades2023 <- separate(SeasonTrades2023, Team1Acquired1, into = c("Team1Acquired1", "Team1Position1"), sep = " (?=\\()", extra = "merge")
SeasonTrades2023 <- separate(SeasonTrades2023, Team1Position1, into = c("Team1Position1", "Team1Cash1"), sep = "\\)\\(", remove = TRUE)
# remove ()
SeasonTrades2023$Team1Position1 <- gsub("\\(|\\)", "", SeasonTrades2023$Team1Position1)
SeasonTrades2023$Team1Cash1 <- gsub("[^0-9]", "", SeasonTrades2023$Team1Cash1)

# splitting up players positions and cash 2
SeasonTrades2023 <- separate(SeasonTrades2023, Team1Acquired2, into = c("Team1Acquired2", "Team1Position2"), sep = " (?=\\()", extra = "merge")
SeasonTrades2023 <- separate(SeasonTrades2023, Team1Position2, into = c("Team1Position2", "Team1Cash2"), sep = "\\)\\(", remove = TRUE)
# remove ()
SeasonTrades2023$Team1Position2 <- gsub("\\(|\\)", "", SeasonTrades2023$Team1Position2)
SeasonTrades2023$Team1Cash2 <- gsub("[^0-9]", "", SeasonTrades2023$Team1Cash2)

# splitting up players positions and cash 3
SeasonTrades2023 <- separate(SeasonTrades2023, Team1Acquired3, into = c("Team1Acquired3", "Team1Position3"), sep = " (?=\\()", extra = "merge")
SeasonTrades2023 <- separate(SeasonTrades2023, Team1Position3, into = c("Team1Position3", "Team1Cash3"), sep = "\\)\\(", remove = TRUE)
# remove ()
SeasonTrades2023$Team1Position3 <- gsub("\\(|\\)", "", SeasonTrades2023$Team1Position3)
SeasonTrades2023$Team1Cash3 <- gsub("[^0-9]", "", SeasonTrades2023$Team1Cash3)

# splitting up players positions and cash 4
SeasonTrades2023 <- separate(SeasonTrades2023, Team1Acquired4, into = c("Team1Acquired4", "Team1Position4"), sep = " (?=\\()", extra = "merge")
SeasonTrades2023 <- separate(SeasonTrades2023, Team1Position4, into = c("Team1Position4", "Team1Cash4"), sep = "\\)\\(", remove = TRUE)
# remove ()
SeasonTrades2023$Team1Position4 <- gsub("\\(|\\)", "", SeasonTrades2023$Team1Position4)
SeasonTrades2023$Team1Cash4 <- gsub("[^0-9]", "", SeasonTrades2023$Team1Cash4)

# splitting up players positions and cash 5
SeasonTrades2023 <- separate(SeasonTrades2023, Team1Acquired5, into = c("Team1Acquired5", "Team1Position5"), sep = " (?=\\()", extra = "merge")
SeasonTrades2023 <- separate(SeasonTrades2023, Team1Position5, into = c("Team1Position5", "Team1Cash5"), sep = "\\)\\(", remove = TRUE)
# remove ()
SeasonTrades2023$Team1Position5 <- gsub("\\(|\\)", "", SeasonTrades2023$Team1Position5)
SeasonTrades2023$Team1Cash5 <- gsub("[^0-9]", "", SeasonTrades2023$Team1Cash5)

# Team 2 separating data
# splitting up players positions and cash 1
SeasonTrades2023 <- separate(SeasonTrades2023, Team2Acquired1, into = c("Team2Acquired1", "Team2Position1"), sep = " (?=\\()", extra = "merge")
SeasonTrades2023 <- separate(SeasonTrades2023, Team2Position1, into = c("Team2Position1", "Team2Cash1"), sep = "\\)\\(", remove = TRUE)
# remove ()
SeasonTrades2023$Team2Position1 <- gsub("\\(|\\)", "", SeasonTrades2023$Team2Position1)
SeasonTrades2023$Team2Cash1 <- gsub("[^0-9]", "", SeasonTrades2023$Team2Cash1)

# splitting up players positions and cash 2
SeasonTrades2023 <- separate(SeasonTrades2023, Team2Acquired2, into = c("Team2Acquired2", "Team2Position2"), sep = " (?=\\()", extra = "merge")
SeasonTrades2023 <- separate(SeasonTrades2023, Team2Position2, into = c("Team2Position2", "Team2Cash2"), sep = "\\)\\(", remove = TRUE)
# remove ()
SeasonTrades2023$Team2Position2 <- gsub("\\(|\\)", "", SeasonTrades2023$Team2Position2)
SeasonTrades2023$Team2Cash2 <- gsub("[^0-9]", "", SeasonTrades2023$Team2Cash2)

# splitting up players positions and cash 3
SeasonTrades2023 <- separate(SeasonTrades2023, Team2Acquired3, into = c("Team2Acquired3", "Team2Position3"), sep = " (?=\\()", extra = "merge")
SeasonTrades2023 <- separate(SeasonTrades2023, Team2Position3, into = c("Team2Position3", "Team2Cash3"), sep = "\\)\\(", remove = TRUE)
# remove ()
SeasonTrades2023$Team2Position3 <- gsub("\\(|\\)", "", SeasonTrades2023$Team2Position3)
SeasonTrades2023$Team2Cash3 <- gsub("[^0-9]", "", SeasonTrades2023$Team2Cash3)

# converting Team*Cash* to numeric currency
# Convert the character column to numeric (remove "$" and ",")
SeasonTrades2023$Team1Cash1 <- as.numeric(gsub("[^0-9.]", "", SeasonTrades2023$Team1Cash1))
# Format the numeric column as currency
# Team 1
SeasonTrades2023$Team1Cash1 <- currency(SeasonTrades2023$Team1Cash1)

SeasonTrades2023$Team1Cash2 <- as.numeric(gsub("[^0-9.]", "", SeasonTrades2023$Team1Cash2))
SeasonTrades2023$Team1Cash2 <- currency(SeasonTrades2023$Team1Cash2)

SeasonTrades2023$Team1Cash3 <- as.numeric(gsub("[^0-9.]", "", SeasonTrades2023$Team1Cash3))
SeasonTrades2023$Team1Cash3 <- currency(SeasonTrades2023$Team1Cash3)

SeasonTrades2023$Team1Cash4 <- as.numeric(gsub("[^0-9.]", "", SeasonTrades2023$Team1Cash4))
SeasonTrades2023$Team1Cash4 <- currency(SeasonTrades2023$Team1Cash4)

SeasonTrades2023$Team1Cash5 <- as.numeric(gsub("[^0-9.]", "", SeasonTrades2023$Team1Cash5))
SeasonTrades2023$Team1Cash5 <- currency(SeasonTrades2023$Team1Cash5)

SeasonTrades2023$Team1Cash6 <- as.numeric(gsub("[^0-9.]", "", SeasonTrades2023$Team1Cash6))
SeasonTrades2023$Team1Cash6 <- currency(SeasonTrades2023$Team1Cash6)
# Format the numeric column as currency
# Team 2
SeasonTrades2023$Team2Cash1 <- as.numeric(gsub("[^0-9.]", "", SeasonTrades2023$Team2Cash1))
SeasonTrades2023$Team2Cash1 <- currency(SeasonTrades2023$Team2Cash1)

SeasonTrades2023$Team2Cash2 <- as.numeric(gsub("[^0-9.]", "", SeasonTrades2023$Team2Cash2))
SeasonTrades2023$Team2Cash2 <- currency(SeasonTrades2023$Team2Cash2)

SeasonTrades2023$Team2Cash3 <- as.numeric(gsub("[^0-9.]", "", SeasonTrades2023$Team2Cash3))
SeasonTrades2023$Team2Cash3 <- currency(SeasonTrades2023$Team2Cash3)

#Adding NA to empty cells
SeasonTrades2023 <- apply(SeasonTrades2023, c(1, 2), function(x) ifelse(x == "", NA, x))

# Save to a CSV file
write.csv(SeasonTrades2023, "SeasonTrades2023.csv", row.names = FALSE)


# Making all PlayerID Columns Numeric
BaseSalaries2022$PlayerID <- as.numeric(BaseSalaries2022$PlayerID)
BaseSalaries2023$PlayerID <- as.numeric(BaseSalaries2023$PlayerID)
BattingDataset2021$PlayerID <- as.numeric(BattingDataset2021$PlayerID)
BattingDataset2022$PlayerID <- as.numeric(BattingDataset2022$PlayerID)
combinedBattingDataset$PlayerID <- as.numeric(combinedBattingDataset$PlayerID)
combinedPitchingDataset$PlayerID <- as.numeric(combinedPitchingDataset$PlayerID)
FreeAgency$PlayerID <- as.numeric(FreeAgency$PlayerID)
PitchingDataset2021$PlayerID <- as.numeric(PitchingDataset2021$PlayerID)
PitchingDataset2022$PlayerID <- as.numeric(PitchingDataset2022$PlayerID)
PlayerIDs$PlayerID <- as.numeric(PlayerIDs$PlayerID)
