library(rvest)
library(dplyr)
library(tidyr)
library(formattable)
library(stringr)
 
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
# Save data frame 
write.csv(finalStatHeadBatting, "finalStatHeadBatting.csv", row.names = FALSE)
 
 
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
 
 
# Editing Base Salaries 2022
# renaming columns
names(BaseSalaries2022) <- c("Player", "Pos", "Age", "Bats", "Throws", "BaseSalary", "Team")
# deleting the first row
BaseSalaries2022 <- BaseSalaries2022[-1,]
# adding NA
BaseSalaries2022 <- apply(BaseSalaries2022, c(1, 2), function(x) ifelse(x == "", NA, x))

# Save to a CSV file
write.csv(BaseSalaries2022, "BaseSalaries2022.csv", row.names = FALSE)

# Editing Base Salaries 2023
# renaming columns
names(BaseSalaries2023) <- c("Player", "Pos", "Age", "Bats", "Throws", "BaseSalary", "Team")
# deleting the first row
BaseSalaries2023 <- BaseSalaries2023[-1,]
# adding NA
BaseSalaries2023 <- apply(BaseSalaries2023, c(1, 2), function(x) ifelse(x == "", NA, x))
# Save to a CSV file
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
