 library(rvest)
 library(dplyr)
 library(tidyr)
 
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
 # Save to a CSV file
 write.csv(BaseSalaries2022, "BaseSalaries2022.csv", row.names = FALSE)

 # Editing Base Salaries 2023
 # renaming columns
 names(BaseSalaries2023) <- c("Player", "Pos", "Age", "Bats", "Throws", "BaseSalary", "Team")
 # deleting the first row
 BaseSalaries2023 <- BaseSalaries2023[-1,]
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
 
  