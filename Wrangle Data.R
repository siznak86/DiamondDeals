# Wrangling Data

# Independent Variables are all Continuous
# Dependent Variables need to be subset into Categorical Levels

library(rvest)
library(dplyr)
library(tidyr)
library(formattable)
library(stringr)
library(stringi)

# Player Position
# Simple
# Pitcher = 1, Infielder = 2, Outfielder = 3, Designated Hitter = 4
# Complex
# Could change P = 1, C = 2, 1B = 3, 2B = 4, 3B = 5, SS = 6, LF = 7, CF = 8, RF = 9, DH = 10, RP = 11
#player datasets
#removing special characters
combinedBattingDataset$CleanPos <- gsub("^[^1-9D]", "", combinedBattingDataset$Pos)
combinedPitchingDataset$CleanPos <- gsub("^[^1-9D]", "", combinedPitchingDataset$Pos)
#Dummy coding data
combinedBattingDataset <- combinedBattingDataset %>% mutate(SimplePos = case_when(
  substr(CleanPos, 1, 1) %in% c("1") ~ 1,
  substr(CleanPos, 1, 1) %in% c("2", "3", "4", "5", "6") ~ 2,
  substr(CleanPos, 1, 1) %in% c("7", "8", "9") ~ 3,
  substr(CleanPos, 1, 1) %in% c("D") ~ 4,
  TRUE ~ NA_integer_
))
combinedPitchingDataset <- combinedPitchingDataset %>% mutate(SimplePos = case_when(
  substr(CleanPos, 1, 1) %in% c("1") ~ 1,
  substr(CleanPos, 1, 1) %in% c("2", "3", "4", "5", "6") ~ 2,
  substr(CleanPos, 1, 1) %in% c("7", "8", "9") ~ 3,
  substr(CleanPos, 1, 1) %in% c("D") ~ 4,
  TRUE ~ NA_integer_
))

#free agency dataset
FreeAgency <- FreeAgency %>%
  mutate(dummy_Pos = case_when(
    Pos %in% c("RP", "SP", "P") ~ 1,
    Pos %in% c("C", "1B", "2B", "3B", "SS") ~ 2,
    Pos %in% c("LF", "CF", "RF", "OF") ~ 3,
    Pos %in% c("DH") ~ 4,
    TRUE ~ NA_integer_
  ))



# Contract Length
# Short(1-2years) = 1, Long(>2years) = 2
#pulling out contract years and dummying data into 2 categories
FreeAgency <- FreeAgency %>%
  mutate(contractYears = as.numeric(str_extract(TERMS, "(?<=\\s)\\d+(?=\\syear)")),
         dummy_contract = case_when(
           between(contractYears, 1, 2) ~ 1,
           contractYears > 2 ~ 2,
           TRUE ~ NA_integer_
         ))



# Salary Range
# Low(<$4million) = 1, Median(=$4million) = 2, High(>$4million) = 3







# Age of Player
# (16-25), (26-34), (35-43)







# Create a Master Sheet
# Player ID, Name, Age, Position, WAR, ERA, OPS+, Contract Length, Salary1, Salary2

PlayerIDs_Select <- PlayerIDs
FreeAgency_Select <- FreeAgency[c("PlayerID", "Pos", "contractYears")]



merged_df <- merge(merge(PlayerIDs, FreeAgency, by = c("PlayerID"), all = TRUE), BaseSalaries2022, by = "PlayerID", all = TRUE)
merged_df$Position <- coalesce(merged_df$Pos.x, merged_df$Pos.y)


# Save all Data
write.csv(FreeAgency, "FreeAgency.csv", row.names = FALSE)
write.csv(combinedBattingDataset, "combinedBattingDataset.csv", row.names = FALSE)
write.csv(combinedPitchingDataset, "combinedPitchingDataset.csv", row.names = FALSE)
write.csv(BaseSalaries2022, "BaseSalaries2022.csv", row.names = FALSE)
write.csv(BaseSalaries2023, "BaseSalaries2023.csv", row.names = FALSE)