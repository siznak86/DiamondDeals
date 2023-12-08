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
#player datasets
#removing special characters
combinedBattingDataset$CleanPos <- gsub("^[^1-9D]", "", combinedBattingDataset$Pos)
combinedPitchingDataset$CleanPos <- gsub("^[^1-9D]", "", combinedPitchingDataset$Pos)
#Dummy Batting and Pitching for positions
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

#free agency dataset dummy code positions
Free.Agency.2022 <- Free.Agency.2022 %>% 
  mutate(dummy_Pos = case_when(
    POS. %in% c("RP", "SP", "P") ~ 1,
    POS. %in% c("C", "1B", "2B", "3B", "SS") ~ 2,
    POS. %in% c("LF", "CF", "RF", "OF") ~ 3,
    POS. %in% c("DH") ~ 4,
    TRUE ~ NA_integer_
  ))

Free.Agency.2023 <- Free.Agency.2023 %>% 
  mutate(dummy_Pos = case_when(
    POS. %in% c("RP", "SP", "P") ~ 1,
    POS. %in% c("C", "1B", "2B", "3B", "SS") ~ 2,
    POS. %in% c("LF", "CF", "RF", "OF") ~ 3,
    POS. %in% c("DH") ~ 4,
    TRUE ~ NA_integer_
  ))


#BaseSalaries datasets
BaseSalaries2022 <- BaseSalaries2022 %>%
  mutate(dummy_Pos = case_when(
    Pos %in% c("RP", "SP", "P", "RP/CL", "CL", "SP/DH") ~ 1,
    Pos %in% c("C", "1B", "2B", "3B", "SS") ~ 2,
    Pos %in% c("LF", "CF", "RF", "OF") ~ 3,
    Pos %in% c("DH") ~ 4,
    TRUE ~ NA_integer_
  ))

BaseSalaries2023 <- BaseSalaries2023 %>%
  mutate(dummy_Pos = case_when(
    Pos %in% c("RP", "SP", "P", "RP/CL", "CL", "SP/DH") ~ 1,
    Pos %in% c("C", "1B", "2B", "3B", "SS") ~ 2,
    Pos %in% c("LF", "CF", "RF", "OF") ~ 3,
    Pos %in% c("DH") ~ 4,
    TRUE ~ NA_integer_
  ))



# Contract Length
# Short(1-2years) = 1, Long(>2years) = 2
#dummying data into 2 categories

Free.Agency.2022 <- Free.Agency.2022 %>% 
  mutate(dummy_Years = case_when(
    YRS == 1 ~ 1,
    YRS >= 2 ~ 2,
    TRUE ~ NA_integer_
  ))        

Free.Agency.2023 <- Free.Agency.2023 %>% 
  mutate(dummy_Years = case_when(
    YRS == 1 ~ 1,
    YRS >= 2 ~ 2,
    TRUE ~ NA_integer_
  ))        


#pulling out contract years and dummying data into 2 categories
FreeAgency <- FreeAgency %>%
  mutate(contractYears = as.numeric(str_extract(TERMS, "(?<=\\s)\\d+(?=\\syear)")),
         dummy_contract = case_when(
           between(contractYears, 1, 2) ~ 1,
           contractYears > 2 ~ 2,
           TRUE ~ NA_integer_
         ))



# Salary Range
# Low(<$4million) = 1, Median(=$4million-%5million) = 2, High(>$4million) = 3

# Changing column to Numeric and separating into 3 dummy values
Free.Agency.2022 <- Free.Agency.2022 %>%
  mutate(contract_value = as.numeric(gsub("\\$|,", "", AVG..SALARY)),
         dummy_salary = case_when(
           contract_value < 4e6 ~ 1,    # Low
           between(contract_value, 4e6, 5e6) ~ 2,   # Median
           contract_value > 5e6 ~ 3,    # High
           TRUE ~ NA_integer_
         ))
# Changing column to Numeric and separating into 3 dummy values
Free.Agency.2023 <- Free.Agency.2023 %>%
  mutate(contract_value = as.numeric(gsub("\\$|,", "", AVG..SALARY)),
         dummy_salary = case_when(
           contract_value < 4e6 ~ 1,    # Low
           between(contract_value, 4e6, 5e6) ~ 2,   # Median
           contract_value > 5e6 ~ 3,    # High
           TRUE ~ NA_integer_
         ))
# Changing column to Numeric and separating into 3 dummy values
FreeAgency <- FreeAgency %>%
  mutate(contract_value = str_extract(TERMS, "\\$[0-9.]+ million") %>%
           gsub("\\$| million", "", .) %>%
           as.numeric(),
         dummy_value = case_when(
           contract_value < 4 ~ 1,    # Low
           contract_value == 4 ~ 2,   # Median
           contract_value > 4 ~ 3,    # High
           TRUE ~ NA_integer_
         ))
# Changing column to Numeric and separating into 3 dummy values
BaseSalaries2022 <- BaseSalaries2022 %>%
  mutate(contract_value = as.numeric(gsub("\\$|,", "", BaseSalary)),
         dummy_salary = case_when(
           contract_value < 4e6 ~ 1,    # Low
           contract_value == 4e6 ~ 2,   # Median
           contract_value > 4e6 ~ 3,    # High
           TRUE ~ NA_integer_
         ))
# Changing column to Numeric and separating into 3 dummy values
BaseSalaries2023 <- BaseSalaries2023 %>%
  mutate(contract_value = as.numeric(gsub("\\$|,", "", BaseSalary)),
         dummy_salary = case_when(
           contract_value < 4e6 ~ 1,    # Low
           contract_value == 4e6 ~ 2,   # Median
           contract_value > 4e6 ~ 3,    # High
           TRUE ~ NA_integer_
         ))


# Age of Player
# (<25) = 1, (26-34) = 2, (>35) = 3
# Dummy Coding into 3 variables
Free.Agency.2022 <- Free.Agency.2022 %>% 
  mutate(dummy_Age = case_when(
    AGE < 26 ~ 1, 
    between(AGE, 26, 34) ~ 2, 
    AGE > 34 ~ 3,
    TRUE ~ NA_integer_
  ))
# Dummy Coding into 3 variables
Free.Agency.2023 <- Free.Agency.2023 %>% 
  mutate(dummy_Age = case_when(
    AGE < 26 ~ 1, 
    between(AGE, 26, 34) ~ 2, 
    AGE > 34 ~ 3,
    TRUE ~ NA_integer_
  ))

# change column to numeric
BaseSalaries2022$Age <- as.numeric(BaseSalaries2022$Age)
# Dummy Coding into 3 variables
BaseSalaries2022 <- BaseSalaries2022 %>%
  mutate(dummy_age = case_when(
    Age < 26 ~ 1,
    between(Age, 26, 34) ~ 2,
    Age > 34 ~ 3,
    TRUE ~ NA_integer_
  ))

# Dummy Coding into 3 variables
BaseSalaries2023 <- BaseSalaries2023 %>%
  mutate(dummy_age = case_when(
    Age <= 25 ~ 1,
    between(Age, 26, 34) ~ 2,
    Age >= 35 ~ 3,
    TRUE ~ NA_integer_
  ))

# Create a Master Sheet
master_dataset <- PlayerIDs %>%
  left_join(BattingDataset2021, by = "PlayerID") %>%
  left_join(BattingDataset2022, by = "PlayerID") %>%
  left_join(PitchingDataset2021, by = "PlayerID") %>%
  left_join(PitchingDataset2022, by = "PlayerID") %>%
  left_join(BaseSalaries2022, by = "PlayerID") %>%
  left_join(BaseSalaries2023, by = "PlayerID") %>%
  left_join(FreeAgency, by = "PlayerID")

write.csv(master_dataset, "master_data.csv", row.names = FALSE)

# Player ID, Name, Age, Position, WAR, ERA, OPS+, Contract Length, Salary1, Salary2

analysis_dataset <- PlayerIDs %>%
  left_join(BattingDataset2021, by = "PlayerID") %>%
  left_join(BattingDataset2022, by = "PlayerID") %>%
  left_join(PitchingDataset2021, by = "PlayerID") %>%
  left_join(PitchingDataset2022, by = "PlayerID") %>%
  left_join(BaseSalaries2022, by = "PlayerID") %>%
  left_join(BaseSalaries2023, by = "PlayerID") %>%
  left_join(FreeAgency, by = "PlayerID") %>% 
  select(PlayerID, Player, Age, Pos, WAR, ERA, OPS.)


# Save all Data
write.csv(PlayerIDs, "PlayerIDs.csv", row.names = FALSE)
write.csv(FreeAgency, "FreeAgency.csv", row.names = FALSE)
write.csv(Free.Agency.2022, "Free.Agency.2022.csv", row.names = FALSE)
write.csv(Free.Agency.2023, "Free.Agency.2023.csv", row.names = FALSE)
write.csv(combinedBattingDataset, "combinedBattingDataset.csv", row.names = FALSE)
write.csv(BattingDataset2021, "BattingDataset2021.csv", row.names = FALSE)
write.csv(BattingDataset2022, "BattingDataset2022.csv", row.names = FALSE)
write.csv(combinedPitchingDataset, "combinedPitchingDataset.csv", row.names = FALSE)
write.csv(PitchingDataset2021, "PitchingDataset2021.csv", row.names = FALSE)
write.csv(PitchingDataset2022, "PitchingDataset2022.csv", row.names = FALSE)
write.csv(BaseSalaries2022, "BaseSalaries2022.csv", row.names = FALSE)
write.csv(BaseSalaries2023, "BaseSalaries2023.csv", row.names = FALSE)
