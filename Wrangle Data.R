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

Free.Agency.2022 <- Free.Agency.2022 %>% 
  mutate(PosNumber = case_when(
    POS. %in% c("RP", "SP", "P") ~ 1,
    POS. %in% c("C") ~ 2,
    POS. %in% c("1B") ~ 3,
    POS. %in% c("2B") ~ 4,
    POS. %in% c("3B") ~ 5,
    POS. %in% c("SS") ~ 6,
    POS. %in% c("LF") ~ 7,
    POS. %in% c("CF") ~ 8,
    POS. %in% c("RF") ~ 9,
    POS. %in% c("DH") ~ 10,
    TRUE ~ NA_integer_
  ))

Free.Agency.2023 <- Free.Agency.2023 %>% 
  mutate(PosNumber = case_when(
    POS. %in% c("RP", "SP", "P") ~ 1,
    POS. %in% c("C") ~ 2,
    POS. %in% c("1B") ~ 3,
    POS. %in% c("2B") ~ 4,
    POS. %in% c("3B") ~ 5,
    POS. %in% c("SS") ~ 6,
    POS. %in% c("LF") ~ 7,
    POS. %in% c("CF") ~ 8,
    POS. %in% c("RF") ~ 9,
    POS. %in% c("DH") ~ 10,
    TRUE ~ NA_integer_
  ))

# Splitting dummy code into position numbers
Free.Agency.2022 <- Free.Agency.2022 %>% 
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

#converting to numeric
Free.Agency.2022$YRS <- as.numeric(gsub("[^0-9.]", "", Free.Agency.2022$YRS))
Free.Agency.2023$YRS <- as.numeric(gsub("[^0-9.]", "", Free.Agency.2023$YRS))

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


# Cleaning Data by Omitting NA data 2022
#cleaned_data <- data$column_name[!is.na(data$column_name)]
#Removing NA Salary
Free.Agency.2022_omit <- Free.Agency.2022[!is.na(Free.Agency.2022$AVG..SALARY), ]
#separating out just pitching positions
Free.Agency.2022_omit_ERA <- Free.Agency.2022_omit[Free.Agency.2022_omit$dummy_Pos == 1, ] 
#separating just pitching stats
Free.Agency.2022_omit_ERA <- Free.Agency.2022_omit_ERA %>% select(-H, -RBI, -HR, -AVG, -OPS)
#removing NA War
Free.Agency.2022_omit_ERA <- Free.Agency.2022_omit_ERA[!is.na(Free.Agency.2022_omit_ERA$WAR), ]
#change all NA to 0
Free.Agency.2022_omit_ERA <- replace(Free.Agency.2022_omit_ERA, is.na(Free.Agency.2022_omit_ERA), 0)


#separating out pitching positions
# removing pitchers
# Column and value to remove
column_to_check <- "dummy_Pos"
value_to_remove <- 1
# Identify rows where the specified value is present in the specified column
rows_to_remove <- Free.Agency.2022_omit[, column_to_check] == value_to_remove
Free.Agency.2022_omit_OPS <- Free.Agency.2022_omit[!rows_to_remove, ]
#separating just batting stats
Free.Agency.2022_omit_OPS <- Free.Agency.2022_omit_OPS %>% select(-IP, -ERA, -WHIP, -W, -SV)
#changing NA to 2 for missing dummy years 
Free.Agency.2022_omit_OPS$dummy_Years <- replace(Free.Agency.2022_omit_OPS$dummy_Years, is.na(Free.Agency.2022_omit_OPS$dummy_Years), 2)
#change all NA to 0
Free.Agency.2022_omit_OPS <- replace(Free.Agency.2022_omit_OPS, is.na(Free.Agency.2022_omit_OPS), 0)



# Cleaning Data by Omitting NA data 2023
#cleaned_data <- data$column_name[!is.na(data$column_name)]
#Removing NA Salary
Free.Agency.2023_omit <- Free.Agency.2023[!is.na(Free.Agency.2023$dummy_salary), ]
#separating out just pitching positions
Free.Agency.2023_omit_ERA <- Free.Agency.2023_omit[Free.Agency.2023_omit$dummy_Pos == 1, ] 
#separating just pitching stats
Free.Agency.2023_omit_ERA <- Free.Agency.2023_omit_ERA %>% select(-H, -RBI, -HR, -AVG, -OPS)
#removing NA War
Free.Agency.2023_omit_ERA <- Free.Agency.2023_omit_ERA[!is.na(Free.Agency.2023_omit_ERA$WAR), ]
#change all NA to 0
Free.Agency.2023_omit_ERA <- replace(Free.Agency.2023_omit_ERA, is.na(Free.Agency.2023_omit_ERA), 0)


#separating out pitching positions
# removing pitchers
# Column and value to remove
column_to_check <- "dummy_Pos"
value_to_remove <- 1
# Identify rows where the specified value is present in the specified column
rows_to_remove <- Free.Agency.2023_omit[, column_to_check] == value_to_remove
Free.Agency.2023_omit_OPS <- Free.Agency.2023_omit[!rows_to_remove, ]
#separating just batting stats
Free.Agency.2023_omit_OPS <- Free.Agency.2023_omit_OPS %>% select(-IP, -ERA, -WHIP, -W, -SV)
#removing NA War
Free.Agency.2023_omit_OPS <- Free.Agency.2023_omit_OPS[!is.na(Free.Agency.2023_omit_OPS$WAR), ]
#changing NA to 2 for missing dummy years 
Free.Agency.2023_omit_OPS$dummy_Years <- replace(Free.Agency.2023_omit_OPS$dummy_Years, is.na(Free.Agency.2023_omit_OPS$dummy_Years), 2)
#change all NA to 0
Free.Agency.2023_omit_OPS <- replace(Free.Agency.2023_omit_OPS, is.na(Free.Agency.2023_omit_OPS), 0)




# Renaming Data Sets
Free.Agency.2022.ERA <- Free.Agency.2022_omit_ERA
Free.Agency.2022.OPS <- Free.Agency.2022_omit_OPS
Free.Agency.2023.ERA <- Free.Agency.2023_omit_ERA
Free.Agency.2023.OPS <- Free.Agency.2023_omit_OPS


# Combining ERA and OPS for more data to run in analysis
Free.Agency.ERA <- rbind(Free.Agency.2022.ERA, Free.Agency.2023.ERA)
Free.Agency.OPS <- rbind(Free.Agency.2022.OPS, Free.Agency.2023.OPS)


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
