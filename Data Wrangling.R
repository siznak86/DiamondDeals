library(dplyr)

## Creating Player IDs
# Creating a Unified Data Set
FreeAgencyTotal <- bind_rows(FreeAgency2019, FreeAgency2020, FreeAgency2021, FreeAgency2022, FreeAgency2023)

# Dropping all Columns but Player Names
FreeAgencyPlayerNames <- select(FreeAgencyTotal, PLAYER)

# Removing All Duplicate Players
FreeAgencyPlayerNames <- distinct(FreeAgencyPlayerNames, PLAYER, .keep_all = TRUE)

# Adding a Player ID Column
FreeAgencyPlayerNames <- FreeAgencyPlayerNames %>% 
  arrange(PLAYER) %>% 
  mutate(PlayerID = row_number())

# Moving Player ID to First Column
FreeAgencyPlayerNames <- FreeAgencyPlayerNames %>%
  select(PlayerID, everything())

## Adding Player ID to existing Data Sets
# Joining data based on Player and ID
FreeAgencyTotal <- left_join(FreeAgencyTotal, select(FreeAgencyPlayerNames, PLAYER, PlayerID), by = "PLAYER")

# Moving Player ID to First Column
FreeAgencyTotal <- FreeAgencyTotal %>%
  select(PlayerID, everything())

# Making Years Numeric
FreeAgencyTotal$YRS <- as.numeric(gsub("[^0-9.]", "", FreeAgencyTotal$YRS))


## Creating Dummy Values
FreeAgencyTotal <- FreeAgencyTotal %>% 
  mutate(DummyPos = case_when(
    POS. %in% c("RP", "SP", "P") ~ 1,
    POS. %in% c("C", "1B", "2B", "3B", "SS") ~ 2,
    POS. %in% c("LF", "CF", "RF", "OF") ~ 3,
    POS. %in% c("DH") ~ 4,
    TRUE ~ NA_integer_
  ))

## Saving Free Agency Total Data Set
write.csv(FreeAgencyTotal, "FreeAgencyTotal.csv", row.names = FALSE)

## Creating Pitching and Position Data Sets
# Separating out just Pitchers
FreeAgencyERA <- FreeAgencyTotal[FreeAgencyTotal$DummyPos == 1, ]
# Removing Hitting Statistics
FreeAgencyERA <- FreeAgencyERA %>% select(-H, -RBI, -HR, -AVG, -OPS)
# Removing NA Salaries
FreeAgencyERA <- FreeAgencyERA[!is.na(FreeAgencyERA$`AVG. SALARY`), ]
# Dropping NA WAR
FreeAgencyERA <- FreeAgencyERA[!is.na(FreeAgencyERA$WAR), ]
# Changing Saves NA to 0 
FreeAgencyERA <- FreeAgencyERA %>% 
  mutate(SV = ifelse(is.na(SV), 0, SV))
# Changing ERA NA to 0 
FreeAgencyERA <- FreeAgencyERA %>% 
  mutate(ERA = ifelse(is.na(ERA), 0, ERA))
# Changing WHIP NA to 0 
FreeAgencyERA <- FreeAgencyERA %>% 
  mutate(WHIP = ifelse(is.na(WHIP), 0, WHIP))

## Saving Free Agency ERA Data Set
write.csv(FreeAgencyERA, "FreeAgencyERA.csv", row.names = FALSE)

# Separating out Position Players
# Removing Pitchers, Column and value to remove
column_to_check <- "DummyPos"
value_to_remove <- 1
# Identify rows where the specified value is present in the specified column
rows_to_remove <- FreeAgencyTotal[, column_to_check] == value_to_remove
FreeAgencyOPS <- FreeAgencyTotal[!rows_to_remove, ]
#separating just batting stats
FreeAgencyOPS <- FreeAgencyOPS %>% select(-IP, -ERA, -WHIP, -W, -SV)
# Removing NA Salaries
FreeAgencyOPS <- FreeAgencyOPS[!is.na(FreeAgencyOPS$`AVG. SALARY`), ]
# Dropping NA WAR
FreeAgencyOPS <- FreeAgencyOPS[!is.na(FreeAgencyOPS$WAR), ]
# Changing AVG NA to 0
FreeAgencyOPS <- FreeAgencyOPS %>% 
  mutate(AVG = ifelse(is.na(AVG), 0, AVG))
# Changing HR NA to 0
FreeAgencyOPS <- FreeAgencyOPS %>% 
  mutate(HR = ifelse(is.na(HR), 0, HR))
# Changing RBI NA to 0
FreeAgencyOPS <- FreeAgencyOPS %>% 
  mutate(RBI = ifelse(is.na(RBI), 0, RBI))
# Changing H NA to 0
FreeAgencyOPS <- FreeAgencyOPS %>% 
  mutate(H = ifelse(is.na(H), 0, H))

## Saving Free Agency OPS Data Set
write.csv(FreeAgencyOPS, "FreeAgencyOPS.csv", row.names = FALSE)
