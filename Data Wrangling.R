library(dplyr)

## Import Free Agency Years Data Sets make sure to change YRS, DOLLARS, AVG. SALARY to numeric

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


## Creating Dummy Values
# Basic Positions Pitcher, Infield, Outfield, DH
FreeAgencyTotal <- FreeAgencyTotal %>% 
  mutate(DummyPos = case_when(
    POS. %in% c("RP", "SP", "P") ~ 1,
    POS. %in% c("C", "1B", "2B", "3B", "SS") ~ 2,
    POS. %in% c("LF", "CF", "RF", "OF") ~ 3,
    POS. %in% c("DH") ~ 4,
    TRUE ~ NA_integer_
  ))

# Advance Positions
FreeAgencyTotal <- FreeAgencyTotal %>% 
  mutate(PosNumber = case_when(
    POS. %in% c("RP", "SP", "P") ~ 1,
    POS. %in% c("C") ~ 2,
    POS. %in% c("1B") ~ 3,
    POS. %in% c("2B") ~ 4,
    POS. %in% c("3B") ~ 5,
    POS. %in% c("SS") ~ 6,
    POS. %in% c("LF", "OF") ~ 7,
    POS. %in% c("CF") ~ 8,
    POS. %in% c("RF") ~ 9,
    POS. %in% c("DH") ~ 10,
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

## Dropping Erroneous Columns
drop <- c("POS", "BATS", "THROWS", "FROM", "TO")
FreeAgencyERA = FreeAgencyERA[,!(names(FreeAgencyERA) %in% drop)]

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

## Dropping Erroneous Columns
drop <- c("POS", "BATS", "THROWS", "FROM", "TO")
FreeAgencyOPS = FreeAgencyOPS[,!(names(FreeAgencyOPS) %in% drop)]

## Saving Free Agency OPS Data Set
write.csv(FreeAgencyOPS, "FreeAgencyOPS.csv", row.names = FALSE)
