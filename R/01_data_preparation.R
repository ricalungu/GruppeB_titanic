# Title: Titanic Preprocessing
# Authors: Meriam & Rica
# Description: 
#  - read dataset titanic.csv
#  - create new columns title and modify existing columns
#  - remove columns which are not needed for further analysis
#  - save adjusted dataset as titanic_modified.csv


df <- read.csv("data/raw/titanic.csv", stringsAsFactors = FALSE)
str(df)
summary(df)

titanic_modified <- df


## install required packages if necessary 
#install.packages(c("tidyr","dplyr","stringr"))

library("tidyr")
library("dplyr")
library("stringr")

###################### recode Pclass as ordered factor ######################

# check if column Pclass only contains numbers
check_Pclass <- unique(titanic_modified$Pclass)

# recode
titanic_modified$Pclass <- ordered(titanic_modified$Pclass)

##############################################################################

########## create new columns side_ship and deck from column Cabin ###########

# check entries in column Cabin to get an overview
check_Cabin <- sort(unique(titanic_modified$Cabin))
check_Cabin <- as.data.frame(check_Cabin)


# replace missing values in Cabin with NA
titanic_modified$Cabin[titanic_modified$Cabin == ""] <- NA 

# separate column Cabin by " " as there are cells with up to four cabin numbers
titanic_modified <- titanic_modified %>%
  separate(Cabin, into = c("cabin_1", "cabin_2", "cabin_3", "cabin_4"), sep = " ", remove = F)

### create new column side_ship

# extract number from cabin number
# create column cabin_1_side (and columns cabin_2_side, cabin_3_side, cabin_4_side respectively)
# if cabin_1 is NA or contains only one character (first character is always a letter), assign value NA
# else assign substring from second to last character as numeric value (number from string)
titanic_modified <- titanic_modified %>%
  mutate(cabin_1_side = case_when(
    is.na(cabin_1) | substring(cabin_1, 2) == "" ~ NA,
    TRUE ~ as.numeric(substring(cabin_1, 2)))) %>%
  mutate(cabin_2_side = case_when(
    is.na(cabin_2) | substring(cabin_2, 2) == "" ~ NA,
    TRUE ~ as.numeric(substring(cabin_2, 2)))) %>%
  mutate(cabin_3_side = case_when(
    is.na(cabin_3) | substring(cabin_3, 2) == "" ~ NA,
    TRUE ~ as.numeric(substring(cabin_3, 2)))) %>%
  mutate(cabin_4_side = case_when(
    is.na(cabin_4) | substring(cabin_4, 2) == "" ~ NA,
    TRUE ~ as.numeric(substring(cabin_4, 2)))) 
  
# replace NA's from cabin_2_side with value from cabin_1_side 
# to allow comparisons between all four columns
# -> proceed with columns cabin_3_side, cabin_4_side accordingly
# (preparation for following step)
titanic_modified$cabin_2_side[is.na(titanic_modified$cabin_2_side)] <- titanic_modified$cabin_1_side[is.na(titanic_modified$cabin_2_side)]
titanic_modified$cabin_3_side[is.na(titanic_modified$cabin_3_side)] <- titanic_modified$cabin_2_side[is.na(titanic_modified$cabin_3_side)]
titanic_modified$cabin_4_side[is.na(titanic_modified$cabin_4_side)] <- titanic_modified$cabin_3_side[is.na(titanic_modified$cabin_4_side)]

# create column side_ship containing either Backbord or Steuerbord (or NA)

# if multiple cabin numbers exist for one person: 
#     - if all of the numeric values from the cabin numbers are odd: Steuerbord
#     - if all of the numeric values from the cabin numbers are even: Backbord
# if one cabin number exists for a person:
#     - numeric value is odd: Steuerbord
#     - numeric value is even: Backbord
# else: NA (e.g. multiple cabin numbers but not all of them contain numeric values)

titanic_modified <- titanic_modified %>%# 
  mutate(side_ship = case_when(
    !is.na(cabin_1_side) 
    & (cabin_1_side %% 2 == 0) 
    & (cabin_2_side %% 2 == 0) 
    & (cabin_3_side %% 2 == 0) 
    & (cabin_4_side %% 2 == 0) ~ "Backbord",
    !is.na(cabin_1_side) 
    & (cabin_1_side %% 2 == 1) 
    & (cabin_2_side %% 2 == 1) 
    & (cabin_3_side %% 2 == 1) 
    & (cabin_4_side %% 2 == 1)~ "Steuerbord",
    TRUE ~ NA
  )) %>% 
  # remove unnecessary columns
  select(-c(cabin_1_side,cabin_2_side, cabin_3_side, cabin_4_side))


### create new column deck

# extract letter from columns cabin_1 to cabin_4 
# and create columns cabin_1_deck to cabin_4_deck 
# (first character from cabin number corresponds to deck)

titanic_modified <- titanic_modified %>%
  mutate(cabin_1_deck = case_when(
    is.na(cabin_1) | substring(cabin_1, 1,1) == "" ~ NA,
    TRUE ~ substring(cabin_1, 1,1))
  ) %>%
  mutate(cabin_2_deck = case_when(
    is.na(cabin_2) | substring(cabin_2, 1,1) == "" ~ NA,
    TRUE ~ substring(cabin_2, 1,1))
  ) %>%
  mutate(cabin_3_deck = case_when(
    is.na(cabin_3) | substring(cabin_3, 1,1) == "" ~ NA,
    TRUE ~ substring(cabin_3, 1,1))
  ) %>%
  mutate(cabin_4_deck = case_when(
    is.na(cabin_4) | substring(cabin_4, 1,1) == "" ~ NA,
    TRUE ~ substring(cabin_4, 1,1))
  )

# replace NA's from cabin_2_deck with value from cabin_1_deck 
# to allow comparisons between all four columns
# -> proceed with columns cabin_3_deck, cabin_4_deck accordingly
# (preparation for following step)
titanic_modified$cabin_2_deck[is.na(titanic_modified$cabin_2_deck)] <- titanic_modified$cabin_1_deck[is.na(titanic_modified$cabin_2_deck)]
titanic_modified$cabin_3_deck[is.na(titanic_modified$cabin_3_deck)] <- titanic_modified$cabin_2_deck[is.na(titanic_modified$cabin_3_deck)]
titanic_modified$cabin_4_deck[is.na(titanic_modified$cabin_4_deck)] <- titanic_modified$cabin_3_deck[is.na(titanic_modified$cabin_4_deck)]

# create column deck containing letter from cabin number (or NA)

# if multiple cabin numbers exist for one person: 
#     - if all letters are the same: assign this letter
#     - else: NA
# if one cabin number exists for a person:
#     - assign containing letter

titanic_modified <- titanic_modified %>%
  mutate(deck = case_when(
    cabin_1_deck == cabin_2_deck & cabin_2_deck == cabin_3_deck & cabin_3_deck == cabin_4_deck ~ cabin_1_deck,
    TRUE ~ NA
  )) %>% 
  # remove unnecessary columns
  select(-c(cabin_1_deck,cabin_2_deck, cabin_3_deck, cabin_4_deck,cabin_1,cabin_2,cabin_3,cabin_4))

##############################################################################

# remove columns which are not needed for following analysis
titanic_modified <- titanic_modified %>% select(-c(PassengerId, Name, Cabin))

# save modified dataset
write.csv(titanic_modified, file = "data/processed/titanic_modified.csv")
