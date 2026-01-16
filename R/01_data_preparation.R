# Title: Titanic Preprocessing
# Authors: Meriam & Rica
# Description: 
#  - read dataset titanic.csv
#  - create new columns title and modify existing columns
#  - remove columns which are not needed for further analysis
#  - save adjusted dataset as titanic_modified.csv

# Date: 13.01.2026

df <- read.csv("data/raw/titanic.csv", stringsAsFactors = FALSE)
str(df)
summary(df)

titanic_modified <- df


############## recode Pclass as ordered factor ##############

# check if column Pclass only contains numbers
check_Pclass <- unique(titanic_modified$Pclass)

# recode
titanic_modified$Pclass <- ordered(titanic_modified$Pclass)

#############################################################

