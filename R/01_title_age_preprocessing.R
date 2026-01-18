# Title: Titanic Preprocessing : Title & Age Imputation
# Author: Meriam
# Description:
#  Extract title from passenger names
#  Recode categorical variables (Survived, Sex, Embarked)
#  Impute missing Age values based on title (median)
#  Save final processed dataset
# Date: 18.01.2026

library(dplyr)
library(stringr)

df <- read.csv("data/raw/titanic.csv", stringsAsFactors = FALSE)

# Title/Anrede aus Name extrahieren
df <- df %>%
  mutate(Title = str_extract(Name, "(?<=,\\s)[A-Za-z]+(?=\\.)")) %>%
  mutate(Title = case_when(
    Title %in% c("Ms", "Mlle") ~ "Miss",
    Title %in% c("Mme") ~ "Mrs",
    TRUE ~ Title
  ))


# Variablen als factor codieren
df <- df %>%
  mutate(
    Survived = factor(Survived),
    Sex = factor(Sex),
    Embarked = factor(Embarked)
  )


# Age-Imputation (Median je Title)
df <- df %>%
  group_by(Title) %>%
  mutate(Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age)) %>%
  ungroup()

# Fallback, falls noch NA übrig sind
df$Age[is.na(df$Age)] <- median(df$Age, na.rm = TRUE)

# Final speichern
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
write.csv(df, "data/processed/titanic_with_title_age.csv", row.names = FALSE)
