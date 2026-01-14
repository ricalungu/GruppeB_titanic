# Titel: Titanic Preprocessing Part 1
# Autorin: Meriam
# Beschreibung:
#  Einlesen des Titanic-Datensatzes
#  Vorbereitung für die Aufbereitung (Titel/Anrede, Codierung)
# Datum: 13.01.2026
df <- read.csv("data/raw/titanic.csv", stringsAsFactors = FALSE)
str(df)
summary(df)
