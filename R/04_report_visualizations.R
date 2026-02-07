# Explorative Datenanalyse: Titanic-Datensatz
# Code für die im Bericht dargestellten Graphen

# erstellte Funktionen laden 
source("R/02a_functions.R")

# bereinigten Datensatz laden
titanic_data <- read.csv("data/processed/titanic_modified.csv")

# Pakete laden
pkgs <- c("ggplot2", "dplyr", "scales","tidyr")
invisible(lapply(pkgs, function(p)
  if (!require(p, character.only = TRUE)) install.packages(p)
))


# Teil 1: Altersverteilung der Passagiere als Histogramm

# Berechnung deskriptiver Kennzahlen 
stats_age <- describe_numeric(titanic_data, "Age")
n_total <- nrow(titanic_data)

age_na <- sum(is.na(titanic_data$Age))
age_n_valid <- n_total - age_na 


# Visualisierung der Altersverteilung
p <- ggplot(titanic_data, aes(Age)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 25, fill = "#3498DB",
                 color = "white", alpha = 0.7) +
  geom_density(color = "#E74C3C", linewidth = 1.2, adjust = 1.5) +
  geom_vline(xintercept = stats_age$mean,
             color = "#27AE60", linetype = "dashed") +
  geom_vline(xintercept = stats_age$median,
             color = "#8E44AD", linetype = "dashed") +
  annotate("text",
           x = stats_age$mean,
           y = 0.05,  
           label = paste0("Mittelwert = ", round(stats_age$mean, 1)),
           color = "#27AE60",
           angle = 90,
           vjust = -1.0,
           #hjust = -0.8,
           fontface = "plain",
           size = 3) +
  annotate("text",
           x = stats_age$median,
           y = 0.05,   # Höhe anpassen
           label = paste0("Median = ", round(stats_age$median, 1)),
           color = "#8E44AD",
           angle = 90,
           vjust = 1.5,
           fontface = "plain",
           size = 3) +
  labs(
    title = "Altersverteilung der Titanic-Passagiere",
    subtitle = paste("n =", age_n_valid, "| fehlende Werte:", age_na),
    x = "Alter (Jahre)",
    y = "Dichte") +
  theme_minimal(base_size = 13)

# Export 
ggsave("R/visualizations/age_histogram_titanic.png", p, width = 12, height = 8, dpi = 300)


# Teil 2: Überlebenswahrscheinlichkeit nach Geschlecht als gruppierte Häufigkeiten

# prozentuale Anteile gestorben/überlebt je Geschlecht berechnen
survival_summary <- titanic_data  %>%
  filter(!is.na(Sex_label), !is.na(Survived)) %>%
  count(Sex_label, Survived_label) %>%
  group_by(Sex_label) %>%
  mutate(
    percentage = n / sum(n) * 100,
    label = paste0("n = ", n, "\n", round(percentage, 1), "%")
  ) %>%
  ungroup()


# Berechnung gruppenspezifischer Überlebensraten
total_stats <- survival_summary %>%
  group_by(Sex_label) %>%
  summarise(
    Total = sum(n),
    Überlebt = sum(n[Survived_label == "Überlebt"]),
    Gestorben = sum(n[Survived_label == "Gestorben"]),
    Überlebensrate = round(Überlebt / Total * 100, 1),
    .groups = "drop"
  )

# Visualisierung
colors <- c("Gestorben" = "#E74C3C", "Überlebt" = "#2ECC71")

p <- ggplot(survival_summary,
            aes(x = Sex_label, y = n, fill = Survived_label)) +
  geom_col(position = position_dodge(0.8), width = 0.7, color = "white") +
  geom_text(aes(label = label),
            position = position_dodge(0.8),
            vjust = -0.3,
            fontface = "plain",
            size = 3) +
  scale_fill_manual(values = colors) +
  labs(
    title = "Überlebensrate der Titanic-Passagiere nach Geschlecht",
    subtitle = paste("Gesamtüberlebensrate:",
                     round(sum(total_stats$Überlebt) /
                             sum(total_stats$Total) * 100, 1), "%",
                     "| n =", sum(total_stats$Total)),
    x = "Geschlecht",
    y = "Anzahl Personen",
    fill = ""
  ) +
  ylim(0,550) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Export
ggsave("R/visualizations/survival_by_sex_grouped.png", p, width = 12, height = 8, dpi = 300)


# Teil 3: Überlebensraten nach Passagierklasse und Geschlecht 

# Visualisierung
p <- plot_3_cat_var(titanic_data, 
                    vars = c("Pclass_label","Survived_label", "Sex_label"),
                    type = "facet_bar_stacked") +
  scale_fill_manual(values = colors) +
  labs(title = "Überlebensraten nach Passagierklasse und Geschlecht",
       subtitle = paste("Gesamtüberlebensrate:",
                        round(sum(total_stats$Überlebt) /
                                n_total * 100, 1), "%",
                        "| n =", n_total),
    x = "",
    y = "Anteil Personen",
    fill = ""
  ) +
  theme_minimal(base_size = 13) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Export
ggsave("R/visualizations/survival_class_sex.png", p, width = 12, height = 8, dpi = 300)


# Teil 4: Ticketpreis in Abhängigkeit vom Überlebensstatus als Violin- und Boxplot

# Visualisierung
p <- ggplot(titanic_data, aes(Survived_label, Fare, fill = Survived_label)) +
  geom_violin(alpha = 0.4, trim = TRUE) +
  geom_boxplot(width = 0.25, alpha = 0.7) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(trans = "log1p",
                     breaks = c(0,10, 30, 100, 300, 500)) +
  labs(
    title = "Ticketpreis in Abhängigkeit vom Überlebensstatus",
    subtitle = "Violin = Dichte | Box = Quartile | gelbe Raute = Mittelwert",
    x = "",
    y = "Ticketpreis (log-Skala)",
    fill = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  annotate(
    "point",
    x = 1,
    y = mean(titanic_data$Fare[titanic_data$Survived == 0]),          
    shape = 18,     
    size = 3,
    color = "yellow"
  ) +
  annotate(
    "point",
    x = 2,
    y = mean(titanic_data$Fare[titanic_data$Survived == 1]),          
    shape = 18,     
    size = 3,
    color = "yellow"
  ) 

print(p)

# 4.4 Export
ggsave("R/visualizations/fare_vs_survival.png", p, width = 12, height = 8, dpi = 300)
