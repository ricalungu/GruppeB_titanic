# =====================================
# Funktionen für deskriptive Statistik
# Teil 2 (VI)
# =====================================

# 1) Deskriptive Statistik für metrische Variablen
desc_metric <- function(x) {
  stopifnot(is.numeric(x))
  
  list(
    mean   = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd     = sd(x, na.rm = TRUE),
    min    = min(x, na.rm = TRUE),
    max    = max(x, na.rm = TRUE)
  )
}

# 2) Deskriptive Statistik für kategoriale Variablen
desc_categorical <- function(x) {
  stopifnot(is.factor(x) || is.character(x))
  
  tab <- table(x, useNA = "ifany")
  prop <- prop.table(tab)
  
  data.frame(
    category = names(tab),
    count = as.vector(tab),
    proportion = round(as.vector(prop), 3)
  )
}

# 3) Bivariat: kategorial x kategorial
desc_cat_cat <- function(x, y) {
  stopifnot((is.factor(x) || is.character(x)) &&
            (is.factor(y) || is.character(y)))
  
  tab <- table(x, y)
  list(
    counts = tab,
    proportions = prop.table(tab, margin = 1)
  )
}

# 4) Bivariat: metrisch x dichotom
desc_metric_dicho <- function(metric, group) {
  stopifnot(is.numeric(metric))
  
  aggregate(metric, by = list(group), 
            FUN = function(x) c(
              mean = mean(x, na.rm = TRUE),
              sd   = sd(x, na.rm = TRUE)
            ))
}

# 5) Visualisierung: 3 kategoriale Variablen
plot_cat3 <- function(data, x, fill, facet) {
  library(ggplot2)
  
  ggplot(data, aes(x = .data[[x]], fill = .data[[fill]])) +
    geom_bar(position = "dodge") +
    facet_wrap(~ .data[[facet]]) +
    theme_minimal()
}







