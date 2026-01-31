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
  x <- as.factor(x)
  
  freq <- table(x)
  prop <- prop.table(freq)
  
  data.frame(
    category   = names(freq),
    count      = as.numeric(freq),
    proportion = round(as.numeric(prop), 3)
  )
}


# 3) Bivariat: kategorial x kategorial
desc_cat_cat <- function(x, y) {
  x <- as.factor(x)
  y <- as.factor(y)

  tab <- table(x, y)

  list(
    counts = tab,
    row_prop = prop.table(tab, 1)
  )
}


# 4) Bivariat: metrisch x dichotom
desc_metric_dicho <- function(metric, group) {
  group <- as.factor(group)

  aggregate(
    metric,
    by = list(group),
    FUN = function(x)
      c(mean = mean(x, na.rm = TRUE),
        sd   = sd(x, na.rm = TRUE))
  )
}


# 5) Visualisierung: 3 kategoriale Variablen
plot_cat3 <- function(df, x, fill, facet) {
  library(ggplot2)

  ggplot(df, aes(.data[[x]], fill = .data[[fill]])) +
    geom_bar(position = "dodge") +
    facet_wrap(~ .data[[facet]]) +
    theme_minimal()
}








