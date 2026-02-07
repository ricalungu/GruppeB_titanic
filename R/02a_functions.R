# include functions from file 02b_inner_functions.R
source("R/02b_inner_functions.R")


#' Title: Descriptive statistics for a numeric variable (2a i.)
#' Author: Meriam
#' Description: Computes the following descriptive statistics for a given 
#'              numeric variable: mean, median, standard deviation, 
#'              interquartile range, minimum, maximum.
#'
#' @param df A data frame containing variable 'var'.
#' @param var A string giving the name of the numeric variable.
#'
#' @return A list containing the following elements:
#'          \describe{
#'            \item{mean}{Arithmetic mean (NA removed)}
#'            \item{median}{Median (NA removed)}
#'            \item{sd}{Standard deviation (NA removed)}
#'            \item{iqr}{Interquartile range, computed using `safe_iqr()`}
#'            \item{min}{Minimum (NA removed)}
#'            \item{max}{Maximum (NA removed)}
#'          }
#' @example
#' df <- data.frame(x = c(1, 2, 3, 10, NA))
#' describe_numeric(df, "x")

describe_numeric <- function(df, var) {
  
  # use helper function from 2b
  if (!is_numeric_var(df, var)) {
    stop("Variable must be numeric")
  }
  
  x <- df[[var]]
  
  stats <- list(
    mean   = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd     = sd(x, na.rm = TRUE),
    iqr    = safe_iqr(x),
    min    = min(x, na.rm = TRUE),
    max    = max(x, na.rm = TRUE)
  )
  return(stats)
}

# Return as a list
ergebnis <- describe_numeric(titanic_data, "Fare")

#' Title: Descriptive statistics for a categorical variable (2a ii.)
#' Author: Sebastian
#' Description: Converts values to factor and computes the following descriptive 
#'              statistics for a given categorical variable: frequency for each 
#'              category,proportion for each category, mode.
#'
#' @param df A data frame containing variable 'var'.
#' @param var A string giving the name of the categorical variable.
#'
#' @return A list of class 'desc_cat' containing the following elements:
#'          \describe{
#'            \item{var}{variable name}
#'            \item{frequencies}{frequency table including NA values}
#'            \item{proportions}{proportions table including NA values}
#'            \item{mode}{category with max. frequency}
#'          }
#' @example
#' df <- data.frame(x = c("A", "A", "B", "C", "A"))
#' descrip_categorical(df, "x")

descrip_categorical <- function(df, var) {
  
  # Validierung der Eingaben: Sicherstellen, dass die Argumente korrekt sind
  if (!is.data.frame(df)) stop("Argument 'df' muss ein data.frame sein.")
  if (!var %in% names(df)) stop("Variable nicht im data.frame gefunden: ", var)
  
  # Extrahieren der Spalte
  x <- df[[var]]
  
  # Einheitliche Umwandlung in Faktor um korrekte Tabellen auch fuer logische-Werte und Text-Werte zu bekommen
  if (!is.factor(x)) {
    x <- as.factor(x)
  }
  
  # Statistische Berechnungen
  # Durch useNA = "ifany" werden fehlende Werte nicht ignoriert 
  freq <- table(x, useNA = "ifany")
  prop <- prop.table(freq)
  
  # Modus-Berechnung identifiziert Werte mit der hoechsten Frequenz 
  max_count <- max(freq, na.rm = TRUE)
  modes <- names(freq)[which(freq == max_count)]
  
  # Rueckgabe als strukturierte Liste mit einer eigenen Klasse 
  structure(
    list(
      variable = var,
      frequencies = freq,
      proportions = prop,
      modes = modes
    ),
    class = "desc_cat" # Optional: Eigene Klasse fuer optisch bessere Print-Ausgaben
  )
}


#' Title: Descriptive bivariate statistics for two categorical variables (2a iii.)
#' Author: Sebastian
#' Description: Converts values to factors and computes the following statistics 
#'              for two given categorical variable: contingency table 
#'              (and associated row-wise and column-wise proportions), 
#'              Cramers V (measure of association), Chi-squared and 
#'              chi-squared test statistics, fisher test statistics or no test
#'              statistics (depending on input).
#'              
#' @param df A data frame containing variables 'var1', 'var2'.
#' @param var1 A string giving the name of the first categorical variable (rows).
#' @param var2 A string giving the name of the second categorical variable (columns).
#' 
#' @return A list containing the following elements:
#'          \describe{
#'            \item{tabelle}{contingency table including NA values}
#'            \item{zeilen_anteile}{row-wise proportions}
#'            \item{spalten_anteile}{column-wise proportions}
#'            \item{angewandter_test}{character string indicating which test was used}
#'            \item{test_details}{test results}
#'            \item{cramers_v}{Cramers V (NA if not computable)}
#'          }
#' @example
#' df <- data.frame(
#'   Sex = c("m", "w", "w", "m", "m"),
#'   Survived    = c("1", "0", "1", "1", "0"))
#' bivar_cat_cat(df, "Sex", "Survived")

bivar_cat_cat <- function(df, var1, var2) {
  
  # Valiedirung: Daten auf existenz pruefen
  if (!all(c(var1, var2) %in% names(df))) {
    stop("Fehler: Eine oder beide Variablen wurden im data.frame nicht gefunden.")
  }
  
  # Daten extrahieren und umwandeln in Faktoren
  x <- as.factor(df[[var1]])
  y <- as.factor(df[[var2]])
  
  # Kontingenztabelle erstellen, Variablenamen werden durch dnn als Tabellenueberschriften gesetzt
  tbl <- table(x, y, useNA = "ifany", dnn = c(var1, var2))
  
  # Relative Haufigkeiten
  row_prop <- prop.table(tbl, 1) # Zeilenweise
  col_prop <- prop.table(tbl, 2) # Spaltenweise
  
  # Testverfahren: Falls die Tabellenstruktur fuer den Test ungeeignet ist, faengt tryCatch Fehler ab
  chi <- tryCatch(chisq.test(tbl), error = function(e) NULL)
  test_used <- "Kein Test möglich"
  test_result <- NULL
  
  if (!is.null(chi)) {
    expected <- chi$expected
    
    # Voraussetzungen fuer Chi-Quadrat-Test pruefen (Erwartungswert >= 5)
    if(any(expected < 5)) {
      # Auf Fishers Test ausweichen bei 2x2 Tabellen
      if (all(dim(tbl) == c(2, 2))) {
        test_used <- "Fishers Test"
        test_result <- fisher.test(tbl)
      } else {
        test_used <- "Pearson Chi-Quadrat-Test"
        test_result <- chi
      }
    } else {
      test_used <- "Pearson Chi-Quadrat-Test"
      test_result <- chi
    }
  }
  
  # Effektstaerke: Cramer´s V berechnen
  # Formel zur Berechnung: Wurzel aus (Chi-Quadrat / (n * min(r-1, c-1)))
  n <- sum(tbl)
  r <- nrow(tbl)
  c <- ncol(tbl)
  chi_stat <- if (!is.null(chi)) as.numeric(chi$statistic) else NA
  
  cramersV <- if (!is.na(chi_stat)) {
    sqrt(chi_stat / (n * (min(r - 1, c - 1))))
  } else {
    NA
  }
  
  # Ausgabe der Ergebnisse in strukturierter Liste
  list(
    tabelle = tbl,
    zeilen_anteile = row_prop,
    spalten_anteile = col_prop,
    angewandter_test = test_used,
    test_details = test_result, 
    cramers_v = cramersV
  )
}


#' Title: Descriptive bivariate statistics for a numeric and a dichotomous variable (2a iv.)
#' Author: Sebastian
#' Description: Computes descriptive statistics (number of observations, mean, 
#'              standard deviation) for a numeric variable across two groups 
#'              defined by a given dichotomous variable, performs Welch's t-test 
#'              and computes Cohen's d.
#'
#' @param df A data frame containing 'numvar' and 'binvar'.
#' @param numvar A string giving the name of the numeric variable.
#' @param binvar A string giving the name of the dichotomous variable.
#'
#' @return A list containing the following elements:
#'          \describe{
#'            \item{variablen}{names of given variables}
#'            \item{kennwerte_gruppen}{descriptive statistics for each of the 
#'                  two groups from 'binvar'}
#'            \item{t_test_details}{results t-tests}
#'            \item{effektstaerke_d}{effect size cohen's d}
#'          }
#' @example
#' df <- data.frame(
#'   Age = c(10, 12, 60, 20, 22, 53),
#'   Survived = c(1, 1, 1, 0, 0, 0))
#' bivar_num_bin(df, "Age", "Survived")

bivar_num_bin <- function(df, numvar, binvar) {
  
  # Validierung: Existenz und Struktur pruefen
  if (!all(c(numvar, binvar) %in% names(df))) {
    stop("Fehler: Variablen im data.frame nicht gefunden.")
  }
  
  # Daten extrahieren und bereinigen
  x <- as.numeric(df[[numvar]])
  g <- as.factor(df[[binvar]])
  
  # Auf genau zwei Auspraegungen pruefen
  levels_g <- levels(g)
  if (length(levels_g) != 2) {
    stop("Die Gruppenvariable muss genau 2 Auspraegungen haben.")
  }
  
  # Nur vollstaendige Faelle benutzen
  is_complete <- complete.cases(x, g)
  x <- x[is_complete]
  g <- g[is_complete]
  
  # Pro Gruppe deskriptive Statistik, "tapply" statt "for" weil es effizienter ist
  group_stats <- list(
    n = tapply(x, g, length),
    mean = tapply(x, g, mean),
    sd = tapply(x, g, sd)
  )
  
  # Anwendung des Welch-t Test (besonders robust gegen Varianzheterogenität)
  t_res <- t.test(x ~ g)
  
  # Effektstarke: Cohen´s d
  n1 <- group_stats$n[1]
  n2 <- group_stats$n[2]
  sd1 <- group_stats$sd[1]
  sd2 <- group_stats$sd[2]
  m1  <- group_stats$mean[1]
  m2  <- group_stats$mean[2]
  
  # Gepoolte Standardabweichung berechnen
  sd_pooled <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  cohen_d <- (m1 - m2) / sd_pooled
  
  # Ausgabe der Ergebnisse
  list(
    variablen = list(numerisch = numvar, gruppe = binvar),
    kennwerte_gruppen = group_stats,
    t_test_details = t_res,
    effektstaerke_d = as.numeric(cohen_d)
  )
}


#' Title: Visualizing three categorical variables (2a v.)
#' Author: Rica
#' Description: This function can be used for visualizing three categorical 
#'              variables from a given dataframe. One can choose between a 
#'              grouped barplot, a stacked barplot and a heatmap (each using facets).  
#' 
#' @param df A data frame containing the variables given in 'vars'.
#' @param vars A character vector containing three categorical variables from df.
#' @param type A character string specifying the plot type. 
#'             Available types are facet_bar_grouped, facet_bar_stacked, facet_heatmap. 
#'             
#' @return The output is a ggplot visualizing the variables given in 'vars'.
#' 
#' @details As ggplot is used for visualizing, you can easily modify the plot 
#'          by adding further specifications using '+' (e.g. adding a title, a theme, ...).
#' @example 
#' plot_3_cat_var(
#'      df = df_titanic,
#'      vars = c("Pclass", "Survived", "Sex"),
#'      type = "facet_bar_stacked")) +
#'      scale_x_discrete(name = "Passenger Class") +
#'      scale_y_continuous(name = "survival rate") + 
#'      labs(title = "Survival Rates by Class and Sex") +
#'      theme_minimal()
#' 

plot_3_cat_var <- function(df, 
                          vars, 
                          type = c("facet_bar_grouped","facet_bar_stacked", "facet_heatmap")
  ) {

  ### load required packages (including error handling)
  required_packages <- c("tidyr", "dplyr", "ggplot2")
  
  for (pkg in required_packages) {
    tryCatch(
      {
        library(pkg, character.only = TRUE)
      },
      error = function(e) {
        stop(
          paste0(
            "Please install package '", pkg, "'.\n"
          )
        )
      }
    )
  }
  
  library(tidyr)
  library(dplyr)
  library(ggplot2)
  
  ### check inputs
  
  if (!is.data.frame(df)) {
    stop("Argument 'df' must be a data frame.")
  }
  
  if (length(vars) != 3){
    stop("Please give three categorical variables.")
  }
  
  allowed_types <- c("facet_bar_grouped","facet_bar_stacked", "facet_heatmap")
  
  if (!type %in% allowed_types) {
    stop("Plot type not allowed. Choose one of the following types: ",
         paste(allowed_types, collapse = ", "), ".")
  }
  
  for (var in vars){
    if (!var %in% names(df)){
      stop(
        paste0("Could not find ", var ," in data frame.\n The given data frame contains the following variables: ", 
               paste(names(df), collapse = ", "), ".")
      )
    }
  }

  ### extract column names
  v1 <- sym(vars[1])
  v2 <- sym(vars[2])
  v3 <- sym(vars[3])
  
  # recode given variables as factor
  df_plot <- df %>%
    mutate(across(all_of(vars), as.factor))
  
  ### definition of different plot types
      
      # facet_bar_grouped
      
      if (type == "facet_bar_grouped") {
        return(
          ggplot(df_plot, aes(x = !!v1, fill = !!v2)) +
            geom_bar(position = "dodge") +
            facet_wrap(vars(!!v3))
        )
      }
      
      # facet_bar_stacked
      
      if (type == "facet_bar_stacked") {
        return(
          ggplot(df_plot, aes(x = !!v1, fill = !!v2)) +
            geom_bar(position = "fill") +
            facet_wrap(vars(!!v3))
        )
      }
      
      # facet_heatmap
      if (type == "facet_heatmap") {
        
        df_plot <- df_plot %>%
          count(across(all_of(vars)), name = "n")
        return(
          ggplot(df_plot, aes(!!v1, !!v2, fill = n)) +
            geom_tile() +
            facet_wrap(vars(!!v3)) 
        )
      }
}

