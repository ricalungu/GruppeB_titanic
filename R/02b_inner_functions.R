# Title: Helper functions
# Author: Meriam
# Description: Internal helper functions used by descriptive analysis functions (2a)

# check if variable is numeric
is_numeric_var <- function(df, var) {
  if (!var %in% names(df)) {
    stop(paste("Variable", var, "not found in data frame"))
  }
  is.numeric(df[[var]])
}

# calculate interquartile range safely
safe_iqr <- function(x) {
  if (all(is.na(x))) return(NA)
  IQR(x, na.rm = TRUE)
}

