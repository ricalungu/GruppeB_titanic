# Title: Descriptive statistics for numeric variables
# Author: Meriam
# Description:
#  Computes descriptive statistics for a numeric variable

source("R/02b_inner_functions.R")

describe_numeric <- function(df, var) {

  # use helper function from 2b
  if (!is_numeric_var(df, var)) {
    stop("Variable must be numeric")
  }

  x <- df[[var]]

  stats <- c(
    mean   = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd     = sd(x, na.rm = TRUE),
    iqr    = safe_iqr(x),
    min    = min(x, na.rm = TRUE),
    max    = max(x, na.rm = TRUE)
  )

  return(stats)
}
