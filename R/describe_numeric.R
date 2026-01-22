
# Title: Descriptive statistics for numeric variables
# Author: Meriam
# Description:
#  Computes descriptive statistics for a numeric variable

describe_numeric <- function(df, var) {
  
  # Input checks
  if (!is.data.frame(df)) {
    stop("df must be a data.frame")
  }
  
  if (!var %in% names(df)) {
    stop(paste("Variable not found:", var))
  }
  
  x <- df[[var]]
  
  if (!is.numeric(x)) {
    stop("Variable must be numeric")
  }
  
  # Descriptive statistics
  stats <- c(
    mean   = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd     = sd(x, na.rm = TRUE),
    min    = min(x, na.rm = TRUE),
    max    = max(x, na.rm = TRUE)
  )
  
  return(stats)
}
