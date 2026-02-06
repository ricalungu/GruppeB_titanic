# Title: Helper functions
# Author: Meriam
# Description: Internal helper functions used by descriptive analysis functions (2a)

#' check if variable is numeric
#'
#' The internal function checks if a specified variable in a data frame is numeric
#' At first it verifies that the variable exists in the data frame and checks it afterwards
#'
#' @param df A data frame containing the variable to check
#' @param var A character string specifying the name of the variable to check
#' @return Logical value indicates whether the variable is numeric (TRUE) or not (FALSE)
#'
#' @details In case the specified variable does not exist in the data frame, the function
#' will stop execution with an informative error message
#'
#' @keywords internal
is_numeric_var <- function(df, var) {    
  if (!var %in% names(df)) {
    stop(paste("Variable", var, "not found in data frame"))
  }
  is.numeric(df[[var]])
}

#' calculate interquartile range (IQR) safely
#'
#' The internal function calculates the interquartile range (IQR) of a numeric vector
#' while handling missing values and edge cases safely
#' 
#' @param x A numeric vector to calculate the IQR
#'
#' @return the IQR as a numeric value or NA if all values are missing
#'
#' @details the function uses \code{na.rm = TRUE} to remove missing values
#' In case all values in the input vector are \code{NA}, it returns \code{NA}
#'
#' @keywords internal
safe_iqr <- function(x) {
  if (all(is.na(x))) return(NA)
  IQR(x, na.rm = TRUE)
}

