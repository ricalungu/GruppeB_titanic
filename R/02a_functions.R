# ------------------------------------------------------
# v. function for visualizing three categorical variables
# ------------------------------------------------------
#' Author: Rica
#' Name:  plot_3_cat_var
#' Description: This function can be used for visualizing three categorical 
#'              variables from a given dataframe. One can choose between a 
#'              grouped barplot, a stacked barplot and a heatmap.  
#' 
#' @param   df      A data frame containing the variables given in 'vars'.
#' @param   vars    A character vector containing three categorical variables from df.
#' @param   type    A character string specifying the plot type. 
#'                  Available types are facet_bar_grouped, facet_bar_stacked, facet_heatmap. 
#' @return  The output is a ggplot visualizing the variables given in 'vars'.
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

# ------------------------------------------------------








