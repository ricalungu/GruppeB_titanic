
# 02_functions_main.R

#Load helper + function files 
source("R/02b_inner_functions.R")
source("R/02a_functions.R")

# Load processed data
df <- read.csv(file.path("data", "processed", "titanic_modified.csv"))

# Example calls 
print(describe_numeric(df, "Age"))
print(describe_numeric(df, "Fare"))

print(descrip_categorical(df, "Sex"))

print(bivar_cat_cat(df, "Sex", "Survived"))

print(bivar_num_bin(df, "Fare", "Survived"))

# Optional visualization 
p <- plot_3_cat_var(
  df = df,
  vars = c("Pclass", "Survived", "Sex"),
  type = "facet_bar_stacked"
)
print(p)
