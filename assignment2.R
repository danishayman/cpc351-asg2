path <- "D:/Projects/R projects/assignment2/Data"
setwd(path)
getwd()

csv_files <- list.files(pattern = "^cars.*\\.csv")

data <- do.call(rbind, lapply(csv_files, read.csv))
data

# features
# - date_reg
# - type
# - maker
# - model
# - colour
# - fuel
# - state

# Load required libraries
library(dplyr)   # For data manipulation
library(ggplot2) # For visualizing missing values
library(naniar)  # For missing value analysis

## EDA

# Check for missing values
cat("\nCount of missing values in each column:\n")
missing_summary <- sapply(data, function(x) sum(is.na(x)))
print(missing_summary)

# Check for percentage missing values
cat("\nPercentage of missing values in each column:\n")
missing_percentage <- sapply(data, function(x) mean(is.na(x)) * 100)
print(missing_percentage)

## CLEANING AND PREPROCESSING

# Handle missing values (Example: Fill missing with appropriate values or remove rows)
data <- data %>%
  mutate(
    date_reg = ifelse(is.na(date_reg), "2000-01-01", date_reg), # Replace missing dates with a default value
    type = ifelse(is.na(type), "Unknown", type),               # Replace missing types with "Unknown"
    maker = ifelse(is.na(maker), "Unknown", maker),            # Replace missing makers with "Unknown"
    model = ifelse(is.na(model), "Unknown", model),            # Replace missing models with "Unknown"
    colour = ifelse(is.na(colour), "Unknown", colour),         # Replace missing colours with "Unknown"
    fuel = ifelse(is.na(fuel), "Unknown", fuel),               # Replace missing fuel types with "Unknown"
    state = ifelse(is.na(state), "Unknown", state)             # Replace missing states with "Unknown"
  )






