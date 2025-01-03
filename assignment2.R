path <- "D:/Projects/R projects/assignment2/Data"
setwd(path)
getwd()

csv_files <- list.files(pattern = "^cars.*\\.csv")

data <- do.call(rbind, lapply(csv_files, read.csv))
data

library(dplyr)
library(lubridate)

# Convert to Date format
data <- data %>%
  mutate(date_reg = as.Date(date_reg, format = "%d/%m/%Y"))

# Replace invalid dates with NA
data <- data %>%
  mutate(date_reg = ifelse(is.na(date_reg) | date_reg > Sys.Date(), NA, date_reg))


