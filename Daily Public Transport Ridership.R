# Set the working directory
setwd("C:/Users/danis/OneDrive/Documents/Year 3/CPC351/Assignment 2/cpc351-asg2")

# Path to the dataset
dataset_path <- "C:/Users/danis/OneDrive/Documents/Year 3/CPC351/Assignment 2/cpc351-asg2/Data/ridership_headline.csv"

# Read the CSV file into a data frame
ridership_data <- read.csv(dataset_path, stringsAsFactors = FALSE)

# Check for missing values
summary(ridership_data)

# View the first few rows
head(ridership_data)

#Convert date column to date format
ridership_data$date <- as.Date(ridership_data$date, format = "%Y-%m-%d")

# Loop through each column and replace NA with 0 for numeric columns
for (col in names(ridership_data)) {
  if (is.numeric(ridership_data[[col]])) {
    ridership_data[[col]][is.na(ridership_data[[col]])] <- 0
  }
}

# Check for remaining NAs
sum(is.na(ridership_data))

# Check for missing values
summary(ridership_data)

# View the updated dataset
head(ridership_data)

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(tidyverse)








#========= QUESTION 6 =========

# Add a month column for aggregation
ridership_data$month <- format(ridership_data$date, "%Y-%m")

# Aggregate data by month
monthly_ridership <- ridership_data %>%
  group_by(month) %>%
  summarise(across(-date, sum)) %>%
  arrange(month)

# Convert data from wide to long format for better plotting
monthly_ridership_long <- monthly_ridership %>%
  gather(key = "service", value = "trips", -month)

#Customize colour for plotting
custom_palette <- c(
  "#DC143C", # Crimson
  "#4169E1", # Royal Blue
  "#DAA520", # Goldenrod
  "#FF1493", # Deep Pink
  "#3CB371", # Medium Sea Green
  "#FFFF33", # Yellow
  "#FF4500", # Orange Red
  "#800080", # Purple
  "#999999", # Grey
  "#66C2A5", # Mint
  "#FC8D62", # Coral
  "#8DA0CB", # Slate Blue
  "#32CD32"  # Lime Green
)


# Create a named vector for service name mapping
service_names <- c(
  "bus_rkl" = "Rapid Bus (KL)",
  "bus_rkn" = "Rapid Bus (Kuantan)",
  "bus_rpn" = "Rapid Bus (Penang)",
  "rail_lrt_ampang" = "LRT Ampang Line",
  "rail_lrt_kj" = "LRT Kelana Jaya Line",
  "rail_monorail" = "Monorail Line",
  "rail_mrt_kajang" = "MRT Kajang Line",
  "rail_mrt_pjy" = "MRT Putrajaya Line",
  "rail_ets" = "KTMB ETS",
  "rail_intercity" = "KTM Intercity",
  "rail_komuter" = "KTM Komuter",
  "rail_komuter_utara" = "KTM Komuter Utara",
  "rail_tebrau" = "KTM Shuttle Tebrau"
)


# Map the service names
monthly_ridership_long$service <- service_names[monthly_ridership_long$service]

# Create the plot and save it to a variable
p <- ggplot(monthly_ridership_long, aes(x = month, y = trips, fill = service)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Monthly Public Transport Ridership (2019-2024)",
    subtitle = "Aggregated trips for all 13 public transport services",
    x = "Month",
    y = "Number of Trips",
    fill = "Service"
  ) +
  scale_y_continuous(labels = comma_format()) +
  scale_fill_manual(values = custom_palette)


p + theme(
  axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
  axis.text.y = element_text(size = 12),
  axis.title = element_text(size = 14),
  legend.text = element_text(size = 12),
  plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
  plot.subtitle = element_text(hjust = 0.5, size = 14)
)






#========= QUESTION 7 =========
# Filter data for the year 2023
ridership_2023 <- ridership_data %>%
  filter(year(date) == 2023)

# Sort by "bus_rkl" to find the highest and lowest ridership days
highest_ridership_2023 <- ridership_2023 %>%
  arrange(desc(bus_rkl)) %>%
  head(10)

lowest_ridership_2023 <- ridership_2023 %>%
  arrange(bus_rkl) %>%
  head(10)

# Format the dates and ridership numbers
format_ridership <- function(df) {
  df %>%
    mutate(
      date = format(date, "%B %d, %Y"),  # Format date as "Month Day, Year"
      bus_rkl = format(bus_rkl, big.mark = ",", scientific = FALSE)  # Add thousand separators
    )
}

# Create nicely formatted data frames
highest_formatted <- format_ridership(highest_ridership_2023)
lowest_formatted <- format_ridership(lowest_ridership_2023)

# Print results with knitr::kable for better formatting
cat("\n=== 10 Days with Highest Ridership for Rapid Bus (KL) in 2023 ===\n")
print(knitr::kable(
  highest_formatted[, c("date", "bus_rkl")],
  col.names = c("Date", "Number of Trips"),
  align = c("l", "r"),
  format = "pipe"
))

cat("\n\n=== 10 Days with Lowest Ridership for Rapid Bus (KL) in 2023 ===\n")
print(knitr::kable(
  lowest_formatted[, c("date", "bus_rkl")],
  col.names = c("Date", "Number of Trips"),
  align = c("l", "r"),
  format = "pipe"
))












#========= QUESTION 8 =========

# Filter data for January and February 2022
ridership_2022 <- ridership_data %>%
  filter(year(date) == 2022 & month(date) %in% c(1, 2))

# Categorize as Weekday or Weekend
ridership_2022 <- ridership_2022 %>%
  mutate(day_type = ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

# Aggregate ridership by date and day_type for detailed trends
daily_ridership <- ridership_2022 %>%
  group_by(date, day_type) %>%
  summarise(total_ridership = sum(bus_rpn), .groups = 'drop')

# Line Plot: Daily Ridership Trend for Weekdays vs Weekends
ggplot(daily_ridership, aes(x = date, y = total_ridership, color = day_type, group = day_type)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Daily Ridership Trend for Rapid Bus (Penang) in Jan-Feb 2022",
    subtitle = "Weekdays vs Weekends",
    x = "Date",
    y = "Total Ridership",
    color = "Day Type"
  ) +
  scale_color_manual(values = c("Weekday" = "#4169E1", "Weekend" = "#FF6347")) +  # Blue for Weekdays, Red for Weekends
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b") +  # Weekly date breaks
  scale_y_continuous(labels = comma) +  # Thousand separators
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


























#========= QUESTION 9 ========

#festival dates in Malaysia for the years 2019–2023
festival_dates <- list(
  "Hari Raya Aidilfitri" = c("2019-06-05", "2020-05-24", "2021-05-13", "2022-05-02", "2023-04-22"),
  "Chinese New Year" = c("2019-02-05", "2020-01-25", "2021-02-12", "2022-02-01", "2023-01-22"),
  "Deepavali" = c("2019-10-27", "2020-11-14", "2021-11-04", "2022-10-24", "2023-11-12"),
  "Christmas" = c("2019-12-25", "2020-12-25", "2021-12-25", "2022-12-25", "2023-12-25")
)

print(festival_dates)

# List of rail lines
lines <- c("rail_lrt_ampang", "rail_mrt_kajang", "rail_lrt_kj", "rail_monorail")
titles <- c(
  "LRT Ampang Line Ridership around Festival (2019-2023)",
  "MRT Kajang Line Ridership around Festival (2019-2023)",
  "LRT Kelana Jaya Line Ridership around Festival (2019-2023)",
  "Monorail Line Ridership around Festival (2019-2023)"
)

result <- data.frame()

# Data for each festival
for (festival in names(festival_dates)) {
  dates <- festival_dates[[festival]]
  
  for (i in seq_along(dates)) {
    date <- as.Date(dates[i])
    year <- as.character(format(date, "%Y"))
    start_date <- date - 7
    end_date <- date + 7
    
    #Filter data for the current year and festival
    filtered <- ridership_data %>%
      filter(date >= start_date & date <= end_date) %>%
      mutate(
        time_period = case_when(
          date < date ~ "7 days before",
          date == date ~ "festival day",
          date > date ~ "7 days after"
        )
      )
    
    for (line in lines) {
      temp <- filtered %>%
        group_by(time_period) %>%
        summarise(ridership = sum(!!sym(line), na.rm = TRUE)) %>%
        mutate(
          line = line,
          year = year,
          festival = festival
        )
      result <- bind_rows(result, temp)
    }
  }
}
print(result)

# Simplify ridership values
result$ridership <- result$ridership / 1e6

# Create bar charts 
for (i in seq_along(lines)) {
  line_data <- result %>% filter(line == lines[i])
  
  total_ridership <- ggplot(line_data, aes(x = year, y = ridership, fill = festival)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = titles[i],
      x = "Year",
      y = "Total Ridership (in millions)",
      fill = "Festival"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 0, hjust = 1)
    )
  
  print(total_ridership)
}

















#========= QUESTION 10 ========

# Filter data 
average <- ridership_data %>%
  select(date, rail_lrt_ampang, rail_lrt_kj, rail_monorail)

# Summarize data by year with average daily rider
average <- average %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(
    lrt_ampang = mean(rail_lrt_ampang),
    lrt_kj = mean(rail_lrt_kj),
    monorail = mean(rail_monorail)
  )
print(average)

# Graph 1:
ggplot(average, aes(x = year)) +
  geom_line(aes(y = lrt_ampang, color = "LRT Ampang"), size = 1) +
  geom_line(aes(y = lrt_kj, color = "LRT Kelana Jaya"), size = 1) +
  geom_line(aes(y = monorail, color = "Monorail"), size = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Average Daily Ridership Trends (2019-2024)",
    x = "Year",
    y = "Average Daily Ridership",
    color = "Line"
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "panel",   
    plot.title = element_text(hjust = 0.5)  
  )

#Phases define
mcophase <- data.frame(
  phase = c(
    "Pre-Pandemic", "MCO Phase 1", "MCO Phase 2", "MCO Phase 3", "MCO Phase 4", 
    "CMCO Phase 1", "CMCO Phase 2", "RMCO Phase 1", "RMCO Phase 2", "RMCO Phase 3",
    "Transitional Phase","Endemic Phase"
  ),
  start_date = as.Date(c(
    "2019-01-01", "2020-03-18", "2020-04-01", "2020-04-15", "2020-04-29", 
    "2020-05-04", "2020-05-13", "2020-06-10", "2020-09-01", "2021-01-01",
    "2021-04-01", "2022-04-01"
  )),
  end_date = as.Date(c(
    "2020-03-17", "2020-03-31", "2020-04-14", "2020-04-28", "2020-05-03", 
    "2020-05-12", "2020-06-09", "2020-08-31", "2020-12-31", "2021-03-31",
    "2022-03-31", "2024-12-31"
  ))
)
print(mcophase)


mcophases <- ridership_data %>%
  select(date, rail_lrt_ampang, rail_lrt_kj, rail_monorail) %>%
  filter(date <= as.Date("2024-12-31")) %>%  
  mutate(
    phase = case_when(
      date >= as.Date("2019-01-01") & date <= as.Date("2020-03-17") ~ "Pre-Pandemic",
      date >= as.Date("2020-03-18") & date <= as.Date("2020-03-31") ~ "MCO Phase 1",
      date >= as.Date("2020-04-01") & date <= as.Date("2020-04-14") ~ "MCO Phase 2",
      date >= as.Date("2020-04-15") & date <= as.Date("2020-04-28") ~ "MCO Phase 3",
      date >= as.Date("2020-04-29") & date <= as.Date("2020-05-03") ~ "MCO Phase 4",
      date >= as.Date("2020-05-04") & date <= as.Date("2020-05-12") ~ "CMCO Phase 1",
      date >= as.Date("2020-05-13") & date <= as.Date("2020-06-09") ~ "CMCO Phase 2",
      date >= as.Date("2020-06-10") & date <= as.Date("2020-08-31") ~ "RMCO Phase 1",
      date >= as.Date("2020-09-01") & date <= as.Date("2020-12-31") ~ "RMCO Phase 2",
      date >= as.Date("2021-01-01") & date <= as.Date("2021-03-31") ~ "RMCO Phase 3",
      date >= as.Date("2021-04-01") & date <= as.Date("2022-03-31") ~ "Transitional Phase",
      date >= as.Date("2022-04-01") & date <= as.Date("2024-12-31") ~ "Endemic Phase",
    )
  ) %>%
  
  group_by(phase) %>%
  summarize(
    lrt_ampang = mean(rail_lrt_ampang),
    lrt_kj = mean(rail_lrt_kj),
    monorail = mean(rail_monorail)
  )
print(mcophases)

mcophases <- mcophases %>%
  pivot_longer(cols = c(lrt_ampang, lrt_kj, monorail), 
               names_to = "line", 
               values_to = "ridership")

#Graph2
ggplot(mcophases, aes(x = factor(phase, levels = c(
  "Pre-Pandemic", "MCO Phase 1", "MCO Phase 2", "MCO Phase 3", "MCO Phase 4", 
  "CMCO Phase 1", "CMCO Phase 2", "RMCO Phase 1", "RMCO Phase 2", "RMCO Phase 3", 
  "Transitional Phase", "Endemic Phase"
)), y = ridership, fill = line)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(
    labels = scales::comma,
    breaks = seq(0, 280000, by = 40000)  
  ) +
  scale_fill_discrete(
    labels = c("lrt_ampang" = "LRT Ampang", "lrt_kj" = "LRT Kelana Jaya", "monorail" = 
                 "Monorail") 
  ) +
  labs(
    title = "Average Ridership by MCO Phase",
    x = "MCO Phase",
    y = "Average Ridership",
    fill = "Line"
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "panel",   
    plot.title = element_text(hjust = 0.5),  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

