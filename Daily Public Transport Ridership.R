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








#========= QUESTION 6 =========
# View the updated dataset
head(ridership_data)


# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)

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

ggplot(monthly_ridership_long, aes(x = month, y = trips, fill = service)) +
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


# Display the results
print("10 days with the highest ridership for Rapid Bus (KL) in 2023")
highest_ridership_2023[, c("date", "bus_rkl")]

print("10 days with the lowest ridership for Rapid Bus (KL) in 2023")
lowest_ridership_2023[, c("date", "bus_rkl")]





#========= QUESTION 8 =========
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

# Filter the data for January and February 2022
ridership_2022 <- ridership_data %>%
  filter(year(date) == 2022 & month(date) %in% c(1, 2))

# Create a new column to categorize the day of the week (Weekday or Weekend)
ridership_2022$day_type <- ifelse(weekdays(ridership_2022$date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

# Aggregate ridership by day_type (Weekday vs Weekend)
aggregated_ridership <- ridership_2022 %>%
  group_by(day_type) %>%
  summarise(total_ridership = sum(bus_rpn), .groups = 'drop')  # "bus_rpn" for Rapid Bus (Penang)



# Aggregate data by month and day_type
monthly_ridership_by_daytype <- ridership_2022 %>%
  group_by(month, day_type) %>%
  summarise(total_ridership = sum(bus_rpn), .groups = 'drop')



# Visualize the aggregated data using a bar plot
ggplot(aggregated_ridership, aes(x = day_type, y = total_ridership, fill = day_type)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_minimal() +
  labs(
    title = "Ridership Comparison for Rapid Bus (Penang) in January and February 2022",
    subtitle = "Weekdays vs Weekends",
    x = "Day Type",
    y = "Total Ridership",
    fill = "Day Type"
  ) +
  scale_y_continuous(labels = comma_format()) +  # Format y-axis to avoid scientific notation
  scale_fill_manual(values = c("Weekday" = "#4169E1", "Weekend" = "#FF6347"))  # Blue for Weekdays, Red for Weekend




# Visualize the trend over time by weekdays and weekends
ggplot(monthly_ridership_by_daytype, aes(x = month, y = total_ridership, color = day_type, group = day_type)) +
  geom_line(size = 1.2) +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Monthly Ridership Trend for Rapid Bus (Penang) in January and February 2022",
    subtitle = "Weekdays vs Weekends",
    x = "Month",
    y = "Total Ridership",
    color = "Day Type"
  ) +
  scale_color_manual(values = c("Weekday" = "#4169E1", "Weekend" = "#FF6347")) +  # Blue for Weekdays, Red for Weekend
  scale_y_continuous(labels = comma_format())  # Format y-axis to avoid scientific notation


