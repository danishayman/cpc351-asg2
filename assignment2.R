# path <- "D:/Projects/R projects/assignment2/Data"
path <- "/Users/ammaradnan/Documents/R_Source/cpc351-asg2/Data"
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
library(lubridate) # For hanlding dates

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

# -------------------------------------------------
# Question 1
# Create a frequency table of vehicles by State and Year
summary_table <- table(data$state, format(as.Date(data$date_reg), "%Y"))

summary_data <- as.data.frame(as.table(summary_table))
colnames(summary_data) <- c("state", "year", "total_vehicles")

# Create a stacked bar chart with filled colors
ggplot(summary_data, aes(x = year, y = total_vehicles, fill = state)) +
  geom_bar(stat = "identity", position = "stack", color="black") +
  labs(
    title = "Vehicle Registrations at JPJ Offices by State (2019-2024)",
    x = "Year",
    y = "Number of Vehicles",
    fill = "State"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "bottom"
  )

# -------------------------------------------------
# Question 2

# year
data$year <- format(as.Date(data$date_reg), "%Y")

# Filter for Pulau Pinang and relevant years
data_filtered <- data[data$state == "Pulau Pinang" & data$year %in% c("2019", "2020", "2021"), ]

# Count registrations by Brand
brand_counts <- aggregate(date_reg ~ year + maker, data_filtered, length)
colnames(brand_counts) <- c("year", "brand", "total_vehicles")

# Find top 5 brands for each year
top_brands_per_year <- do.call(rbind, lapply(split(brand_counts, brand_counts$year), function(df) {
  df[order(-df$total_vehicles), ][1:5, ]
}))

# Bar chart for top 5 brands
ggplot(top_brands_per_year, aes(x = reorder(brand, -total_vehicles), y = total_vehicles, fill = brand)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ year, scales = "free_x") +
  labs(
    title = "Top 5 Vehicle Brands in Pulau Pinang by Year (2019-2021)",
    x = "Brand",
    y = "Number of Registrations",
    fill = "Brand"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )


# -------------------------------------------------
# Question 3

# Define regions
regions <- list(
  "Northern" = c("Perlis", "Kedah", "Pulau Pinang", "Perak"),
  "Central" = c("Selangor", "Kuala Lumpur", "Putrajaya", "Negeri Sembilan"),
  "East Coast" = c("Pahang", "Terengganu", "Kelantan"),
  "Southern" = c("Melaka", "Johor"),
  "East Malaysia" = c("Sarawak", "Sabah", "Labuan")
)

# Assign region to each state
data$region <- sapply(data$state, function(state) {
  region <- names(Filter(function(x) state %in% x, regions))
  if (length(region) > 0) region else NA
})

# Count favorite colors per region
color_counts <- aggregate(date_reg ~ region + colour, data, length)
colnames(color_counts) <- c("Region", "Color", "Count")

# Find favorite color for each region
favorite_colors <- do.call(rbind, lapply(split(color_counts, color_counts$Region), function(df_color) {
  df_color[which.max(df_color$Count), ]
}))

# Display results
print(favorite_colors)

ggplot(favorite_colors, aes(x = Region, y = Count, fill = Color)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Favorite Vehicle Colors by Region",
    x = "Region",
    y = "Number of Vehicles",
    fill = "Color"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# -------------------------------------------------
# Question 4

# Count the number of vehicles by Year and FuelType
fuel_counts <- table(data$year, data$fuel)
fuel_counts <- as.data.frame(fuel_counts)
colnames(fuel_counts) <- c("Year", "FuelType", "Count")

ggplot(fuel_counts, aes(x = Year, y = Count, color = FuelType, group = FuelType)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +  # Adds points to the line chart for better visibility
  labs(
    title = "Trends in Vehicle Fuel Types (2019-2024)",
    x = "Year",
    y = "Number of Vehicles Registered",
    color = "Fuel Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# -------------------------------------------------
# Question 5

# Extract year and month
data <- data %>%
  mutate(
    year = year(date_reg),
    month = month(date_reg, label = TRUE, abbr = TRUE)
  )

data <- data %>%
  mutate(
    period = case_when(
      year < 2020 ~ "Pre-COVID",
      year >= 2020 ~ "Post-COVID"
    )
  )

# Count registrations per year
yearly_data <- data %>%
  group_by(year, period) %>%
  summarize(total_registrations = n(), .groups = "drop")

# Visualize yearly trends
ggplot(yearly_data, aes(x = year, y = total_registrations, fill = period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Yearly Vehicle Registrations in Malaysia",
    x = "Year",
    y = "Total Registrations",
    fill = "Period"
  ) +
  theme_minimal()

# Filter data for the COVID-19 period (2020–2021)
covid_data <- data %>%
  filter(year %in% c(2020, 2021))

# Count registrations per month
monthly_data <- covid_data %>%
  group_by(year, month) %>%
  summarize(total_registrations = n(), .groups = "drop")

# Visualize monthly trends
ggplot(monthly_data, aes(x = month, y = total_registrations, group = year, color = as.factor(year))) +
  geom_line(size = 1) +
  labs(
    title = "Monthly Vehicle Registrations During COVID-19",
    x = "Month",
    y = "Total Registrations",
    color = "Year"
  ) +
  theme_minimal()

# Summarize yearly registrations by state
state_yearly_data <- data %>%
  group_by(state, year) %>%
  summarize(total_registrations = n(), .groups = "drop")

# Visualize yearly trends by state
ggplot(state_yearly_data, aes(x = year, y = total_registrations, group = state, color = state)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Yearly Vehicle Registrations by State",
    x = "Year",
    y = "Total Registrations",
    color = "State"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Summarize vehicle type data
type_data <- data %>%
  group_by(year, type) %>%
  summarize(total_registrations = n(), .groups = "drop")

# Visualize distribution by vehicle type
ggplot(type_data, aes(x = year, y = total_registrations, fill = type)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Distribution of Vehicle Registrations by Type",
    x = "Year",
    y = "Proportion",
    fill = "Vehicle Type"
  ) +
  theme_minimal()

# Summarize fuel type data
fuel_data <- data %>%
  group_by(year, fuel) %>%
  summarize(total_registrations = n(), .groups = "drop")

# Visualize fuel type distribution
ggplot(fuel_data, aes(x = year, y = total_registrations, fill = fuel)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Fuel Type Distribution of Vehicle Registrations",
    x = "Year",
    y = "Proportion",
    fill = "Fuel Type"
  ) +
  theme_minimal()



