library(readr)
library(here)
library(dplyr)
library(ggplot2)
library(car)
data <- read_csv(here("data", "sentiments.csv"))
total_records <- nrow(data)
missing_values <- sum(is.na(data))
percentage_missing <- (missing_values / (total_records * ncol(data))) * 100
cat("Percentage of missing values in the dataset:", percentage_missing, "%\n")
missing_year <- sum(is.na(data$Year))
percentage_missing_year <- (missing_year / total_records) * 100
cat("Percentage of missing 'year' values:", percentage_missing_year, "%\n")

data$Year <- as.numeric(data$Year)
# Define a reasonable range for Year
valid_year_range <- c(1900, 2025)
# Replace extreme values with NA
data <- data %>%
  mutate(Year = ifelse(Year >= valid_year_range[1] & Year <= valid_year_range[2], Year, NA))

# Calculate the median year for each QualificationStudied
median_year_by_qualification <- data %>%
  group_by(QualificationStudied) %>%
  summarise(median_year = median(as.numeric(Year), na.rm = TRUE))
# Calculate the median year for each Region
median_year_by_region <- data %>%
  group_by(Region) %>%
  summarise(median_year = median(as.numeric(Year), na.rm = TRUE))
# Calculate the overall median year from the dataset
overall_median_year <- median(as.numeric(data$Year), na.rm = TRUE)
# Using the median 'Year' for each QualificationStudied
data <- data %>%
  left_join(median_year_by_qualification, by = "QualificationStudied") %>%
  mutate(Year = ifelse(is.na(Year), median_year, Year)) %>%
  select(-median_year)  # Remove the temporary column
# Second option: Use the median 'Year' for the corresponding Region
data <- data %>%
  left_join(median_year_by_region, by = "Region") %>%
  mutate(Year = ifelse(is.na(Year), median_year, Year)) %>%
  select(-median_year)  # Remove the temporary column
# Final option: Use the overall median year (2003) if both QualificationStudied and Region are missing
overall_median_year <- median(data$Year)
data$Year[is.na(data$Year)] <- overall_median_year
# Verify that there are no missing 'Year' values after imputation
print(paste("Number of missing values in 'Year' column:", sum(is.na(data$Year))))
