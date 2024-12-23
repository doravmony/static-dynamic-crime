
#### Preamble ####
# Purpose: Perform exploratory data analysis (EDA) on the cleaned crime data to understand
#          variable distributions and temporal/spatial trends.
# Author: Weiyang Li
# Date: 23 December 2024
# Contact: weiyang.li@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
#   - The cleaned dataset ("analysis_data.parquet") must be available in the directory "data/02-analysis_data/"
#   - Required R packages: arrow, dplyr, ggplot2
# Any other information needed? Ensure the dataset was successfully cleaned and validated 
# before proceeding with this script.



#### Workspace setup ####

library(arrow)
library(dplyr)
library(ggplot2)



#### Read data ####

crime_data <- read_parquet("data/02-analysis_data/analysis_data.parquet")



#### Exploring Variables ####


# Histogram for Crime_Rate_per_1000
ggplot(crime_data, aes(x = Crime_Rate_per_1000)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Crime Rate per 1000", x = "Crime Rate per 1000", y = "Frequency")

# Bar plot for Crime_Type
ggplot(crime_data, aes(x = Crime_Type)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Count of Crime Types", x = "Crime Type", y = "Count")

# Line plot for crime rates over years
ggplot(crime_data, aes(x = Year, y = Crime_Rate_per_1000, color = Crime_Type)) +
  geom_line(stat = "summary", fun = mean) +
  labs(title = "Temporal Trends in Crime Rates", x = "Year", y = "Average Crime Rate per 1000")

# Time Trend by Community
ggplot(crime_data, aes(x = Year, y = Crime_Rate_per_1000, color = AREA_NAME)) +
  geom_line(stat = "summary", fun = mean) +
  labs(title = "Crime Rate Trends by Community", x = "Year", y = "Average Crime Rate per 1000") +
  theme(legend.position = "none")

# Crime rate by Community
ggplot(crime_data, aes(x = Crime_Type, y = Crime_Rate_per_1000)) +
  geom_boxplot(fill = "steelblue", outlier.color = "red") +
  labs(title = "Crime Rate per 1000 by Crime Type", x = "Crime Type", y = "Crime Rate per 1000")


