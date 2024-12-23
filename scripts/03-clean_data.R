
#### Preamble ####
# Purpose: Cleans the raw neighborhood crime data for analysis by selecting relevant columns, reshaping, and normalizing the data.
# Author: Weiyang Li
# Date: 23 December 2024
# Contact: weiyang.li@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - The `tidyverse`, `dplyr`, `tidyr`, and `arrow` packages must be installed.
# - Raw data must be saved in the `data/01-raw_data/raw_data.csv` file.
# - Ensure the working directory is set to the project folder.
# Description: 
# - This script focuses on preparing the data for analysis. It includes:
#   - Selecting columns related to population and crime rates for multiple years.
#   - Reshaping the data for year-wise analysis.
#   - Normalizing crime counts by population to calculate rates per 1,000.
#   - Handling missing values by removing incomplete entries.
# - The cleaned dataset is saved as a Parquet file for further analysis.



#### Workspace setup ####

library(tidyverse)
library(dplyr)
library(tidyr)
library(arrow) 


#### Clean data ####

raw_data <- read_csv("data/01-raw_data/raw_data.csv")


# Step 1: Select relevant columns for analysis
# We'll focus on population, assault rates, and theft rates across years
columns_to_keep <- c("AREA_NAME", "POPULATION_2023", 
                     grep("ASSAULT_\\d{4}$", names(raw_data), value = TRUE),
                     grep("AUTOTHEFT_\\d{4}$", names(raw_data), value = TRUE),
                     grep("BIKETHEFT_\\d{4}$", names(raw_data), value = TRUE),
                     grep("BREAKENTER_\\d{4}$", names(raw_data), value = TRUE),
                     grep("HOMICIDE_\\d{4}$", names(raw_data), value = TRUE),
                     grep("ROBBERY_\\d{4}$", names(raw_data), value = TRUE),
                     grep("SHOOTING_\\d{4}$", names(raw_data), value = TRUE),
                     grep("THEFTFROMMV_\\d{4}$", names(raw_data), value = TRUE),
                     grep("THEFTOVER_\\d{4}$", names(raw_data), value = TRUE))

# Filter only the relevant columns
clean_data <- raw_data %>%
  dplyr::select(all_of(columns_to_keep))

# Step 2: Reshape the data to a long format for year-wise analysis
# Expand ASSAULT data for each year into a column
crime_data_long <- clean_data %>%
  pivot_longer(
    cols = starts_with("ASSAULT_") | starts_with("AUTOTHEFT_") | starts_with("BIKETHEFT_") | 
      starts_with("BREAKENTER_") | starts_with("HOMICIDE_") | starts_with("ROBBERY_") | 
      starts_with("SHOOTING_") | starts_with("THEFTFROMMV_") | starts_with("THEFTOVER_"),
    names_to = c("Crime_Type", "Year"),
    names_pattern = "(.*)_(\\d+)",
    values_to = "Crime_Count")

# Step 3: Convert columns to appropriate data types
crime_data_long <- crime_data_long %>%
  mutate(
    Year = as.integer(Year),
    Crime_Count = as.numeric(Crime_Count))

# Step 4: Normalize crime counts per 1,000 population
crime_data_long <- crime_data_long %>%
  mutate(Crime_Rate_per_1000 = Crime_Count / POPULATION_2023 * 1000)

# Step 5: Missing values
crime_data_no_na <- crime_data_long %>%
  drop_na()


#### Save data ####

write_parquet(crime_data_no_na, "data/02-analysis_data/analysis_data.parquet")



