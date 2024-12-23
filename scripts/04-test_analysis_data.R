
#### Preamble ####
# Purpose: Test the cleaned analysis dataset for integrity, consistency, and correctness.
# Author: Weiyang Li
# Date: 23 December 2024
# Contact: weiyang.li@mail.utoronto.ca
# License: MIT
# Pre-requisites:
# - The `testthat`, `validate`, and `pointblank` packages must be installed.
# - The cleaned dataset must be saved in the `data/02-analysis_data/analysis_data.parquet` file.
# Description:
# - This script performs extensive tests on the analysis dataset, ensuring:
#   - Column names and types are as expected.
#   - Data integrity checks, such as no negative crime counts.
#   - Completeness of key variables.
#   - Valid ranges for numerical columns.
#   - Normalization of crime rates is consistent with population values.
# - The tests aim to ensure the dataset is suitable for downstream analysis.



#### Workspace setup ####

library(testthat)
library(validate)
library(pointblank)
library(arrow)
library(dplyr)

# Load the dataset

analysis_data <- read_parquet("data/02-analysis_data/analysis_data.parquet")




#### Test data ####

# 1. Test column names and types
test_that("Column names and types are correct", {
  expected_columns <- c("AREA_NAME", "POPULATION_2023", "Crime_Type", "Year", "Crime_Count", "Crime_Rate_per_1000")
  expect_named(analysis_data, expected_columns)
  expect_type(analysis_data$AREA_NAME, "character")
  expect_type(analysis_data$POPULATION_2023, "double")
  expect_type(analysis_data$Crime_Type, "character")
  expect_type(analysis_data$Year, "integer")
  expect_type(analysis_data$Crime_Count, "double")
  expect_type(analysis_data$Crime_Rate_per_1000, "double")
})

# 2. Validate no missing values in key columns
validate_no_missing <- validator(
  !is.na(AREA_NAME),
  !is.na(POPULATION_2023),
  !is.na(Crime_Type),
  !is.na(Year),
  !is.na(Crime_Count),
  !is.na(Crime_Rate_per_1000)
)
check_no_missing <- confront(analysis_data, validate_no_missing)
summary(check_no_missing)

# 3. Test for valid ranges
test_that("Values are within valid ranges", {
  expect_gte(min(analysis_data$Crime_Count, na.rm = TRUE), 0)
  expect_gte(min(analysis_data$Crime_Rate_per_1000, na.rm = TRUE), 0)
  expect_gte(min(analysis_data$POPULATION_2023, na.rm = TRUE), 0)
})

# 4. Test for normalization correctness
test_that("Crime rate normalization is correct", {
  analysis_data <- analysis_data %>%
    mutate(Calculated_Rate = Crime_Count / POPULATION_2023 * 1000)
  expect_equal(analysis_data$Crime_Rate_per_1000, analysis_data$Calculated_Rate, tolerance = 1e-9)
})

# 5. Completeness of Crime_Type categories
test_that("Crime_Type has all expected categories", {
  expected_crime_types <- c("ASSAULT", "AUTOTHEFT", "BIKETHEFT", "BREAKENTER", "HOMICIDE",
                            "ROBBERY", "SHOOTING", "THEFTFROMMV", "THEFTOVER")
  expect_setequal(unique(analysis_data$Crime_Type), expected_crime_types)
})

# 6. Pointblank validation suite
agent <- create_agent(
  tbl = analysis_data,
  tbl_name = "Analysis Dataset",
  label = "Pointblank Validation for Analysis Data"
) %>%
  col_vals_not_null(columns = c("AREA_NAME", "POPULATION_2023", "Crime_Type", "Year", "Crime_Count", "Crime_Rate_per_1000")) %>%
  col_vals_gte(columns = vars(Crime_Count, Crime_Rate_per_1000, POPULATION_2023), value = 0) %>%
  col_vals_between(columns = vars(Year), left = 2014, right = 2023) %>%
  interrogate()


#### Results ####

# Print the summary of validation results
summary(check_no_missing)

# Print agent report
agent
