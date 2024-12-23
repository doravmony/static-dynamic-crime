
#### Preamble ####
# Purpose: Provides an extensive test suite for the simulated dataset.
# Author: Weiyang Li
# Date: 23 December 2024
# Contact: weiyang.li@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - The `testthat` package must be installed.
# - The simulated dataset (`simulated_data.csv`) should be available in the `data/00-simulate_data` directory.
# Any other information needed: 
# - This script tests data integrity, consistency, and conformance with expected structures and constraints.


#### Work Setup ####

library(testthat)
library(readr)
library(dplyr)



#### Test data ####

# Load the simulated dataset
simulated_data <- read_csv("data/00-simulate_data/simulated_data.csv")

#### Test Suite for Simulated Data ####
test_that("Simulated dataset structure is correct", {
  expect_named(simulated_data, c("AREA_NAME", "Year", "Crime_Type", "POPULATION_2023", "Crime_Count", "Crime_Rate_per_1000"))
  expect_true(is.data.frame(simulated_data))
  expect_true(all(c("AREA_NAME", "Year", "Crime_Type") %in% names(simulated_data)))
})

test_that("Simulated dataset types are correct", {
  expect_true(is.character(simulated_data$AREA_NAME))
  expect_true(is.numeric(simulated_data$Year))
  expect_true(is.character(simulated_data$Crime_Type))
  expect_true(is.numeric(simulated_data$POPULATION_2023))
  expect_true(is.numeric(simulated_data$Crime_Count))
  expect_true(is.numeric(simulated_data$Crime_Rate_per_1000))
})

test_that("Simulated dataset has valid values", {
  expect_true(all(simulated_data$POPULATION_2023 > 0))
  expect_true(all(simulated_data$Crime_Count >= 0))
  expect_true(all(simulated_data$Crime_Rate_per_1000 >= 0))
})

test_that("Simulated dataset has no missing values", {
  expect_false(any(is.na(simulated_data)))
})

test_that("Simulated dataset contains data for all years and crime types", {
  expect_true(length(unique(simulated_data$Year)) > 1) # Ensure multiple years are present
  expect_true(length(unique(simulated_data$Crime_Type)) > 1) # Ensure multiple crime types are present
})
