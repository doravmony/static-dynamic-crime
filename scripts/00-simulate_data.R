
#### Preamble ####
# Purpose: Simulates a dataset resembling crime statistics in Toronto, including
# community names, years, crime types, population, crime counts, and crime rates.
# Author: Weiyang Li
# Date: 23 December 2024
# Contact: weiyang.li@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - The `tidyverse` package must be installed.
# - Make sure you are in the appropriate R project directory.
# Any other information needed: 
# - This script creates a simulated dataset and saves it as `simulated_data.csv`.


#### Workspace setup ####

# Load required libraries
library(tidyverse)

# Set seed for reproducibility
set.seed(123)




#### Simulate data ####

# Define parameters for the simulation
n_areas <- 100  # Number of communities/neighborhoods
n_years <- 10   # Number of years
crime_types <- c("ASSAULT", "AUTOTHEFT", "BIKETHEFT", "BREAKENTER", "HOMICIDE", "ROBBERY", "SHOOTING", "THEFTFROMMV", "THEFTOVER")

# Create a data frame for simulation
simulated_data <- expand.grid(
  AREA_NAME = paste("Area", 1:n_areas),
  Year = 2014:(2014 + n_years - 1),
  Crime_Type = crime_types
)

# Add population for each area (simulated as a random value between 5000 and 40000)
area_populations <- data.frame(
  AREA_NAME = paste("Area", 1:n_areas),
  POPULATION_2023 = sample(5000:40000, n_areas, replace = TRUE)
)

# Merge population into the simulated dataset
simulated_data <- simulated_data %>%
  left_join(area_populations, by = "AREA_NAME")

# Simulate crime counts for each crime type and year
# Crime counts are simulated using a Poisson distribution with varying rates
simulated_data <- simulated_data %>%
  mutate(
    Crime_Count = rpois(n(), lambda = ifelse(Crime_Type == "HOMICIDE", 0.2, runif(1, 1, 5) * (POPULATION_2023 / 10000))),
    Crime_Rate_per_1000 = (Crime_Count / POPULATION_2023) * 1000
  )

# Save the simulated dataset
write_csv(simulated_data, "data/00-simulate_data/simulated_data.csv")

