
#### Preamble ####
# Purpose: Downloads the raw data for neighborhood crime rates from the Toronto Open Data portal.
# Author: Weiyang Li
# Date: 23 December 2024
# Contact: weiyang.li@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - The `opendatatoronto` package must be installed.
# - The `dplyr` package must be installed.
# - An active internet connection is required to access the Toronto Open Data portal.
# Any other information needed? Ensure the working directory is set to the project folder.



#### Workspace setup ####

library(opendatatoronto)
library(dplyr)



#### Download data ####

# get all resources for this package
resources <- list_package_resources("neighbourhood-crime-rates")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv'))

# load the first datastore resource as a sample
data <- filter(datastore_resources, row_number()==1) %>% get_resource()


#### Save data ####

# Save the raw data
write.csv(data, "data/01-raw_data/raw_data.csv", row.names = FALSE)

