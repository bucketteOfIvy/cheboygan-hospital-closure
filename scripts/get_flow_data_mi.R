# Author: Ashlynn Wimer
# Date: 11/3/23
#
# This script pulls flow data from every county subdivision in Michigan from 
# 2010 to 2020 using tidycensus `get_flows`. It then merges and saves the data
# sets.

library(tidycensus)
library(purrr)
library(dplyr)
library(tidyr)
library(here)

setwd('C:\\Users\\wimer\\Documents\\classes\\gisc27104\\final assignment\\scripts')


#### County Subdivisions

# Get a list of every county in Michigan
counties <- tigris::counties(state = "MI") |> 
  select(NAME) |>
  sf::st_drop_geometry()

# List of relevant years
years <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016)

# List of all combinations of counties and year
counties_by_year <- expand_grid(counties, years)

# Function which, given a county and year, grabs all flow data
# for its subdivisions and returns net summaries
get_reduced_flows <- function(cnty, yr) {
  rv <- get_flows(geography = 'county subdivision',
                  state     = 'MI',
                  county    = cnty,
                  output    = 'wide',
                  year      = yr,
                  geometry  = FALSE) |> 
    group_by(GEOID1) |> 
    summarize(MOVEDOUT_M = sum(MOVEDOUT, na.rm = TRUE),
              MOVEDIN    = sum(MOVEDIN,  na.rm = TRUE),
              MOVEDOUT   = sum(MOVEDOUT, na.rm = TRUE),
              MOVEDNET   = sum(MOVEDNET, na.rm = TRUE),
              MOVEDNET_M = sum(MOVEDNET, na.rm = TRUE)) |>
  mutate(year = yr)
    
  return(rv)
}

# Grab our data for every combination of year and county
# Fix this so that the year is not. What it now is.
data <- map(.x = 1:nrow(counties_by_year), 
         .f = ~get_reduced_flows(counties_by_year[[ .x, 1 ]], 
                                  counties_by_year[[ .x, 2 ]])) |>
  reduce(rbind)

# Names
county_sds <- tigris::county_subdivisions("MI") |> 
  select(NAME, GEOID) |> 
  sf::st_drop_geometry()

data <- merge(data, county_sds, by.x = 'GEOID1', by.y = 'GEOID') |>
  rename(GEOID = GEOID1)


#### Save

# Save data
write.csv(data, '../data/michigan-longitudinal-flow-data.csv', row.names=F)

# Save shapes
sf::st_write(tigris::county_subdivisions("MI"), '../data/shapes/mi-county-subs.shp')

