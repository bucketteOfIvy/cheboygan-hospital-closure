# Author: Ashlynn Wimer
# Date: 11/3/23
#
# About: This script pulls county and county subdivision level economic
#        and population ACS 5-Year estimates for 2011 and 2016.

library(tidycensus)
library(tidyr)
library(dplyr)

### Variables ###
# B19326_001 : Median Income for the Past 12 Months
# B19083_001 : Gini Index of Inequality
# B01003_001 : Estimated Total Population

### County Subdivision

county_subdiv <- c(2011, 2016) |>
  purrr::lmap(.f = ~ get_acs(geography = 'County Subdivision',
                             state     = 'Michigan',
                             county    = 'Cheboygan',
                             variables = c('MedInc' = 'B19326_001',
                                           'Gini'   = 'B19083_001',
                                           'TotPop' = 'B01003_001'),
                             year      = .x,
                             output    = 'wide',
                             geometry  = FALSE) |>
                mutate(Year = .x)) |>
  purrr::reduce(rbind)

### County

county <- c(2011, 2016) |>
  purrr::lmap(.f = ~ get_acs(geography = 'County',
                             state     = 'Michigan',
                             variables = c('MedInc' = 'B19326_001',
                                          'Gini'    = 'B19083_001',
                                          'TotPop'  = 'B01003_001'),
                             year      = .x,
                             output    = 'wide',
                             geometry  = FALSE) |>
                mutate(Year = .x)) |>
  purrr::reduce(rbind)

### Save

write.csv(county_subdiv, '../data/county_subdiv_econ_data.csv', row.names = FALSE)
write.csv(county, '../data/county_econ_data.csv', row.names = FALSE)
