# Author: Ashlynn Wimer
# Date: 11/7/2023
#
# About: Script used to clean IRS county level migration data from 2008-2016.
#        Note that we have to treat data from prior to 2011 and after 2011
#        differently, due to changes in IRS data formatting and collection.

library(tidyr)
library(dplyr)
library(purrr)
library(stringr)

PREFIX = '../data/IRS/'

YEARS = c('2008-2009', '2009-2010', 
          '2010-2011', '2011-2012', 
          '2012-2013', '2013-2014', 
          '2014-2015', '2015-2016')

#### Inflows

# Directories
old_format_inflow <- c('inflow/countyinflow0809.csv',
                       'inflow/countyinflow0910.csv',
                       'inflow/countyinflow1011.csv')

new_format_inflow <- c('inflow/countyinflow1112.csv',
                       'inflow/countyinflow1213.csv',
                       'inflow/countyinflow1314.csv',
                       'inflow/countyinflow1415.csv',
                       'inflow/countyinflow1516.csv')


# Clean our data tables
old_format_inflows <- old_format_inflow |> 
  map(.f = ~str_c(PREFIX, .x)) |> # fix names
  map(.f = ~read.csv(.x) |> # format Hell
        mutate(State_Code_Dest = str_pad(State_Code_Dest, 2, 'left', '0'),
                   County_Code_Dest = str_pad(County_Code_Dest, 3, 'left', '0'),
                   d_fips = str_c(State_Code_Dest, County_Code_Dest)) |>
        select(d_fips, Return_Num, Exmpt_Num, Aggr_AGI) |>
        group_by(d_fips) |>
        summarise(n_return_in = sum(Return_Num),
                  n_exempt_in = sum(Exmpt_Num),
                  agi = sum(Aggr_AGI)))

new_format_inflows <- new_format_inflow |>
  map(.f = ~str_c(PREFIX, .x)) |>
  map(.f = ~read.csv(.x) |>
        mutate(y2_statefips = str_pad(y2_statefips, 2, 'left', '0'),
               y2_countyfips = str_pad(y2_countyfips, 3, 'left', '0'),
               d_fips = str_c(y2_statefips, y2_countyfips)) |>
        select(d_fips, n1, n2, agi) |>
        group_by(d_fips) |>
        summarise(n_return_in = sum(n1),
                  n_exempt_in = sum(n2),
                  agi = sum(agi)))


# Add a year column and merge.
old_format_inflows <- old_format_inflows |> 
  map(.x = 1:length(old_format_inflows), 
      .f = ~mutate(old_format_inflows[[.x]], 
                   year = YEARS[[.x]])) |>
  reduce(.f = rbind)

new_format_inflows <- new_format_inflows |>
  map(.x = 1:length(new_format_inflows),
      .f = ~mutate(new_format_inflows[[.x]], 
                   year= YEARS[[.x + 3]]
                   )) |>
  reduce(.f = rbind)

# Combine the old and new data.
inflows <- rbind(old_format_inflows, new_format_inflows)

#### Outflows

# Directories
old_format_outflow <- c('outflow/countyoutflow0809.csv',
                        'outflow/countyoutflow0910.csv',
                        'outflow/countyoutflow1011.csv')

new_format_outflow <- c('outflow/countyoutflow1112.csv',
                        'outflow/countyoutflow1213.csv',
                        'outflow/countyoutflow1314.csv',
                        'outflow/countyoutflow1415.csv',
                        'outflow/countyoutflow1516.csv')

# Clean data tables
old_format_outflows <- old_format_outflow |> 
  map(.f = ~str_c(PREFIX, .x)) |> # fix names
  map(.f = ~read.csv(.x) |> # format Hell
        mutate(State_Code_Origin = str_pad(State_Code_Origin, 2, 'left', '0'),
               County_Code_Origin = str_pad(County_Code_Origin, 3, 'left', '0'),
               o_fips = str_c(State_Code_Origin, County_Code_Origin)) |>
        select(o_fips, Return_Num, Exmpt_Num, Aggr_AGI) |>
        group_by(o_fips) |>
        summarise(n_return_out = sum(Return_Num),
                  n_exempt_out = sum(Exmpt_Num),
                  agi = sum(Aggr_AGI)))

new_format_outflows <- new_format_outflow |>
  map(.f = ~str_c(PREFIX, .x)) |>
  map(.f = ~read.csv(.x) |>
        mutate(y1_statefips = str_pad(y1_statefips, 2, 'left', '0'),
               y1_countyfips = str_pad(y1_countyfips, 3, 'left', '0'),
               o_fips = str_c(y1_statefips, y1_countyfips)) |>
        select(o_fips, n1, n2, agi) |>
        group_by(o_fips) |>
        summarise(n_return_out = sum(n1),
                  n_exempt_out = sum(n2),
                  agi = sum(agi)))

# Tag and combine
old_format_outflows <- old_format_outflows |> 
  map(.x = 1:length(old_format_outflows), 
      .f = ~mutate(old_format_outflows[[.x]], 
                   year = YEARS[[.x]])) |>
  reduce(.f = rbind)

new_format_outflows <- new_format_outflows |>
  map(.x = 1:length(new_format_outflows),
      .f = ~mutate(new_format_outflows[[.x]], 
                   year= YEARS[[.x + 3]]
      )) |>
  reduce(.f = rbind)

# Final merge
outflows <- rbind(new_format_outflows, old_format_outflows)


#### Save

write.csv(outflows, '../data/irs_outflows.csv')
write.csv(inflows, '../data/irs_inflows.csv')

# Also save geographic information
sf::st_write(tigris::counties(state="Michigan"), '../data/shapes/mi_counties.shp')
