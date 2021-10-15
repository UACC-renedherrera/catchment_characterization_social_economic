# household below poverty 

# load packages
library(here)
library(tidycensus)
library(tidyverse)

# load variable tables to find variables of interest
var_acs_2019_acs5 <- load_variables(2019, "acs5", cache = TRUE)
var_acs_2019_acs5_profile <- load_variables(2019, "acs5/profile", cache = TRUE)
var_acs_2019_acs5_subject <- load_variables(2019, "acs5/subject", cache = TRUE)
var_acs_2019_acs1 <- load_variables(2019, "acs1", cache = TRUE)

# functions
# catchment counties only 
# function 
subset_catchment <- function(x){
  counties <- c(
    "Cochise County, Arizona",
    "Pima County, Arizona",
    "Pinal County, Arizona",
    "Santa Cruz County, Arizona",
    "Yuma County, Arizona"
  )
  x %>%
    filter(x$NAME %in% counties)
}

# Poverty USA ----
# POVERTY STATUS IN THE PAST 12 MONTHS
# Survey/Program: American Community Survey
# Year: 2019
# Estimates: 5-Year
# Table ID: S1701
#
# Source: U.S. Census Bureau, 2015-2019 American Community Survey 5-Year Estimates

acs5_poverty_usa <- get_acs(
  geography = "us",
  variables = c(
    "rate" = "S1701_C03_001",
    "number" = "S1701_C02_001",
    "total" = "S1701_C01_001"
  ),
  cache_table = TRUE,
  year = 2019,
  survey = "acs5"
)

poverty_usa <- acs5_poverty_usa %>%
  select(NAME, variable, estimate) %>%
  spread(key = "variable", value = "estimate") %>%
  mutate(estimate = 100*(number/total))

# Poverty AZ ----
# population unemployment az
# POVERTY STATUS IN THE PAST 12 MONTHS
# Survey/Program: American Community Survey
# Year: 2019
# Estimates: 5-Year
# Table ID: S1701
#
# Source: U.S. Census Bureau, 2015-2019 American Community Survey 5-Year Estimates

acs5_poverty_az <- get_acs(
  geography = "state",
  state = "az",
  variables = c(
    "rate" = "S1701_C03_001",
    "number" = "S1701_C02_001",
    "total" = "S1701_C01_001"
  ),
  cache_table = TRUE,
  year = 2019,
  survey = "acs5"
)

poverty_az <- acs5_poverty_az %>%
  select(NAME, variable, estimate) %>%
  spread(key = "variable", value = "estimate") %>%
  mutate(estimate = 100*(number/total))

# Poverty Catchment ----
# population unemployment catch
# POVERTY STATUS IN THE PAST 12 MONTHS
# Survey/Program: American Community Survey
# Year: 2019
# Estimates: 5-Year
# Table ID: S1701
#
# Source: U.S. Census Bureau, 2015-2019 American Community Survey 5-Year Estimates

acs5_poverty_catch <- get_acs(
  geography = "county",
  state = "az",
  variables = c(
    "rate" = "S1701_C03_001",
    "number" = "S1701_C02_001",
    "total" = "S1701_C01_001"
  ),
  cache_table = TRUE,
  year = 2019,
  survey = "acs5"
)

poverty_catchment <- acs5_poverty_catch %>%
  subset_catchment() %>%
  select(NAME, variable, estimate) %>%
  spread(key = "variable", value = "estimate") %>%
  summarise(number = sum(number),
            total = sum(total)) %>%
  mutate(estimate = 100*(number/total),
         NAME = "Catchment")

# Poverty County ----
# population unemployment catch
# POVERTY STATUS IN THE PAST 12 MONTHS
# Survey/Program: American Community Survey
# Year: 2019
# Estimates: 5-Year
# Table ID: S1701
#
# Source: U.S. Census Bureau, 2015-2019 American Community Survey 5-Year Estimates

acs5_poverty_catch <- get_acs(
  geography = "county",
  state = "az",
  variables = c(
    "rate" = "S1701_C03_001",
    "number" = "S1701_C02_001",
    "total" = "S1701_C01_001"
  ),
  cache_table = TRUE,
  year = 2019,
  survey = "acs5"
)

poverty_catchment_counties <- acs5_poverty_catch %>%
  subset_catchment() %>% 
  select(NAME, variable, estimate) %>%
  spread(key = "variable", value = "estimate") %>%
  select(NAME, 
         estimate = rate)

# Poverty Catchment Hispanic ----

# S1701_C03_020

acs5_poverty_catch_race <- get_acs(
  geography = "county",
  state = "az",
  variables = c(
    "percent_white" = "S1701_C03_021",
    "percent_hisp" = "S1701_C03_020",
    "percent_ai" = "S1701_C03_015",
    "percent_black" = "S1701_C03_014"
  ),
  cache_table = TRUE,
  year = 2019,
  survey = "acs5"
)

poverty_catchment_nhw <- acs5_poverty_catch_race %>%
  subset_catchment() %>%
  filter(variable == "percent_white") %>%
  summarise(min = min(estimate),
            max = max(estimate))

poverty_catchment_nhw$range <- str_c(poverty_catchment_nhw$min, "-", poverty_catchment_nhw$max)

poverty_catchment_nhw <- poverty_catchment_nhw %>%
  mutate(variable = "Non-Hispanic White",
         NAME = "Catchment")

poverty_catchment_hisp <- acs5_poverty_catch_race %>%
  subset_catchment() %>%
  filter(variable == "percent_hisp") %>%
  summarise(min = min(estimate),
            max = max(estimate))

poverty_catchment_hisp$range <- str_c(poverty_catchment_hisp$min, "-", poverty_catchment_hisp$max)

poverty_catchment_hisp <- poverty_catchment_hisp %>%
  mutate(variable = "Hispanic",
         NAME = "Catchment")

poverty_catchment_ai <- acs5_poverty_catch_race %>%
  subset_catchment() %>%
  filter(variable == "percent_ai") %>%
  summarise(min = min(estimate),
            max = max(estimate))

poverty_catchment_ai$range <- str_c(poverty_catchment_ai$min, "-", poverty_catchment_ai$max)

poverty_catchment_ai <- poverty_catchment_ai %>%
  mutate(variable = "American Indian",
         NAME = "Catchment")

acs5_poverty_catch_race %>%
  subset_catchment() %>%
  filter(variable == "percent_black") %>%
  summarise(min = min(estimate),
            max = max(estimate))

poverty_catchment_ai$range <- str_c(poverty_catchment_ai$min, "-", poverty_catchment_ai$max)

poverty_catchment_ai <- poverty_catchment_ai %>%
  mutate(variable = "American Indian",
         NAME = "Catchment")

# combine everything to one table for output 
poverty_table <- bind_rows(
  poverty_usa,
  poverty_az,
  poverty_catchment,
  poverty_catchment_nhw,
  poverty_catchment_hisp,
  poverty_catchment_ai,
  poverty_catchment_counties
) %>%
  select(NAME,
         variable,
         estimate,
         range)

# save to disk 
write_csv(poverty_table, "data/tidy/poverty_level.csv")
