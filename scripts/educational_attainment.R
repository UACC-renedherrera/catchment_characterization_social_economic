# data tidy 
# Collect, tidy, and process the social and economic data for the UAZCC Catchment
# high school graduation 
# some college 
# college graduate 
# unemployment 
# uninsured 
# food insecurity
# households below poverty level
# René Dario Herrera 
# 6 Oct 2021 
# The University of Arizona Cancer Center 
# renedherrera at email dot arizona dot edu 

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

# HS grad; some college; college grad
# educational attainment usa ----
# EDUCATIONAL ATTAINMENT
# Survey/Program: American Community Survey
# Year: 2019
# Estimates: 5-Year
# Table ID: S1501
#
# Source: U.S. Census Bureau, 2015-2019 American Community Survey 5-Year Estimates

acs5_population_edu_usa <- get_acs(
  geography = "us",
  variables = c(
    "Population 25 years and over" = "S1501_C01_006",
    "High school graduate (includes equivalency)" = "S1501_C02_009",
    "Some college, no degree" = "S1501_C02_010",
    "Bachelor's degree or higher" = "S1501_C02_012"
  ),
  year = 2019,
  cache_table = TRUE,
  survey = "acs5"
)

# HS grad; some college; college grad
# educational attainment az ----
# EDUCATIONAL ATTAINMENT
# Survey/Program: American Community Survey
# Year: 2019
# Estimates: 5-Year
# Table ID: S1501
#
# Source: U.S. Census Bureau, 2015-2019 American Community Survey 5-Year Estimates

acs5_population_edu_az <- get_acs(
  geography = "state",
  state = "AZ",
  variables = c(
    "Population 25 years and over" = "S1501_C01_006",
    "High school graduate or higher" = "S1501_C02_009",
    "Some college, no degree" = "S1501_C02_010",
    "Bachelor's degree or higher" = "S1501_C02_012"
  ),
  year = 2019,
  cache_table = TRUE,
  survey = "acs5"
)

# educational attainment catchment ----
# EDUCATIONAL ATTAINMENT
# Survey/Program: American Community Survey
# Year: 2019
# Estimates: 5-Year
# Table ID: S1501
#
# Source: U.S. Census Bureau, 2015-2019 American Community Survey 5-Year Estimates

acs5_population_edu_catch <- get_acs(
  geography = "county",
  state = "AZ",
  variables = c(
    "Population 25 years and over" = "S1501_C01_006",
    # "High school graduate or higher percent" = "S1501_C02_009",
    "High school graduate or higher" = "S1501_C01_009",
    # "Some college, no degree percent" = "S1501_C02_010",
    "Some college, no degree" = "S1501_C01_010",
    # "Bachelor's degree or higher percent" = "S1501_C02_012",
    "Bachelor's degree or higher" = "S1501_C01_012"
  ),
  year = 2019,
  cache_table = TRUE,
  survey = "acs5"
)

# create a value of the total catchment population
val_catch_edu_den <- acs5_population_edu_catch %>%
  subset_catchment() %>%
  filter(variable == "Population 25 years and over") %>%
  summarise(estimate = sum(estimate)) %>%
  as.numeric()

# HS grad; some college; college grad
# for catchment
acs5_population_edu_catch <- acs5_population_edu_catch %>%
  subset_catchment() %>%
  # filter(variable != "Population 25 years and over") %>%
  group_by(variable) %>%
  summarise(numerator = sum(estimate)) %>%
  ungroup() %>%
  mutate(denominator = val_catch_edu_den,
         prop = 100*(numerator / denominator),
         NAME = "Catchment") %>%
  mutate(prop = if_else(variable == "Population 25 years and over", numerator, prop)) %>%
  select(NAME, 
         variable, 
         estimate = prop) 

acs5_population_edu_catch_counties <- get_acs(
  geography = "county",
  state = "AZ",
  variables = c(
    "Population 25 years and over" = "S1501_C01_006",
    "High school graduate or higher percent" = "S1501_C02_009",
    # "High school graduate or higher" = "S1501_C01_009",
    "Some college, no degree percent" = "S1501_C02_010",
    # "Some college, no degree" = "S1501_C01_010",
    "Bachelor's degree or higher percent" = "S1501_C02_012"
    # "Bachelor's degree or higher" = "S1501_C01_012"
  ),
  year = 2019,
  cache_table = TRUE,
  survey = "acs5"
)

# for counties
acs5_population_edu_catch_counties <- acs5_population_edu_catch_counties %>%
  subset_catchment()

# education attainment catchment race ----
# high school graduate or equivalency

acs5_edu_catch_race <- get_acs(
  geography = "county",
  state = "az",
  cache_table = TRUE,
  year = 2019,
  survey = "acs5",
  variables = c(
    "white_total" = "S1501_C01_031",
    "white_hs" = "S1501_C01_032",
    "white_bach" = "S1501_C01_033",
    "hisp_total" = "S1501_C01_052",
    "hisp_hs" = "S1501_C01_053",
    "hisp_bach" = "S1501_C01_054",
    "ai_total" = "S1501_C01_037",
    "ai_hs" = "S1501_C01_038",
    "ai_bach" = "S1501_C01_039"
  )
) 

acs5_edu_catch_race

# white hs grad
acs5_edu_catch_race_white <- acs5_edu_catch_race %>%
  subset_catchment() %>%
  filter(grepl("white_", variable)) %>%
  group_by(variable) %>%
  summarize(estimate = sum(estimate)) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(prop_nhw_hs = 100*(white_hs / white_total),
         prop_nhw_coll_grad = 100*(white_bach / white_total)) %>%
  select(3:5) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "estimate") %>%
  mutate(NAME = "Non-Hispanic White") %>%
  mutate(variable = if_else(grepl("_total", variable), "Population 25 years and over", as.character(variable)))

# FLAG
# hisp hs grad
acs5_edu_catch_race_hisp <- acs5_edu_catch_race %>%
  subset_catchment() %>%
  filter(grepl("hisp_", variable)) %>%
  group_by(variable) %>%
  summarize(estimate = sum(estimate)) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(prop_hisp_hs = 100*(hisp_hs / hisp_total),
         prop_hisp_coll_grad = 100*(hisp_bach / hisp_total)) %>%
  select(3:5) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "estimate") %>%
  mutate(NAME = "Hispanic") %>%
  mutate(variable = if_else(grepl("_total", variable), "Population 25 years and over", as.character(variable)))

# american indian hs grad
acs5_edu_catch_race_ai <- acs5_edu_catch_race %>%
  subset_catchment() %>%
  filter(grepl("ai_", variable)) %>%
  group_by(variable) %>%
  summarize(estimate = sum(estimate)) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(prop_ai_hs = 100*(ai_hs / ai_total),
         prop_ai_coll_grad = 100*(ai_bach / ai_total)) %>%
  select(3:5) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "estimate") %>%
  mutate(NAME = "American Indian") %>%
  mutate(variable = if_else(grepl("_total", variable), "Population 25 years and over", as.character(variable)))

# educational attainment county ----
# EDUCATIONAL ATTAINMENT
# Survey/Program: American Community Survey
# Year: 2019
# Estimates: 5-Year
# Table ID: S1501
#
# Source: U.S. Census Bureau, 2015-2019 American Community Survey 5-Year Estimates

# acs5_population_edu_catch_counties <- get_acs(
#   geography = "county",
#   state = "AZ",
#   variables = c(
#     "Population 25 years and over" = "S1501_C01_006",
#     "High school graduate or higher" = "S1501_C02_009",
#     "Some college, no degree" = "S1501_C02_010",
#     "Bachelor's degree or higher" = "S1501_C02_012"
#   ),
#   year = 2019,
#   cache_table = TRUE,
#   survey = "acs5"
# ) %>%
#   subset_catchment()

# population educational attainment catchment hispanic ----
# EDUCATIONAL ATTAINMENT
# Survey/Program: American Community Survey
# Year: 2019
# Estimates: 5-Year
# Table ID: S1501
#
# Source: U.S. Census Bureau, 2015-2019 American Community Survey 5-Year Estimates
# 
# acs5_population_edu_catch_hisp <- get_acs(
#   geography = "county",
#   state = "AZ",
#   variables = c(
#     "Hispanic or Latino Population 25 years and over" = "S1501_C01_052",
#     "Hispanic or Latino Bachelor's degree or higher" = "S1501_C02_054"
#   ),
#   year = 2019,
#   cache_table = TRUE,
#   survey = "acs5"
# ) %>%
#   subset_catchment()

# build the table for the UAZCC Catchment Table 
table_edu <- full_join(acs5_population_edu_usa, acs5_population_edu_az) # USA, AZ
table_edu <- full_join(table_edu, acs5_population_edu_catch) # catchment
table_edu <- full_join(table_edu, acs5_edu_catch_race_white) # catchment non-hispanic white 
table_edu <- full_join(table_edu, acs5_edu_catch_race_hisp) # catchment hispanic 
table_edu <- full_join(table_edu, acs5_edu_catch_race_ai) # catchment american indian 
table_edu <- full_join(table_edu, acs5_population_edu_catch_counties) # catchment counties

# 
glimpse(table_edu)
unique(table_edu$NAME)
unique(table_edu$variable)

subset_catchment_ <- function(x){
  area <- c(
    "Cochise County, Arizona",
    "Pima County, Arizona",
    "Pinal County, Arizona",
    "Santa Cruz County, Arizona",
    "Yuma County, Arizona",
    "United States",
    "Arizona",
    "Non-Hispanic White",
    "Hispanic",
    "American Indian"
  )
  x %>%
    filter(x$name %in% area)
}

table_edu_ <- table_edu %>%  
  mutate(name = if_else(grepl("_nhw", variable), "Non-Hispanic White", if_else(
           grepl("_hisp", variable), "Hispanic", if_else(
             grepl("_ai", variable), "American Indian", as.character(NAME)))),
         variable = if_else(grepl("High school graduate", variable), "High school graduate", if_else(
           grepl("_hs", variable), "High school graduate", as.character(variable))),
         variable = if_else(grepl("Bachelor's degree", variable), "Bachelor's degree or higher", as.character(variable)),
         variable = if_else(grepl("Some college,", variable), "Some college, no degree", as.character(variable))) %>%
  mutate(variable = if_else(grepl("_coll_grad", variable), "Bachelor's degree or higher", as.character(variable))) 

table_edu_ %>%
  write_rds("data/tidy/educational_attainment.rds")

unique(table_edu_$name)
unique(table_edu_$variable)

table_edu_ %>%
  filter(name == "United States")

table_edu_ %>%
  filter(name == "Arizona")

table_edu_ %>%
  filter(name == "Catchment")


# 
table_edu_ %>%
  select(3,4,6) %>%
  pivot_wider(
    names_from = name,
    values_from = estimate
  ) %>%
  write_csv("outputs/table_social_economic factors_educational_attainment.csv")
