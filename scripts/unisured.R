# uninsured 

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

# uninsured usa ----
# COMPARATIVE ECONOMIC CHARACTERISTICS
# Survey/Program: American Community Survey
# Year: 2019
# Estimates: 5-Year
# Table ID: CP03

# Source: U.S. Census Bureau, 2015-2019 American Community Survey 5-Year Estimates

acs5_population_insurance <- tibble(
  area = c("United States",
           "AZ",
           "Cochise",
           "Pima",
           "Pinal",
           "Santa Cruz",
           "Yuma"),
  prop = c(0.094,
           0.109,
           0.082,
           0.099,
           0.088,
           0.116,
           0.133),
  denominator = c(317941631,
                  6838270,
                  116140,
                  998748,
                  393843,
                  46251,
                  201230))

acs5_population_insurance <- acs5_population_insurance %>%
  mutate(estimate = prop * denominator) %>%
  select(NAME = area, 
         numerator = estimate, 
         denominator, 
         prop)

insurance_usa <- acs5_population_insurance %>%
  filter(NAME == "United States") %>%
  mutate(estimate = 100*prop)

# uninsured az ----
# COMPARATIVE ECONOMIC CHARACTERISTICS
# Survey/Program: American Community Survey
# Year: 2019
# Estimates: 5-Year
# Table ID: CP03
#
# Source: U.S. Census Bureau, 2015-2019 American Community Survey 5-Year Estimates

insurance_az <- acs5_population_insurance %>%
  filter(NAME == "AZ") %>%
  mutate(estimate = 100*prop)

# uninsured catchment ----
# COMPARATIVE ECONOMIC CHARACTERISTICS
# Survey/Program: American Community Survey
# Year: 2019
# Estimates: 5-Year
# Table ID: CP03
#
# Source: U.S. Census Bureau, 2015-2019 American Community Survey 5-Year Estimates

insurance_catchment <- acs5_population_insurance %>%
  filter(!(NAME == "AZ" | NAME == "United States")) %>%
  summarise(numerator = sum(numerator),
            denominator = sum(denominator)) %>%
  mutate(prop = numerator / denominator) %>%
  mutate(estimate = 100*prop,
         NAME = "Catchment")

# uninsured county ----
# COMPARATIVE ECONOMIC CHARACTERISTICS
# Survey/Program: American Community Survey
# Year: 2019
# Estimates: 5-Year
# Table ID: CP03
#
# Source: U.S. Census Bureau, 2015-2019 American Community Survey 5-Year Estimates

insurance_catchment_counties <- acs5_population_insurance %>%
  filter(!(NAME == "AZ" | NAME == "United States")) %>%
  mutate(estimate = 100*(numerator / denominator))

# uninsured catchment white ----
# HEALTH INSURANCE COVERAGE STATUS BY AGE (WHITE ALONE, NOT HISPANIC OR LATINO)
# Survey/Program: American Community Survey
# Universe: Hispanic or Latino civilian noninstitutionalized population
# Year: 2019
# Estimates: 1-Year
# Table ID: B27001H
#
# Source: U.S. Census Bureau, 2019 American Community Survey 1-Year Estimates

acs1_population_insurance_white <- get_acs(geography = "county",
                                           state = "az",
                                           year = 2019,
                                           survey = "acs1",
                                           variables = c(
                                             "Total" = "B27001H_001",
                                             "age 6 under" = "B27001H_004",
                                             "age 6-18" = "B27001H_007",
                                             "age 19-25" = "B27001H_010",
                                             "age 26-34" = "B27001H_013",
                                             "age 35-44" = "B27001H_016",
                                             "age 45-54" = "B27001H_019",
                                             "age 55-64" = "B27001H_022",
                                             "age 65-74" = "B27001H_025",
                                             "age 75 over" = "B27001H_028"
                                           )
)

# total denominator
acs1_population_insurance_white_den <- acs1_population_insurance_white %>%
  subset_catchment() %>%
  filter(variable == "Total") %>%
  summarise(estimate = sum(estimate)) %>%
  as.numeric()

# numerator
insurance_catchment_nhw <- acs1_population_insurance_white %>%
  subset_catchment() %>%
  filter(variable != "Total") %>%
  summarise(estimate = sum(estimate)) %>%
  mutate(total = acs1_population_insurance_white_den,
         estimate = 100*(estimate / total)) %>%
  mutate(variable = "Non-Hispanic White",
         NAME = "Catchment")

# population uninsured catchment hispanic ----
# HEALTH INSURANCE COVERAGE STATUS BY AGE (HISPANIC OR LATINO)
# Survey/Program: American Community Survey
# Universe: Hispanic or Latino civilian noninstitutionalized population
# Year: 2019
# Estimates: 1-Year
# Table ID: B27001I
#
# Source: U.S. Census Bureau, 2019 American Community Survey 1-Year Estimates

acs1_population_insurance_hisp <- get_acs(geography = "county",
                                          state = "az",
                                          year = 2019,
                                          survey = "acs1",
                                          variables = c(
                                            "Total" = "B27001I_001",
                                            "age 6 under" = "B27001I_004",
                                            "age 6-18" = "B27001I_007",
                                            "age 19-25" = "B27001I_010",
                                            "age 26-34" = "B27001I_013",
                                            "age 35-44" = "B27001I_016",
                                            "age 45-54" = "B27001I_019",
                                            "age 55-64" = "B27001I_022",
                                            "age 65-74" = "B27001I_025",
                                            "age 75 over" = "B27001I_028"
                                          )
)


# total denominator
acs1_population_insurance_hisp_den <- acs1_population_insurance_hisp %>%
  subset_catchment() %>%
  filter(variable == "Total") %>%
  summarise(estimate = sum(estimate)) %>%
  as.numeric()

# numerator
insurance_catchment_hisp <- acs1_population_insurance_hisp %>%
  subset_catchment() %>%
  filter(variable != "Total") %>%
  summarise(estimate = sum(estimate)) %>%
  mutate(total = acs1_population_insurance_hisp_den,
         estimate = 100*(estimate / total)) %>%
  mutate(variable = "Hispanic",
         NAME = "Catchment")

# population uninsured catchment american indian ----
# HEALTH INSURANCE COVERAGE STATUS BY AGE (AMERICAN INDIAN AND ALASKA NATIVE ALONE)
# Survey/Program: American Community Survey
# Universe: Hispanic or Latino civilian noninstitutionalized population
# Year: 2019
# Estimates: 1-Year
# Table ID: B27001C
#
# Source: U.S. Census Bureau, 2019 American Community Survey 1-Year Estimates

acs1_population_insurance_ai <- get_acs(geography = "county",
                                        state = "az",
                                        year = 2019,
                                        survey = "acs1",
                                        variables = c(
                                          "Total" = "B27001C_001",
                                          "age 6 under" = "B27001C_004",
                                          "age 6-18" = "B27001C_007",
                                          "age 19-25" = "B27001C_010",
                                          "age 26-34" = "B27001C_013",
                                          "age 35-44" = "B27001C_016",
                                          "age 45-54" = "B27001C_019",
                                          "age 55-64" = "B27001C_022",
                                          "age 65-74" = "B27001C_025",
                                          "age 75 over" = "B27001C_028"
                                        )
)


# total denominator
acs1_population_insurance_ai_den <- acs1_population_insurance_ai %>%
  subset_catchment() %>%
  filter(variable == "Total") %>%
  drop_na()%>%
  summarise(estimate = sum(estimate)) %>%
  as.numeric()

# numerator
insurance_catchment_ai <- acs1_population_insurance_ai %>%
  subset_catchment() %>%
  filter(variable != "Total") %>%
  drop_na() %>%
  summarise(estimate = sum(estimate)) %>%
  mutate(total = acs1_population_insurance_ai_den,
         estimate = 100*(estimate / total)) %>%
  mutate(variable = "American Indian",
         NAME = "Catchment")

# bind rows
uninsured_table <- bind_rows(
  insurance_usa,
  insurance_az,
  insurance_catchment,
  insurance_catchment_nhw,
  insurance_catchment_hisp,
  insurance_catchment_ai,
  insurance_catchment_counties
) %>%
  select(NAME, 
         variable,
         estimate)

# save to disk 
write_rds(uninsured_table, "data/tidy/uninsured.rds")
write_csv(uninsured_table, "data/tidy/uninsured.csv")
