# Food Insecurity 

# set up ----
# load packages
library(here)
library(tidyverse)
library(readxl)
library(tidycensus)

#sources
# Gunderson, Craig, et al. Map the Meal Gap 2020: A Report on County and Congressional District Food Insecurity and County Food Cost in the United States in 2018. Feeding America, 2020.
# U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates

# load variables of US Census acs1 into environment
acs1_2018 <- load_variables(2018, "acs1", cache = TRUE)

# to get the denominator in order to calculate rates
# get from acs1 total population of USA ----
population_us <- get_acs(
  geography = "us",
  variables = c("population" = "B01003_001"),
  year = 2018,
  survey = "acs1"
)

# get from acs1 total population of AZ ----
population_az <- get_acs(
  geography = "state",
  state = "az",
  variables = c("population" = "B01003_001"),
  year = 2018,
  survey = "acs1"
)

# get from acs5 total population of UAZCC catchment ----
# acs five year estimate because
# The one-year ACS provides data for geographies with populations of 65,000 and greater.
population_az_counties <- get_acs(
  geography = "county",
  state = "az",
  variables = c("population" = "B01003_001"),
  year = 2018,
  survey = "acs5"
)

# save catchment counties to value
counties <- c(
  "Cochise",
  "Pima",
  "Pinal",
  "Santa Cruz",
  "Yuma"
)

# use stringr to clean up county name
# and save
population_az_counties <- population_az_counties %>%
  mutate(NAME = str_replace(population_az_counties$NAME, " County, Arizona", ""))

# combine all population into one dataframe
population_tbl <- bind_rows(
  population_us,
  population_az,
  population_az_counties
)

# population estimates for each area
population_tbl

# total population
population_catchment <- population_az_counties %>%
  filter(NAME %in% counties) %>%
  summarise(estimate = sum(estimate)) %>%
  mutate(NAME = "Catchment")

# total population in catchment
population_catchment

population_tbl <- bind_rows(
  population_tbl,
  population_catchment
)

# data import
# food insecurity
# state data for the map the meal gap ----
mmg2020_state <- read_xlsx("data/raw/MMG2020_2018Data_ToShare_Updated.xlsx",
                           sheet = "2018 State",
                           range = "A2:R53",
                           col_names = TRUE,
                           na = "",
                           trim_ws = TRUE,
                           skip = 1
)

glimpse(mmg2020_state)

# display column names
names(mmg2020_state)

# food insecurity rate usa ----
food_insecure_us <- mmg2020_state %>%
  select(
    `State Name`,
    "# of Food Insecure Persons in 2018"
  ) %>%
  summarise(food_insecure = sum(`# of Food Insecure Persons in 2018`)) %>%
  mutate(total = population_us$estimate) %>%
  mutate(
    prop = food_insecure / total,
    NAME = "United States"
  )

# calculated food insecurity rate usa
food_insecure_us

# save for processing later
write_rds(mmg2020_state, "data/tidy/food_insecure_usa_states.rds")
write_csv(mmg2020_state, "data/tidy/food_insecure_usa_states.csv")

# food insecurity rate az ----
food_insecure_az <- mmg2020_state %>%
  filter(State == "AZ") %>%
  select(
    `State Name`,
    "# of Food Insecure Persons in 2018"
  ) %>%
  summarise(food_insecure = sum(`# of Food Insecure Persons in 2018`)) %>%
  mutate(total = population_az$estimate) %>%
  mutate(
    prop = food_insecure / total,
    NAME = "Arizona"
  )

# calculated food insecurity rate az
food_insecure_az

# county data for the map the meal gap ----
mmg2020_county <- read_xlsx("data/raw/MMG2020_2018Data_ToShare_Updated.xlsx",
                            sheet = "2018 County",
                            range = "A2:R3144",
                            col_names = TRUE,
                            na = "",
                            trim_ws = TRUE,
                            skip = 1
)

# save for processing later
write_rds(mmg2020_county, "data/tidy/food_insecure_usa_counties.rds")
write_csv(mmg2020_county, "data/tidy/food_insecure_usa_counties.csv")

glimpse(mmg2020_county)

# filter to AZ only
mmg2020_county <- mmg2020_county %>%
  filter(State == "AZ")

# use stringr to clean up county name
mmg2020_county <- mmg2020_county %>%
  mutate(`County, State` = str_replace(mmg2020_county$`County, State`, " County, Arizona", ""))

# save for processing later
write_rds(mmg2020_county, "data/tidy/food_insecure_az_counties.rds")
write_csv(mmg2020_county, "data/tidy/food_insecure_az_counties.csv")

# food insecurity az counties
food_insecure_az_counties <- mmg2020_county %>%
  select(
    State,
    NAME = `County, State`,
    food_insecure = "# of Food Insecure Persons in 2018"
  ) %>%
  mutate(total = population_az_counties$estimate) %>%
  mutate(prop = food_insecure / total) %>%
  select(!(State))

# food insecurity shown for all az counties
food_insecure_az_counties

# catchment food insecurity rate ----
food_insecure_catch <- mmg2020_county %>%
  filter(`County, State` %in% counties) %>%
  select(
    State,
    `County, State`,
    "# of Food Insecure Persons in 2018"
  ) %>%
  summarise(food_insecure = sum(`# of Food Insecure Persons in 2018`)) %>%
  mutate(total = population_catchment$estimate) %>%
  mutate(
    prop = food_insecure / total,
    NAME = "Catchment"
  )

# food insecurity for catchment altogether
food_insecure_catch

# combine all food insecurity values to one
food_insecurity <- bind_rows(
  food_insecure_us,
  food_insecure_az,
  food_insecure_catch,
  food_insecure_az_counties
)

# food insecurity shown for all areas
food_insecurity <- food_insecurity %>%
  select(
    NAME,
    food_insecure,
    total,
    prop
  )

# print food insecurity for all areas
food_insecurity

# save
write_rds(
  food_insecurity,
  "data/tidy/food_insecurity.rds"
)

food_insecurity %>%
  mutate(estimate = 100*prop) %>%
  select(NAME, estimate) %>%
  write_csv("data/tidy/food_insecurity.csv")


# set up ----
# load packages
library(here)
library(tidyverse)
library(ggthemes)

# load data to environment
# already prepared table for UAZCC
food_insecurity_table <- read_rds("data/tidy/food_insecurity.rds")

# data provided by MMG for counties in USA
food_insecure_counties <- read_rds("data/tidy/food_insecure_usa_counties.rds")

# data provided by MMG for states in USA
food_insecure_states <- read_rds("data/tidy/food_insecure_usa_states.rds")

glimpse(food_insecure_states)

# plot of food insecurity for AZ counties, in comparison to USA, AZ, and catchment
food_insecurity_table %>%
  ggplot(mapping = aes(x = reorder(NAME, prop), y = prop)) +
  geom_bar(stat = "identity", fill = "#0C234B") +
  coord_flip() +
  theme_clean() +
  ylim(0, .25) +
  geom_text(aes(label = round(prop, digits = 3), hjust = -.25)) +
  labs(title = "Food Insecurity in Arizona",
       subtitle = "For Arizona counties in 2018",
       y = "Proportion",
       x = "",
       caption = "Sources: Map the Meal Gap 2020 \n U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates")

glimpse(food_insecure_counties)

# explore the county dataset 
food_insecure_az_counties <- food_insecure_counties %>%
  filter(State == "AZ")

# use stringr to clean up county name
# and save
food_insecure_az_counties <- food_insecure_az_counties %>%
  mutate(`County, State` = str_replace(food_insecure_az_counties$`County, State`, " County, Arizona", ""))

# proportion of food insecure above poverty threshold 
food_insecure_az_counties %>% 
  ggplot(mapping = aes(x = reorder(`County, State`, `% FI > High Threshold`), y = `% FI > High Threshold`)) +
  geom_bar(stat = "identity", fill = "#0C234B") +
  coord_flip() +
  theme_clean() +
  ylim(0, .5) +
  geom_text(aes(label = round(`% FI > High Threshold`, digits = 3), hjust = -.25))  +
  labs(title = "% of Food Insecure > Threshold in Arizona",
       subtitle = "Proportion of food insecure households above poverty \nthreshold for Arizona counties in 2018",
       y = "Proportion",
       x = "",
       caption = "Sources: Map the Meal Gap 2020")

# child food insecurity rate
food_insecure_az_counties %>% 
  ggplot(mapping = aes(x = reorder(`County, State`, `2018 Child food insecurity rate`), y = `2018 Child food insecurity rate`)) +
  geom_bar(stat = "identity", fill = "#0C234B") +
  coord_flip() +
  theme_clean() +
  ylim(0, .5) +
  geom_text(aes(label = round(`2018 Child food insecurity rate`, digits = 3), hjust = -.25))  +
  labs(title = "Child Food Insecurity Rate",
       subtitle = "Proportion of children living in food insecure households \nfor Arizona counties in 2018",
       y = "Proportion",
       x = "",
       caption = "Sources: Map the Meal Gap 2020")

# food insecurity rate by cost per meal 
food_insecure_az_counties %>% 
  ggplot(mapping = aes(y = `2018 Food Insecurity Rate`, x = `2018 Cost Per Meal`)) +
  geom_point() +
  geom_text(aes(label = `County, State`)) +
  theme_bw()


# state comparisons
# proportion of food insecure above poverty threshold 
food_insecure_states %>% 
  ggplot(mapping = aes(x = reorder(`State Name`, `% FI > High Threshold`), y = `% FI > High Threshold`)) +
  geom_bar(stat = "identity", fill = "#0C234B") +
  coord_flip() +
  theme_clean() +
  ylim(0, .75) +
  geom_text(aes(label = round(`% FI > High Threshold`, digits = 3), hjust = -.25))  +
  labs(title = "% of Food Insecure > Threshold in USA",
       subtitle = "Proportion of food insecure households above poverty \nthreshold for USA states in 2018",
       y = "Proportion",
       x = "",
       caption = "Sources: Map the Meal Gap 2020")

# child food insecurity rate
food_insecure_states %>% 
  ggplot(mapping = aes(x = reorder(`State Name`, `2018 Child Food Insecurity Rate`), y = `2018 Child Food Insecurity Rate`)) +
  geom_bar(stat = "identity", fill = "#0C234B") +
  coord_flip() +
  theme_clean() +
  ylim(0, .5) +
  geom_text(aes(label = round(`2018 Child Food Insecurity Rate`, digits = 3), hjust = -.25))  +
  labs(title = "Child Food Insecurity Rate",
       subtitle = "Proportion of children living in food insecure households \nfor Arizona counties in 2018",
       y = "Proportion",
       x = "",
       caption = "Sources: Map the Meal Gap 2020")

# food insecurity rate by cost per meal 
food_insecure_states %>% 
  ggplot(mapping = aes(y = `2018 Food Insecurity Rate`, x = `2018 Cost Per Meal`)) +
  geom_point() +
  geom_text(aes(label = `State Name`)) +
  theme_bw()
