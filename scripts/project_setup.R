# Project setup
# Collect, tidy, and process the social and economic data for the UAZCC Catchment
# René Dario Herrera 
# 6 Oct 2021 
# The University of Arizona Cancer Center 
# renedherrera at email dot arizona dot edu 

# install packages 
install.packages("here")
install.packages("tidyverse")
install.packages("tidycensus")
install.packages("curl")
install.packages("readxl")

# load packages
library(here)

# create folders
dir.create(path = "data")
dir.create(path = "data/raw")
dir.create(path = "data/tidy")
dir.create(path = "scripts")
