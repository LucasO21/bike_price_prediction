# FUNCTIONS FOR ANALYSIS ----
# **** ----

# Libraries ----
library(tidyverse)
library(janitor)

# * Load Data ----
bike_data_raw_tbl <- data.table::fread("../data/cannondale_bikes_2019.csv") %>% 
    as_tibble() %>% 
    clean_names()

bike_data_raw_tbl %>% glimpse()
