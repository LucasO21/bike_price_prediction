# BIKE PRICING PREDICTION ----
# FUNCTIONS SCRIPT ----
# # **** ----

# ******************************************************************************
# SETUP ----
# ******************************************************************************

# Working Dir ----
setwd(here::here("R"))

# * Libraries ----
library(tidyverse)
library(janitor)
library(tidymodels)

# ******************************************************************************
# LOAD ARTIFACTS ----
# ******************************************************************************

model_artifacts_list <- read_rds("../artifacts/model_artifacts.rds")

data_tbl <- read_rds("../data/trekbikes_clead_data.rds") 

data_tbl %>% View()
data_tbl %>% count(frame_material)


# PRICE PREDICTION FUNCTION ----
get_new_bike_price <- function(bike_model, bike_year, .ml_model){
    
    # model setup
    if (.ml_model == "XGBOOST") model = model_artifacts_list[[1]]
    if (.ml_model == "RANDOM FOREST") model = model_artifacts_list[[2]]
    if (.ml_model == "GLMNET") model = model_artifacts_list[[3]]
    if (.ml_model == "MARS") model = model_artifacts_list[[4]]
    
    new_bike_tbl <- data_tbl %>% 
        filter(model_name == bike_model & model_year == bike_year) 
    
    new_bike_price <- new_bike_tbl %>% 
        predict(model, new_data = .) %>% 
        bind_cols(new_bike_tbl) %>%
        rename(price = .pred) %>% 
        select(-c(model_base, model_tier, model_price))
    
    delta     <- (new_bike_price$price/new_bike_tbl$model_price) -1
    
    if (delta < 0) sign = "-"
    if (delta > 0) sign = "+"
    if (delta == 0) sign = ""
    
    delta_txt <- abs(delta) %>% scales::percent(accuracy = 0.3)

    
    new_bike_price <- new_bike_price %>% 
        mutate(price = str_glue("{price %>% scales::dollar(accuracy = 1)} // {sign}{delta_txt} vs National Average"))
    
    return(new_bike_price)
    
}

get_new_bike_price("Fuel EX 5 Gen 5", 2023, "RANDOM FOREST") %>% glimpse()


# PRICE PREDICTION TABLE ----
get_new_bike_price_table <- function(data){
    
    data_prep <- data %>% 
        select(model_name, product_id, everything(.)) %>% 
        rename(model_id = product_id,
               year = model_year) %>% 
        gather(key = "New Model Attribute", value = "value", -model_name, factor_key = TRUE) %>% 
        mutate(value = case_when(
            value == 0 ~ "No",
            value == 1 ~ "Yes",
            TRUE ~ value
        )) %>% 
        spread(key = model_name, value = value) %>% 
        mutate(`New Model Attribute` = `New Model Attribute` %>% str_replace_all("_", " ")) %>% 
        mutate(`New Model Attribute` = `New Model Attribute` %>% str_to_upper()) 
    
    return(data_prep)
}

get_new_bike_price("Fuel EX 5 Gen 5", 2023, "RANDOM FOREST") %>% 
    get_new_bike_price_table()


# PREP DATA FOR PREDICTION PLOT ----
get_price_prediction_data <- function(data_tbl, new_bike_tbl){
    
    data_tbl %>% 
        select(product_id, model_name, category, family, frame_material, model_price) %>% 
        mutate(estimate = "Actual") %>% 
        bind_rows(
            new_bike_tbl %>% 
                select(product_id, model_name, category, family, frame_material, model_price) %>% 
                mutate(estimate = "Prediction")
        ) 
    
    
}

get_price_prediction_data(data_tbl, new_bike_tbl) %>% tail()
