# BIKE PRICING PREDICTION ----
# FUNCTIONS SCRIPT ----
# # **** ----

# ******************************************************************************
# SETUP ----
# ******************************************************************************

# Working Dir ----
# setwd(here::here("app", "app_functions))

# * Libraries ----
library(tidyverse)
library(janitor)
library(tidymodels)


# ******************************************************************************
# LOAD ARTIFACTS ----
# ******************************************************************************

# model_artifacts_list <- read_rds("../artifacts/model_artifacts.rds")
# 
# data_tbl <- read_rds("../data/bikes_data_clean_tbl.rds") %>% filter(frame_material %in% c("Carbon", "Aluminum"))
# 
# data_tbl %>% View()
# data_tbl %>% count(frame_material)
# 
# data <- data_tbl
# bike_model <- "Madone SLR eTap Gen 7"
# bike_year  <- 2023
# .ml_model  <- "RANDOM FOREST"

ml_model_xgboost    <- readRDS("../app_artifacts/model_artifacts.rds")[[1]]
ml_model_rf         <- readRDS("../app_artifacts/model_artifacts.rds")[[2]]
ml_model_glmnet     <- readRDS("../app_artifacts/model_artifacts.rds")[[3]]
ml_model_mars       <- readRDS("../app_artifacts/model_artifacts.rds")[[4]]
# bike_family <-  "Aero"
# bike_frame_material <-  "Carbon"

# ******************************************************************************
# FUNCTIONS ----
# ******************************************************************************

# PRICE PREDICTION FUNCTION ----
get_new_bike_price <- function(data, bike_model, bike_family, bike_frame_material, .ml_model){
    
    # model setup
    if (.ml_model == "XGBOOST") model = ml_model_xgboost
    if (.ml_model == "RANDOM FOREST") model = ml_model_rf
    if (.ml_model == "GLMNET") model = ml_model_glmnet
    if (.ml_model == "MARS") model = ml_model_mars
    
    new_bike_tbl <- data %>% 
        filter(model_name == bike_model) %>% 
        mutate(
            family = bike_family,
            frame_material = bike_frame_material
        ) %>% 
        mutate(
            battery = ifelse(family == "Electric", 1, battery),
            charger = ifelse(family == "Electric", 1, charger)
        ) %>% 
        mutate(model_year = 2023)
        
    
    new_bike_pred_tbl <- new_bike_tbl %>% 
        predict(model, new_data = .) %>% 
        bind_cols(new_bike_tbl) %>%
        rename(price = .pred) %>% 
        select(-c(model_base, model_tier, model_price)) %>% 
        rename(model_price = price)
    
    # delta     <- (new_bike_pred_tbl$model_price/new_bike_tbl$model_price) -1
    # 
    # if (delta < 0) sign = "-"
    # if (delta > 0) sign = "+"
    # if (delta == 0) sign = ""
    
    # delta_txt <- abs(delta) %>% scales::percent(accuracy = 0.3)

    # 
    # new_bike_pred_tbl_fmt <- new_bike_pred_tbl %>% 
    #     mutate(model_price = str_glue("{model_price %>% scales::dollar(accuracy = 1)} // {sign}{delta_txt} vs National Average"))
    
    # if (return_fmt) {
    #     ret <- new_bike_pred_tbl_fmt
    # } else {
    #     ret <- new_bike_pred_tbl
    # }
    
    return(new_bike_pred_tbl)
    
}

# get_new_bike_price(data_tbl, bike_model, "Electric", "Aluminum", "RANDOM FOREST") %>% glimpse()




# PRICE PREDICTION TABLE ----
get_new_bike_price_table <- function(data){
    
    data_prep <- data %>% 
        mutate(model_price = model_price %>% scales::dollar(accuracy = 1)) %>% 
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

# get_new_bike_price(data_tbl, bike_model, "Electric", "Aluminum", "RANDOM FOREST") %>%
#     get_new_bike_price_table()



# PREP DATA FOR PREDICTION PLOT ----
get_price_prediction_data <- function(data, new_bike_pred_tbl){
    
    bike_name <- new_bike_pred_tbl %>% pull(model_name)
    
    data %>% 
        select(product_id, model_name, category, family, frame_material, model_price) %>% 
        mutate(estimate = "Actual") %>% 
        filter(!model_name == bike_name) %>% 
        bind_rows(
            new_bike_pred_tbl %>% 
                select(product_id, model_name, category, family, frame_material, model_price) %>% 
                mutate(estimate = "Prediction")
        ) 
    
    
}

# get_price_prediction_data(
#     data_tbl,
#     get_new_bike_price(data_tbl, bike_model, "Electric", "Aluminum", "RANDOM FOREST")
#     
# ) %>% tail()


# PREDICTION PLOT ----
get_price_prediction_plot <- function(data){
    
    p <- data %>% 
        mutate(family = fct_reorder(family, model_price)) %>% 
        mutate(frame_material = frame_material %>% fct_rev()) %>% 
        mutate(label_text = str_glue("Unit Price: {scales::dollar(model_price, accuracy = 1)}
                                     Model: {model_name}
                                     Category: {category}
                                     Family: {family}
                                     Frame Material: {frame_material}")) %>% 
        ggplot(aes(family, model_price, color = estimate))+
        geom_violin(scale = "width")+
        geom_jitter(aes(text = label_text), width = 0.1, alpha = 0.6, size = 2)+
        facet_wrap(~ frame_material)+
        coord_flip()+
        scale_y_log10(labels = scales::dollar_format(accuracy = 1))+
        tidyquant::scale_color_tq()+
        tidyquant::theme_tq()+
        theme(strip.text.x = element_text(margin = margin(6, 6, 6, 6)))+
        theme(strip.text.x = element_text(size = 12, face = "bold"))+
        labs(title = NULL, x = NULL, y = "Log Scale")
    
    plotly::ggplotly(p, tooltip = "text")
    
    
    
}

# get_price_prediction_data(
#     data_tbl,
#     get_new_bike_price(data_tbl, bike_model, "Electric", "Aluminum", "RANDOM FOREST")
# 
# ) %>% 
#     get_price_prediction_plot()


