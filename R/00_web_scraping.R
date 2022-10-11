# BIKE PRICING PREDICTION ----
# WEBSCRAPING SCRIPT ----
# # **** ----

# Working Dir ----
setwd(here::here("R"))

# * Libraries ----
library(tidyverse)
library(stringi)
library(janitor)
library(rvest)
library(fs)
library(furrr)
library(xopen)
library(tictoc)

# Website URL ----
# "https://www.trekbikes.com/us/en_US/"


# ******************************************************************************
# GET URLS FOR PRODUCT CATEGORIES ----
# ******************************************************************************

# * Road Bikes ----
road_bikes_url_tbl <- tibble(
    family = c("Aero", "Lightweight", "Endurance", "Gravel", "Electric"),
    url = c(
        "https://www.trekbikes.com/us/en_US/bikes/road-bikes/performance-road-bikes/madone/c/B213/",
        "https://www.trekbikes.com/us/en_US/bikes/road-bikes/performance-road-bikes/%C3%A9monda/c/B211/",
        "https://www.trekbikes.com/us/en_US/bikes/road-bikes/performance-road-bikes/domane/c/B221/",
        "https://www.trekbikes.com/us/en_US/bikes/road-bikes/gravel-bikes/checkpoint/c/B224/",
        "https://www.trekbikes.com/us/en_US/bikes/electric-bikes/electric-road-bikes/c/B552/"
    )
) %>% 
    mutate(category = "Road", .before = family)

# * Mountain Bikes ----
mountain_bikes_url_tbl <- tibble(
    family = c("Cross Country"),
    url = c(
        "https://www.trekbikes.com/us/en_US/bikes/mountain-bikes/cross-country-mountain-bikes/top-fuel/c/B311/",
        "https://www.trekbikes.com/us/en_US/bikes/mountain-bikes/cross-country-mountain-bikes/supercaliber/c/B314/",
        "https://www.trekbikes.com/us/en_US/bikes/mountain-bikes/cross-country-mountain-bikes/procaliber/c/B312/",
        "https://www.trekbikes.com/us/en_US/bikes/mountain-bikes/cross-country-mountain-bikes/x-caliber/c/B315/",
        "https://www.trekbikes.com/us/en_US/bikes/mountain-bikes/cross-country-mountain-bikes/marlin/c/B321/",
        "https://www.trekbikes.com/us/en_US/bikes/mountain-bikes/cross-country-mountain-bikes/820/c/B324/"
    )
) %>% 
    bind_rows(
        tibble(
            family = c("Train"),
            url = c(
                "https://www.trekbikes.com/us/en_US/bikes/mountain-bikes/trail-mountain-bikes/slash/c/B341/",
                "https://www.trekbikes.com/us/en_US/bikes/mountain-bikes/trail-mountain-bikes/remedy/c/B331/",
                "https://www.trekbikes.com/us/en_US/bikes/mountain-bikes/trail-mountain-bikes/fuel-ex/c/B332/",
                "https://www.trekbikes.com/us/en_US/bikes/mountain-bikes/trail-mountain-bikes/roscoe/c/B342/"
            )
        )
    ) %>% 
    bind_rows(
        tibble(
            family = c("Electric"),
            url = c(
                "https://www.trekbikes.com/us/en_US/bikes/mountain-bikes/electric-mountain-bikes/powerfly/c/B337/",
                "https://www.trekbikes.com/us/en_US/bikes/mountain-bikes/electric-mountain-bikes/powerfly-equipped/c/B355/",
                "https://www.trekbikes.com/us/en_US/bikes/mountain-bikes/electric-mountain-bikes/rail/c/B344/",
                "https://www.trekbikes.com/us/en_US/bikes/mountain-bikes/electric-mountain-bikes/e-caliber/c/B356/",
                "https://www.trekbikes.com/us/en_US/bikes/mountain-bikes/electric-mountain-bikes/fuel-exe/c/B346/"
            )
        )
    ) %>% 
    bind_rows(
        tibble(
            family = c("Fat", "Downhill"),
            url = c(
                "https://www.trekbikes.com/us/en_US/bikes/mountain-bikes/fat-bikes/farley/c/B336/",
                "https://www.trekbikes.com/us/en_US/bikes/mountain-bikes/downhill-mountain-bikes/session/c/B351/"
            )
        )
    ) %>% 
    mutate(category = "Mountain", .before = family)

# * City Bikes ----
city_bikes_url_tbl <- tibble(
    family = c("Urban & Commuter", "Electric & Hybrid", "Comfort & Recreation", 
               "Fitness", "Townie"),
    url = c(
        "https://www.trekbikes.com/us/en_US/bikes/hybrid-bikes/urban-commuter-bikes/c/B440/?pageSize=72&q=%3Arelevance&sort=relevance#",
        "https://www.trekbikes.com/us/en_US/bikes/hybrid-bikes/electric-hybrid-bikes/c/B550/?pageSize=72&q=%3Arelevance&sort=relevance#",
        "https://www.trekbikes.com/us/en_US/bikes/hybrid-bikes/comfort-recreation-bikes/c/B410/?pageSize=72&q=%3Arelevance&sort=relevance#",
        "https://www.trekbikes.com/us/en_US/bikes/hybrid-bikes/fitness-bikes/c/B420/?pageSize=72&q=%3Arelevance&sort=relevance#",
        "https://www.trekbikes.com/us/en_US/bikes/electra-bikes/townie/c/EB300/?pageSize=72&q=%3Arelevance&sort=relevance#"
    )
) %>% 
    mutate(category = "City", .before = family)


# * Combine Bike Tables
final_bikes_url_tbl <- bind_rows(
    road_bikes_url_tbl,
    mountain_bikes_url_tbl,
    city_bikes_url_tbl
)



# ******************************************************************************
# EXPERIMENTING WITH JUST 1 URL ----
# ******************************************************************************
base_url  <- "https://www.trekbikes.com/"
url       <- "https://www.trekbikes.com/us/en_US/bikes/mountain-bikes/downhill-mountain-bikes/session/c/B351/"
url_html  <- read_html(url)

url_html %>%
    html_nodes(".product-tile__link") %>% 
    html_attr("href") %>% 
    unique() %>% 
    enframe(name = "position", value = "product_url") %>% 
    mutate(full_product_url = paste0(base_url, url)) %>% 
    filter(!str_detect(product_url, "frameset"))

# Function To Get Product URLs ----
get_bikes_urls <- function(url) {
    
    base_url  <- "https://www.trekbikes.com/"
    url_html  <- read_html(url)
    
    url_html %>%
        html_nodes(".product-tile__link") %>% 
        html_attr("href") %>% 
        unique() %>% 
        enframe(name = "position", value = "product_url") %>% 
        filter(!str_detect(product_url, "frameset")) %>% 
        mutate(full_product_url = paste0(base_url, product_url)) %>% 
        select(-product_url)
    
}

get_bikes_urls(url)


# Scale Up To All Product Categories ----
plan("multisession")
bikes_url_tbl <- road_bikes_url_tbl %>% 
    mutate(product_urls = future_map(url, get_bikes_urls)) 

bikes_url_tbl <- bikes_url_tbl %>% 
    unnest() %>% 
    mutate(product_id = gsub("[^0-9.-]", "", stri_sub(full_product_url, -6))) 

bikes_url_tbl %>% View()


# Web Scrape Bike Information ----
url <- "https://www.trekbikes.com/us/en_US/bikes/hybrid-bikes/electric-hybrid-bikes/dual-sport/dual-sport-2/p/35854/?colorCode=grey"

# product_name <- read_html(url) %>% 
#     html_element("[product-name]") %>% 
#     html_attr("product-name")
# 
# product_name <- read_html(url) %>% 
#     html_nodes("title") %>% 
#     html_text()
# 
# product_name <- sub("\\|.*", "", product_name)

product_name <- read_html(url) %>% 
    html_element("#gtm-product-name") %>% 
    html_attr("value")

product_id <- read_html(url) %>% 
    html_element("#gtm-product-code") %>% 
    html_attr("value") %>% 
    as.numeric()

product_price <- read_html(url) %>% 
    html_element("#gtm-product-display-price") %>% 
    html_attr("value") %>% 
    as.numeric()

product_year <- read_html(url) %>% 
    html_element("[product-model-year]") %>% 
    html_attr("product-model-year") %>% 
    as.numeric()

product_image_url <- read_html(url) %>% 
    html_element("[image-url]") %>% 
    html_attr("image-url")
    

specs_tbl <- read_html(url) %>% 
    html_elements('[class="sprocket__table spec"]') %>%
    html_table()

specs_cols_to_keep <- c("Frame", "Fork", "Shock", "Rim", "Tire", "Shifter", 
                        "Front derailleur", "Rear derailleur",
                        "Chain", "Brake", "Battery", "Charger", "Controller", 
                        "Motor", "Weight")

specs_tbl <- tibble(X1 = specs_cols_to_keep) %>% 
    left_join(
        bind_rows(specs_tbl) %>%
            filter(X1 %in% specs_cols_to_keep)
    ) %>% 
    spread(X1, X2) %>% 
    clean_names()

final_product_tbl <- tibble(product_id, product_name, product_price, product_year,
                            product_image_url) %>% 
    bind_cols(specs_tbl)

bike_urls_tbl %>% 
    mutate(product_id = gsub("[^0-9.-]", "", stri_sub(my_string, -6))) %>% 
    View()


specs_tbl %>% View()


# ******************************************************************************
# CREATE FUNCTIONS ----
# ******************************************************************************

# * Function To Get Product URLs ----
get_bikes_urls <- function(url) {
    
    base_url  <- "https://www.trekbikes.com/"
    url_html  <- read_html(url)
    
    url_tbl <- url_html %>%
        html_nodes(".product-tile__link") %>% 
        html_attr("href") %>% 
        unique() %>% 
        enframe(name = "position", value = "product_url") %>% 
        filter(!str_detect(product_url, "frameset")) %>% 
        mutate(full_product_url = paste0(base_url, product_url)) %>% 
        select(-product_url)
    
    return(url_tbl)
    
}


# * Function To Scrape Data Fore Each Product URL ----
get_bikes_data <- function(url){
    
    # product name
    product_name <- read_html(url) %>% 
        html_element("#gtm-product-name") %>% 
        html_attr("value")
    
    # product id
    product_id <- read_html(url) %>% 
        html_element("#gtm-product-code") %>% 
        html_attr("value") %>% 
        as.numeric()
    
    # product price
    product_price <- read_html(url) %>% 
        html_element("#gtm-product-display-price") %>% 
        html_attr("value") %>% 
        as.numeric()
    
    # product year
    product_year <- read_html(url) %>% 
        html_element("[product-model-year]") %>% 
        html_attr("product-model-year") %>% 
        as.numeric()
    
    # product image url
    product_image_url <- read_html(url) %>% 
        html_element("[image-url]") %>% 
        html_attr("image-url")
    
    # product specs
    specs_info <- read_html(url) %>% 
        html_elements('[class="sprocket__table spec"]') %>%
        html_table()
    
    # specs table columns
    specs_cols_to_keep <- c("Frame", "Fork", "Shock", "Rim", "Tire", "Shifter", 
                            "Front derailleur", "Rear derailleur",
                            "Chain", "Brake", "Battery", "Charger", "Controller", 
                            "Motor", "Weight")
    
    # specs dataframe
    specs_tbl <- tibble(X1 = specs_cols_to_keep) %>% 
        left_join(
            bind_rows(specs_info) %>%
                filter(X1 %in% specs_cols_to_keep)
        ) %>% 
        spread(X1, X2) %>% 
        clean_names()
    
    # final product dataframe
    final_product_tbl <- tibble(
        product_id, 
        product_name, 
        product_price, 
        product_year,
        product_image_url
    ) %>% 
        bind_cols(specs_tbl)
    
    return(final_product_tbl)
    
}


# ******************************************************************************
# SCRAPE DATA ----
# ******************************************************************************
# * Get Product URLs ----
tic()
plan("multisession")
product_url_tbl <- final_bikes_url_tbl %>% 
    mutate(product_urls = future_map(url, get_bike_urls))
toc()

# Unnest Product URLs Table ----
product_url_unest_tbl <- product_url_tbl %>% 
    unnest() %>% 
    mutate(product_id = gsub("[^0-9.-]", "", stri_sub(full_product_url, -6))) 

product_url_unest_tbl %>% View()


# * Scrape Data For Each Product URL ----


