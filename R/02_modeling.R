# BIKE PRICING PREDICTION ----
# PRICE MODELING SCRIPT ----
# # **** ----

# ******************************************************************************
# SETUP ----
# ******************************************************************************


# Working Dir ----
setwd(here::here("R"))

# * Libraries ----
library(tidyverse)
library(janitor)
library(lubridate)
library(tidymodels)
library(xgboost)
library(rules)
library(tictoc)
library(future)
library(doFuture)
library(parallel)

# ******************************************************************************
# LOAD DATA ----
# ******************************************************************************
bikes_tbl <- read_rds("../data/trekbikes_clead_data.rds") 

bikes_tbl %>% glimpse()
bikes_tbl %>% sapply(function(x) sum(is.na(x)))

# ******************************************************************************
# SPLIT DATA ----
# ******************************************************************************
set.seed(123)
split_obj <- initial_split(bikes_tbl, prop = 0.80, strata = family)

train_tbl <- training(split_obj)
test_tbl  <- testing(split_obj)


# ******************************************************************************
# CROSS VALIDATION SPECS ----
# ******************************************************************************
set.seed(101)
resamples_obj <- vfold_cv(train_tbl, v = 10)


# ******************************************************************************
# RECIPE ----
# ******************************************************************************
recipe_spec <- recipe(product_price ~ ., data = train_tbl) %>% 
    step_rm(product_id, model_tier) %>% 
    step_novel(family, model_base, frame_material, category) %>% 
    step_dummy(model_base, category, family, frame_material, one_hot = TRUE)

recipe_spec %>% prep() %>% juice() %>% glimpse()

# ******************************************************************************
# BASELINE MODELING ----
# ******************************************************************************

# * GLMNET ----
wflw_fit_glmnet <- workflow() %>% 
    add_model(spec = linear_reg(penalty = 0.1, mixture = 0.5) %>% 
                  set_engine("glmnet")
    ) %>% 
    add_recipe(recipe_spec %>% step_normalize(weight)) %>% 
    fit(train_tbl)

# * SVM ----
wflw_fit_svm <- workflow() %>% 
    add_model(spec = svm_rbf(mode = "regression") %>% set_engine("kernlab")) %>% 
    add_recipe(recipe_spec %>% step_normalize(weight)) %>% 
    fit(train_tbl)

# * Random Forest ----
wflw_fit_rf <- workflow() %>% 
    add_model(spec = rand_forest(mode = "regression") %>% set_engine("ranger")) %>% 
    add_recipe(recipe_spec) %>% 
    fit(train_tbl)

# * Xgboost ----
wflw_fit_xgboost <- workflow() %>% 
    add_model(spec = boost_tree(mode = "regression") %>% set_engine("xgboost")) %>% 
    add_recipe(recipe_spec) %>% 
    fit(train_tbl)

# * MARS ----
wflw_fit_earth <- workflow() %>% 
    add_model(spec = mars(mode = "regression") %>% set_engine("earth")) %>% 
    add_recipe(recipe_spec) %>% 
    fit(train_tbl)

# * Cubist ----
wflw_fit_cubist <- workflow() %>% 
    add_model(spec = cubist_rules(mode = "regression") %>% set_engine("Cubist")) %>% 
    add_recipe(recipe_spec) %>% 
    fit(train_tbl)


# Baseline Model Results Evaluation ----

# * Function to Get Baseline Model Metrics ----
get_baseline_model_metrics <- function(model, new_data, model_name){
    
    pred_tbl <- predict(model, new_data = new_data) %>% 
        bind_cols(new_data %>% select(product_price)) 
    
    metrics <- metric_set(mae, rmse, rsq)
    
    metrics_tbl <- metrics(pred_tbl, truth = product_price, estimate = .pred) %>% 
        select(.metric, .estimate) %>% 
        spread(.metric, .estimate) %>% 
        mutate(model = model_name, .before = mae)
    
    return(metrics_tbl)
    
}

# * Baseline Model Metrics Table ----
baseline_model_metrics_tbl <- bind_rows(
    get_baseline_model_metrics(wflw_fit_glmnet, test_tbl, "GLMNET"),
    get_baseline_model_metrics(wflw_fit_svm, test_tbl, "SVM"),
    get_baseline_model_metrics(wflw_fit_xgboost, test_tbl, "XGBOOST"),
    get_baseline_model_metrics(wflw_fit_rf, test_tbl, "RANDOM FOREST"),
    get_baseline_model_metrics(wflw_fit_earth, test_tbl, "MARS"),
    get_baseline_model_metrics(wflw_fit_cubist, test_tbl, "CUBIST")
) %>% 
    arrange(rmse)

# - I decided to tune the top 4 models, meaning the models with lowest rmse
# - These models are xgboost, random forest, glmnet and mars


# ******************************************************************************
# HYPER - PARAMETER TUNING ----
# ******************************************************************************

# * XGBOOST TUNE ----
# ** Spec ----
model_spec_xgboost_tune <- boost_tree(
    mode           = "regression",
    mtry           = tune(),
    trees          = tune(),
    min_n          = tune(),
    tree_depth     = tune(),
    learn_rate     = tune(),
    loss_reduction = tune()
) %>% 
    set_engine("xgboost")

# - check "mtry" range
workflow() %>% 
    add_model(model_spec_xgboost_tune) %>% 
    add_recipe(recipe_spec) %>% 
    parameters() %>% 
    finalize(train_tbl) %>% 
    hardhat::extract_parameter_dials("mtry")

# ** Workflow ----
wflw_spec_xgboost_tune <- workflow() %>% 
    add_model(model_spec_xgboost_tune) %>% 
    add_recipe(recipe_spec)

# ** Tuning ----
tic()
set.seed(123)
tune_results_xgboost <- tune_grid(
    object    = wflw_spec_xgboost_tune,
    resamples = resamples_obj,
    grid      = grid_latin_hypercube(parameters(model_spec_xgboost_tune) %>% 
                                         update(mtry = mtry(range = c(1, 20))),
                                     size = 10),
    control   = control_grid(save_pred = TRUE, verbose = FALSE, allow_par = TRUE),
    metrics   = metric_set(mae, rmse, rsq)
)
toc()

# ** Results ----
tune_results_xgboost %>% show_best("rmse", n = 5)

# ** Finalize Fit ----
wflw_fit_xgboost_tuned <- wflw_spec_xgboost_tune %>% 
    finalize_workflow(select_best(tune_results_xgboost, "rmse")) %>% 
    fit(bikes_tbl)


# * RANDOM FOREST TUNE ----
# ** Spec ----
model_spec_ranger_tune <- rand_forest(
    mode           = "regression",
    mtry           = tune(),
    trees          = tune(),
    min_n          = tune()
) %>% 
    set_engine("ranger")

# - check "mtry" range
workflow() %>% 
    add_model(model_spec_ranger_tune) %>% 
    add_recipe(recipe_spec) %>% 
    parameters() %>% 
    finalize(train_tbl) %>% 
    hardhat::extract_parameter_dials("mtry")

# ** Workflow ----
wflw_spec_rf_tune <- workflow() %>% 
    add_model(model_spec_ranger_tune) %>% 
    add_recipe(recipe_spec)

# ** Tuning ----
tic()
set.seed(123)
tune_results_rf <- tune_grid(
    object    = wflw_spec_rf_tune,
    resamples = resamples_obj,
    grid      = grid_latin_hypercube(parameters(model_spec_ranger_tune) %>% 
                                         update(mtry = mtry(range = c(1, 20))),
                                     size = 10),
    control   = control_grid(save_pred = TRUE, verbose = FALSE, allow_par = TRUE),
    metrics   = metric_set(mae, rmse, rsq)
)
toc()

# ** Results ----
tune_results_rf %>% show_best("rmse", n = 5)

# ** Finalize ----
wflw_fit_rf_tuned <- wflw_spec_rf_tune %>% 
    finalize_workflow(select_best(tune_results_rf, "rmse")) %>% 
    fit(bikes_tbl)


# * GLMNET TUNE ----
# ** Spec ----
model_spec_glmnet_tune <- linear_reg(
    mode    = "regression",
    mixture = tune(),
    penalty = tune()
) %>% 
    set_engine("glmnet")

# ** Workflow ----
wflw_spec_glmnet_tune <- workflow() %>% 
    add_model(model_spec_glmnet_tune) %>% 
    add_recipe(recipe_spec)

# ** Tuning ----
tic()
set.seed(123)
tune_results_glmnet <- tune_grid(
    object    = wflw_spec_glmnet_tune,
    resamples = resamples_obj,
    grid      = grid_latin_hypercube(parameters(model_spec_glmnet_tune),
                                     size = 10),
    control   = control_grid(save_pred = TRUE, verbose = FALSE, allow_par = TRUE),
    metrics   = metric_set(mae, rmse, rsq)
)
toc()

# ** Results ----
tune_results_glmnet %>% show_best("rmse", n = 5)

# ** Finalize ----
wflw_fit_glmnet_tuned <- wflw_spec_glmnet_tune %>% 
    finalize_workflow(select_best(tune_results_glmnet, "rmse")) %>% 
    fit(bikes_tbl)


# * EARTH TUNE ----
# ** Spec ----
model_spec_earth_tuned <- mars(
    mode        = "regression",
    num_terms   = tune(),
    prod_degree =  tune()
) %>% 
    set_engine("earth")

# ** Workflow ----
wflw_spec_earth_tune <- workflow() %>% 
    add_model(model_spec_earth_tuned) %>% 
    add_recipe(recipe_spec)

# ** Tuning ----
tic()
set.seed(123)
tune_results_earth <- tune_grid(
    object    = wflw_spec_earth_tune,
    resamples = resamples_obj,
    grid      = grid_latin_hypercube(parameters(model_spec_earth_tuned),
                                     size = 10),
    control   = control_grid(save_pred = TRUE, verbose = FALSE, allow_par = TRUE),
    metrics   = metric_set(mae, rmse, rsq)
)
toc()

# ** Results ----
tune_results_earth %>% show_best("rmse", n = 5)

# ** Finalize ----
wflw_fit_earth_tuned <- wflw_spec_earth_tune %>% 
    finalize_workflow(select_best(tune_results_earth, "rmse")) %>% 
    fit(bikes_tbl)


# ******************************************************************************
# SAVING ----
# ******************************************************************************
model_artifacts <- list(
    xgboost       = wflw_fit_xgboost_tuned,
    random_forest = wflw_fit_rf_tuned,
    glmnet        = wflw_fit_glmnet_tuned,
    earth         = wflw_fit_earth_tuned
)

model_artifacts %>% write_rds("../artifacts/model_artifacts.rds")
