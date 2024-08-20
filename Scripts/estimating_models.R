library(tidyverse)
library(caret)
library(ranger)
library(MLmetrics)
library(pbapply)

#################
### Functions ###
#################

# R2 and R2_adj functions
R2_Score <- function(actual, predicted) {
  1 - var(actual - predicted) / var(actual)
}

R2_adj_Score <- function(actual, predicted, n_predictors) {
  n <- length(actual)
  r2 <- 1 - var(actual - predicted) / var(actual)
  
  # Calculate adjusted R-squared
  adjusted_r2 <- 1 - (1 - r2) * ((n - 1) / (n - n_predictors - 1))
  
  return(adjusted_r2)
}

# 3. Create Helper Functions
train_ols <- function(formula, data) {
  train(formula, data = data, method = "lm", trControl = trainControl(method = "cv", number = 5))
}

train_rf <- function(formula, data, number_of_features) {
  rf_grid <- expand.grid(
    mtry = floor(number_of_features * c(.15, .25, .333, .4)),
    splitrule = c("variance"),
    min.node.size = c(3, 5, 10)
  )
  train(formula, data = data, method = "ranger",
        trControl = trainControl(method = "cv", number = 5),
        tuneGrid = rf_grid, num.trees = 500, importance = 'impurity')
}

evaluate_model <- function(model, test_data) {
  predictions <- predict(model, newdata = test_data)
  actual <- log(test_data$price_sold)
  
  mse <- MSE(exp(predictions), test_data$price_sold)
  mape <- MAPE(exp(predictions), test_data$price_sold)
  r2 <- R2_Score(actual, predictions)
  r2_adj <- R2_adj_Score(actual, predictions, length(model$finalModel$coefficients) - 1)
  
  list(MSE = mse, MAPE = mape, R2 = r2, R2_adj = r2_adj)
}

########################
### Data preparation ###
########################

# Load data
raw_df <- read_csv("./Data/final_data_for_modeling.csv")

# Create Data Frame for modeling
df <- raw_df %>% 
  # Select relevant columns
  select(-c(url, full_address, 
            #Area, 
            building_type, date_sold, date_listed, 
            associated_panoids_100m, associated_panoids_250m, associated_panoids_500m, associated_panoids_1000m)) %>%
  # Convert categorical columns
  mutate(
    energy_label = factor(energy_label, levels = c("Higher", "Lower")),
    insulation = factor(insulation, levels = c("Double Glass", "Fully Insulated", "Other")),
    heating = factor(heating, levels = c("Boiler", "District Heating", "Other")),
    house_type = factor(house_type),
    Area = factor(Area)
  ) %>%
  # Remove columns ending in _100m
  select(-ends_with("_100m")) %>%
  # Remove rows with NAs
  drop_na(
)

# Create separate dataframes
datasets <- list(
  all = df,
  houses = df %>% filter(house_type == "huis"),
  apartments = df %>% filter(house_type == "appartement")
)

# 1. Data Preparation
set.seed(123)

# Function to create train-test split
create_train_test <- function(data, p = 0.8) {
  data <- data %>% na.omit()
  train_indices <- createDataPartition(data$price_sold, p = p, list = FALSE)
  list(
    train = data[train_indices, ],
    test = data[-train_indices, ]
  )
}

# Create train-test splits for each dataset
datasets_split <- list(
  all = create_train_test(datasets$all),
  apartments = create_train_test(datasets$apartments),
  houses = create_train_test(datasets$houses)
)

# 2. Define Model Specifications
base_vars <- c("living_area", "energy_label", "room", "year_built", "insulation", "heating")
image_vars <- c("UGI_1000m", "people_1000m", "cars_1000m", "bicycles_1000m")

model_specs <- list(
  list(name = "OLS_base", type = "OLS", vars = base_vars, data = "all"),
  list(name = "OLS_ugi_1000m", type = "OLS", vars = c(base_vars, "UGI_1000m"), data = "all"),
  list(name = "OLS_people_1000m", type = "OLS", vars = c(base_vars, "people_1000m"), data = "all"),
  list(name = "OLS_cars_1000m", type = "OLS", vars = c(base_vars, "cars_1000m"), data = "all"),
  list(name = "OLS_bicycles_1000m", type = "OLS", vars = c(base_vars, "bicycles_1000m"), data = "all"),
  list(name = "OLS_all_250m", type = "OLS", vars = c(base_vars, gsub("1000m", "250m", image_vars)), data = "all"),
  list(name = "OLS_all_500m", type = "OLS", vars = c(base_vars, gsub("1000m", "500m", image_vars)), data = "all"),
  list(name = "OLS_all_1000m", type = "OLS", vars = c(base_vars, image_vars), data = "all"),
  list(name = "OLS_apartments_1000m", type = "OLS", vars = c(base_vars, image_vars), data = "apartments"),
  list(name = "OLS_houses_1000m", type = "OLS", vars = c(base_vars, image_vars), data = "houses"),
  list(name = "RF_base", type = "RF", vars = base_vars, data = "all"),
  list(name = "RF_ugi_1000m", type = "RF", vars = c(base_vars, "UGI_1000m"), data = "all"),
  list(name = "RF_people_1000m", type = "RF", vars = c(base_vars, "people_1000m"), data = "all"),
  list(name = "RF_cars_1000m", type = "RF", vars = c(base_vars, "cars_1000m"), data = "all"),
  list(name = "RF_bicycles_1000m", type = "RF", vars = c(base_vars, "bicycles_1000m"), data = "all"),
  list(name = "RF_all_250m", type = "RF", vars = c(base_vars, gsub("1000m", "250m", image_vars)), data = "all"),
  list(name = "RF_all_500m", type = "RF", vars = c(base_vars, gsub("1000m", "500m", image_vars)), data = "all"),
  list(name = "RF_all_1000m", type = "RF", vars = c(base_vars, image_vars), data = "all"),
  list(name = "RF_apartments_1000m", type = "RF", vars = c(base_vars, image_vars), data = "apartments"),
  list(name = "RF_houses_1000m", type = "RF", vars = c(base_vars, image_vars), data = "houses")
)


# 4. Model Training Loop
model_results <- pblapply(model_specs, function(spec) {
  tryCatch({
    formula <- as.formula(paste("log(price_sold) ~", paste(spec$vars, collapse = " + ")))
    
    train_data <- datasets_split[[spec$data]]$train
    test_data <- datasets_split[[spec$data]]$test
    
    if (spec$type == "OLS") {
      model <- train_ols(formula, train_data)
    } else {
      model <- train_rf(formula, train_data, length(spec$vars))
    }
    
    performance <- evaluate_model(model, test_data)
    
    list(model = model, performance = performance)
  }, error = function(e) {
    warning(paste("Error in model", spec$name, ":", e$message))
    list(model = NULL, performance = NULL)
  })
})

# Extract trained models and performance metrics
trained_models <- setNames(
  lapply(model_results, function(x) x$model),
  sapply(model_specs, function(x) x$name)
)

model_performance <- setNames(
  lapply(model_results, function(x) x$performance),
  sapply(model_specs, function(x) x$name)
)


# 5. Save Results
save(trained_models, model_performance, file = "Data/trained_models.RData") 




########################################################################
####### Estimate Models again, now with the Area column included ####### 
########################################################################

# 1. Data Preparation
set.seed(123)

# Function to create train-test split
create_train_test <- function(data, p = 0.8) {
  data <- data %>% na.omit()
  train_indices <- createDataPartition(data$price_sold, p = p, list = FALSE)
  list(
    train = data[train_indices, ],
    test = data[-train_indices, ]
  )
}

# Create train-test splits for each dataset
datasets_split <- list(
  all = create_train_test(datasets$all),
  apartments = create_train_test(datasets$apartments),
  houses = create_train_test(datasets$houses)
)

# 2. Define Model Specifications
base_vars <- c("living_area", "energy_label", "room", "year_built", "insulation", "heating", "Area")
image_vars <- c("UGI_1000m", "people_1000m", "cars_1000m", "bicycles_1000m")

model_specs <- list(
  list(name = "OLS_base", type = "OLS", vars = base_vars, data = "all"),
  list(name = "OLS_ugi_1000m", type = "OLS", vars = c(base_vars, "UGI_1000m"), data = "all"),
  list(name = "OLS_people_1000m", type = "OLS", vars = c(base_vars, "people_1000m"), data = "all"),
  list(name = "OLS_cars_1000m", type = "OLS", vars = c(base_vars, "cars_1000m"), data = "all"),
  list(name = "OLS_bicycles_1000m", type = "OLS", vars = c(base_vars, "bicycles_1000m"), data = "all"),
  list(name = "OLS_all_250m", type = "OLS", vars = c(base_vars, gsub("1000m", "250m", image_vars)), data = "all"),
  list(name = "OLS_all_500m", type = "OLS", vars = c(base_vars, gsub("1000m", "500m", image_vars)), data = "all"),
  list(name = "OLS_all_1000m", type = "OLS", vars = c(base_vars, image_vars), data = "all"),
  list(name = "OLS_apartments_1000m", type = "OLS", vars = c(base_vars, image_vars), data = "apartments"),
  list(name = "OLS_houses_1000m", type = "OLS", vars = c(base_vars, image_vars), data = "houses"),
  list(name = "RF_base", type = "RF", vars = base_vars, data = "all"),
  list(name = "RF_ugi_1000m", type = "RF", vars = c(base_vars, "UGI_1000m"), data = "all"),
  list(name = "RF_people_1000m", type = "RF", vars = c(base_vars, "people_1000m"), data = "all"),
  list(name = "RF_cars_1000m", type = "RF", vars = c(base_vars, "cars_1000m"), data = "all"),
  list(name = "RF_bicycles_1000m", type = "RF", vars = c(base_vars, "bicycles_1000m"), data = "all"),
  list(name = "RF_all_250m", type = "RF", vars = c(base_vars, gsub("1000m", "250m", image_vars)), data = "all"),
  list(name = "RF_all_500m", type = "RF", vars = c(base_vars, gsub("1000m", "500m", image_vars)), data = "all"),
  list(name = "RF_all_1000m", type = "RF", vars = c(base_vars, image_vars), data = "all"),
  list(name = "RF_apartments_1000m", type = "RF", vars = c(base_vars, image_vars), data = "apartments"),
  list(name = "RF_houses_1000m", type = "RF", vars = c(base_vars, image_vars), data = "houses")
)


# 4. Model Training Loop
model_results <- pblapply(model_specs, function(spec) {
  tryCatch({
    formula <- as.formula(paste("log(price_sold) ~", paste(spec$vars, collapse = " + ")))
    
    train_data <- datasets_split[[spec$data]]$train
    test_data <- datasets_split[[spec$data]]$test
    
    if (spec$type == "OLS") {
      model <- train_ols(formula, train_data)
    } else {
      model <- train_rf(formula, train_data, length(spec$vars))
    }
    
    performance <- evaluate_model(model, test_data)
    
    list(model = model, performance = performance)
  }, error = function(e) {
    warning(paste("Error in model", spec$name, ":", e$message))
    list(model = NULL, performance = NULL)
  })
})

# Extract trained models and performance metrics
trained_models_with_area <- setNames(
  lapply(model_results, function(x) x$model),
  sapply(model_specs, function(x) x$name)
)

model_performance_with_area <- setNames(
  lapply(model_results, function(x) x$performance),
  sapply(model_specs, function(x) x$name)
)


# 5. Save Results
save(trained_models_with_area, model_performance_with_area, file = "Data/trained_models_with_area.RData")



###############################################################
####### Estimate Models again, now just within one Area #######  
###############################################################

# take only subset of df, where area is equal to Noord
df_one_area <- df %>% filter(Area == "Noord")

datasets <- list(
  all = df_one_area,
  houses = df_one_area %>% filter(house_type == "huis"),
  apartments = df_one_area %>% filter(house_type == "appartement")
)

# 1. Data Preparation
set.seed(123)

# Function to create train-test split
create_train_test <- function(data, p = 0.8) {
  data <- data %>% na.omit()
  train_indices <- createDataPartition(data$price_sold, p = p, list = FALSE)
  list(
    train = data[train_indices, ],
    test = data[-train_indices, ]
  )
}

# Create train-test splits for each dataset
datasets_split <- list(
  all = create_train_test(datasets$all),
  apartments = create_train_test(datasets$apartments),
  houses = create_train_test(datasets$houses)
)

# 2. Define Model Specifications
base_vars <- c("living_area", "energy_label", "room", "year_built", "insulation", "heating")
image_vars <- c("UGI_1000m", "people_1000m", "cars_1000m", "bicycles_1000m")

model_specs <- list(
  list(name = "OLS_base", type = "OLS", vars = base_vars, data = "all"),
  list(name = "OLS_ugi_1000m", type = "OLS", vars = c(base_vars, "UGI_1000m"), data = "all"),
  list(name = "OLS_people_1000m", type = "OLS", vars = c(base_vars, "people_1000m"), data = "all"),
  list(name = "OLS_cars_1000m", type = "OLS", vars = c(base_vars, "cars_1000m"), data = "all"),
  list(name = "OLS_bicycles_1000m", type = "OLS", vars = c(base_vars, "bicycles_1000m"), data = "all"),
  list(name = "OLS_all_250m", type = "OLS", vars = c(base_vars, gsub("1000m", "250m", image_vars)), data = "all"),
  list(name = "OLS_all_500m", type = "OLS", vars = c(base_vars, gsub("1000m", "500m", image_vars)), data = "all"),
  list(name = "OLS_all_1000m", type = "OLS", vars = c(base_vars, image_vars), data = "all"),
  list(name = "OLS_apartments_1000m", type = "OLS", vars = c(base_vars, image_vars), data = "apartments"),
  list(name = "OLS_houses_1000m", type = "OLS", vars = c(base_vars, image_vars), data = "houses"),
  list(name = "RF_base", type = "RF", vars = base_vars, data = "all"),
  list(name = "RF_ugi_1000m", type = "RF", vars = c(base_vars, "UGI_1000m"), data = "all"),
  list(name = "RF_people_1000m", type = "RF", vars = c(base_vars, "people_1000m"), data = "all"),
  list(name = "RF_cars_1000m", type = "RF", vars = c(base_vars, "cars_1000m"), data = "all"),
  list(name = "RF_bicycles_1000m", type = "RF", vars = c(base_vars, "bicycles_1000m"), data = "all"),
  list(name = "RF_all_250m", type = "RF", vars = c(base_vars, gsub("1000m", "250m", image_vars)), data = "all"),
  list(name = "RF_all_500m", type = "RF", vars = c(base_vars, gsub("1000m", "500m", image_vars)), data = "all"),
  list(name = "RF_all_1000m", type = "RF", vars = c(base_vars, image_vars), data = "all"),
  list(name = "RF_apartments_1000m", type = "RF", vars = c(base_vars, image_vars), data = "apartments"),
  list(name = "RF_houses_1000m", type = "RF", vars = c(base_vars, image_vars), data = "houses")
)


# 4. Model Training Loop
model_results <- pblapply(model_specs, function(spec) {
  tryCatch({
    formula <- as.formula(paste("log(price_sold) ~", paste(spec$vars, collapse = " + ")))
    
    train_data <- datasets_split[[spec$data]]$train
    test_data <- datasets_split[[spec$data]]$test
    
    if (spec$type == "OLS") {
      model <- train_ols(formula, train_data)
    } else {
      model <- train_rf(formula, train_data, length(spec$vars))
    }
    
    performance <- evaluate_model(model, test_data)
    
    list(model = model, performance = performance)
  }, error = function(e) {
    warning(paste("Error in model", spec$name, ":", e$message))
    list(model = NULL, performance = NULL)
  })
})

# Extract trained models and performance metrics
trained_models_only_one_area <- setNames(
  lapply(model_results, function(x) x$model),
  sapply(model_specs, function(x) x$name)
)

model_performance_only_one_area <- setNames(
  lapply(model_results, function(x) x$performance),
  sapply(model_specs, function(x) x$name)
)

# 6. Save Results
save(trained_models_only_one_area, model_performance_only_one_area, file = "Data/trained_models_only_one_area.RData")
