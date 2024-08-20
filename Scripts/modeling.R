# General
library(tidyverse)
library(MLmetrics)
library(see)
library(performance)
library(parameters)
library(caret)
library(progress)
library(gtsummary)
library(kableExtra)

# Random Forest
library(ranger) 

# Interpretation
library(kernelshap)
library(shapviz)
library(pdp)
library(vip)

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

# Function to calculate evaluation metrics
calculate_metrics <- function(model, test_data) {
  predictions <- predict(model, newdata = test_data)
  actual <- log(test_data$price_sold)
  
  mse <- MSE(exp(predictions), test_data$price_sold)
  mape <- MAPE(exp(predictions), test_data$price_sold)
  r2 <- R2_Score(actual, predictions)
  r2_adj <- R2_adj_Score(actual, predictions, length(model$finalModel$coefficients) - 1)
  
  return(list(MSE = mse, MAPE = mape, R2 = r2, R2_adj = r2_adj))
}

# Function to select variables based on the chosen radius
select_variables_by_radius <- function(df, radii, include_lat_lon = FALSE, include_socio_demographic = TRUE) {
  base_vars <- c(
    "price_sold", "living_area", "energy_label", "room", "year_built", 
    "insulation", "heating"
  )
  
  if (include_lat_lon) {
    base_vars <- c(base_vars, "latitude", "longitude")
  }
  
  if (include_socio_demographic) {
    socio_demographic_vars <- c("income_low", "income_high", "edu_low", 
                                "edu_high", "schools_3km", "supermarket_km", "crimes", "net_LFP")
    base_vars <- c(base_vars, socio_demographic_vars)
  }
  
  radius_vars <- c()
  for (radius in radii) {
    radius_vars <- c(radius_vars,
      paste0("UGI_", radius), 
      paste0("people_", radius), 
      paste0("cars_", radius), 
      paste0("bicycles_", radius)
    )
  }
  
  selected_vars <- c(base_vars, radius_vars)
  df %>% select(all_of(selected_vars))
}

# Function for OLS modeling
ols_model <- function(df, radii, use_image_features = TRUE, include_lat_lon = FALSE, include_socio_demographic = TRUE, ctrl) {
  model_df <- select_variables_by_radius(df, radii, include_lat_lon, include_socio_demographic)
  if (!use_image_features) {
    model_df <- model_df %>% select(-contains(c("UGI", "people", "cars", "bicycles")))
  }
  
  model <- train(
    log(price_sold) ~ .,
    data = model_df,
    method = "lm",
    trControl = ctrl
  )
  
  return(model)
}

# Function for Random Forest modeling
rf_model <- function(df, radii, use_image_features = TRUE, include_lat_lon = FALSE, include_socio_demographic = TRUE, ctrl, rf_grid, num_trees) {
  model_df <- select_variables_by_radius(df, radii, include_lat_lon, include_socio_demographic)
  if (!use_image_features) {
    model_df <- model_df %>% select(-contains(c("UGI", "people", "cars", "bicycles")))
  }
  
  model <- train(
    log(price_sold) ~ .,
    data = model_df,
    method = "ranger",
    trControl = ctrl,
    tuneGrid = rf_grid,
    importance = 'impurity',
    num.trees = num_trees
  )
  
  return(model)
}

# Function to run models
run_models <- function(datasets, 
                       dataset_names = names(datasets),
                       radii = c("250m", "500m", "1000m"), 
                       multiple_radii = NULL,
                       model_types = c("OLS", "RF"),
                       use_image_features_options = c(TRUE, FALSE),
                       include_lat_lon_options = c(FALSE, TRUE),
                       include_socio_demographic_options = c(FALSE, TRUE),
                       train_proportion_options = c(0.7, 0.8),
                       seed_options = c(123, 456),
                       rf_grid = expand.grid(
                         mtry = c(3, 5, 7),
                         splitrule = c("variance", "extratrees"),
                         min.node.size = c(5, 10, 20)
                       ),
                       num_trees = 500,
                       cv_folds = 5) {
  
  results <- data.frame()
  
  # Calculate total iterations
  total_iterations <- length(dataset_names) * length(model_types) * 
    length(c(radii, list(multiple_radii))) * length(use_image_features_options) *
    length(include_lat_lon_options) * length(include_socio_demographic_options) *
    length(train_proportion_options) * length(seed_options)
  
  current_iteration <- 0

  for (dataset_name in dataset_names) {
    for (model_type in model_types) {
      for (radius in c(radii, list(multiple_radii))) {
        for (use_image_features in use_image_features_options) {
          for (include_lat_lon in include_lat_lon_options) {
            for (include_socio_demographic in include_socio_demographic_options) {
              for (train_proportion in train_proportion_options) {
                for (seed in seed_options) {
                  current_iteration <- current_iteration + 1
                  
                  # Print progress
                  cat(sprintf("\rProgress: %d/%d (%.1f%%)", 
                              current_iteration, 
                              total_iterations, 
                              100 * current_iteration / total_iterations))
                  flush.console()
                  
                  # Determine which radii to use
                  if (is.null(radius)) {
                    radii_to_use <- multiple_radii
                  } else {
                    radii_to_use <- radius
                  }
                  
                  # Skip iteration if no radii to use
                  if (length(radii_to_use) == 0) next
                  
                  # Split data
                  set.seed(seed)
                  train_indices <- createDataPartition(datasets[[dataset_name]]$price_sold, p = train_proportion, list = FALSE)
                  train_data <- datasets[[dataset_name]][train_indices, ]
                  test_data <- datasets[[dataset_name]][-train_indices, ]
                  
                  # Set up cross-validation
                  ctrl <- trainControl(method = "cv", number = cv_folds)
                  
                  # Train model
                  if (model_type == "OLS") {
                    model <- ols_model(train_data, radii_to_use, use_image_features, include_lat_lon, include_socio_demographic, ctrl)
                  } else {
                    model <- rf_model(train_data, radii_to_use, use_image_features, include_lat_lon, include_socio_demographic, ctrl, rf_grid, num_trees)
                  }
                  
                  # Calculate metrics
                  metrics <- calculate_metrics(model, test_data)
                  
                  # Store results
                  results <- rbind(results, data.frame(
                    Dataset = dataset_name,
                    Model = model_type,
                    Radius = paste(radii_to_use, collapse = ", "),
                    Image_Features = use_image_features,
                    Include_Lat_Lon = include_lat_lon,
                    Include_Socio_Demographic = include_socio_demographic,
                    Train_Proportion = train_proportion,
                    Seed = seed,
                    MSE = metrics$MSE,
                    MAPE = metrics$MAPE,
                    R2 = metrics$R2,
                    R2_adj = metrics$R2_adj
                  ))
                }
              }
            }
          }
        }
      }
    }
  }
  
  cat("\n")  # Move to next line after progress is complete
  
  return(results)
}




########################
### Data preparation ###
########################

# Load data
raw_df <- read_csv("./Data/final_data_for_modeling.csv")

# Create Data Frame for modeling
df <- raw_df %>% 
  # Select relevant columns
  select(-c(url, full_address, Area, building_type, date_sold, date_listed, 
            associated_panoids_100m, associated_panoids_250m, associated_panoids_500m, associated_panoids_1000m)) %>%
  # change all occurances of "Boiler" to "Central Heating" in the heating column
  mutate(heating = str_replace(heating, "Boiler", "Central Heating")) %>%
  # Convert categorical columns
  mutate(
    energy_label = factor(energy_label, levels = c("Higher", "Lower")),
    insulation = factor(insulation, levels = c("Double Glass", "Fully Insulated", "Other")),
    heating = factor(heating, levels = c("Central Heating", "District Heating", "Other")),
    house_type = factor(house_type)
  )

# Identify columns that are not 100m
cols_to_check <- names(df)[!grepl("_100m$", names(df))]

# Remove rows with NAs in these columns
df <- df %>% drop_na(all_of(cols_to_check))

# Create separate dataframes
datasets <- list(
  all = df,
  houses = df %>% filter(house_type == "huis"),
  apartments = df %>% filter(house_type == "appartement")
)

####################################
### Run Models and Store Results ###
####################################


# Run models
results <- run_models(
datasets = datasets,
  dataset_names = c("all"),  # c("all", "houses", "apartments")
  radii = c("1000m"), # c("250m", "500m", "1000m")
  multiple_radii = c("250m", "500m", "1000m"), # c("250m", "500m", "1000m")
  model_types = c("OLS", "RF"), # c("OLS", "RF")
  use_image_features_options = c(TRUE, FALSE), # c(TRUE, FALSE)
  include_lat_lon_options = c(FALSE, TRUE),
  include_socio_demographic_options = c(FALSE, TRUE),
  train_proportion_options = c(0.7, 0.8),
  seed_options = c(123, 456),
  rf_grid = expand.grid(
    mtry = c(3, 5), # c(3, 5, 7)
    splitrule = c("variance"), # c("variance", "extratrees")
    min.node.size = c(5, 10) # c(5, 10, 20)
  ),
  num_trees = c(500), # c(500, 1000)
  cv_folds = 2 # 5
)

# Display results
print(results)

# Optionally, save results to a CSV file
#write.csv(results, "model_comparison_results.csv", row.names = FALSE)



#####################################################
### Modeling specifically for Results Subsections ###
#####################################################


############################################################################################################################
############################# Part 1: Overview of OLS vs RF models for different features ##################################
############################################################################################################################

# Import from trained_models.RData
load("Data/trained_models.RData")

# Settings
radius <- "1000m"
use_adjusted_r2 <- F

# Create vectors to specify which cells should be bold
bold_cells_ols <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE)
bold_cells_rf <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)

# Initialize results dataframe
results <- data.frame(
  Model = character(),
  Feature = character(),
  R2 = numeric(),
  AdjR2 = numeric(),
  MSE = numeric(),
  stringsAsFactors = FALSE
)

for (model_name in names(model_performance)) {
  model_type <- ifelse(startsWith(model_name, "OLS"), "OLS", "RF")
  feature <- case_when(
    endsWith(model_name, "base") ~ "base",
    endsWith(model_name, "ugi_1000m") ~ "ugi",
    endsWith(model_name, "people_1000m") ~ "people",
    endsWith(model_name, "cars_1000m") ~ "cars",
    endsWith(model_name, "bicycles_1000m") ~ "bicycles",
    endsWith(model_name, "all_1000m") ~ "all",
    TRUE ~ "not of interest"
  )
  
  results <- rbind(results, data.frame(
    Model = model_type,
    Feature = feature,
    R2 = model_performance[[model_name]]$R2,
    AdjR2 = model_performance[[model_name]]$R2_adj,
    MSE = model_performance[[model_name]]$MSE
  ))

  # Delete results where feature is not of interest
  results <- results[!results$Feature == "not of interest", ]
}

# Calculate relative MSE
ols_baseline_mse <- results$MSE[results$Model == "OLS" & results$Feature == "base"]
results$RelativeMSE <- results$MSE / ols_baseline_mse

# Choose R2 or Adjusted R2 based on use_adjusted_r2
r2_col <- if(use_adjusted_r2) "AdjR2" else "R2"

# Reshape the dataframe
results_wide <- results %>%
  select(Model, Feature, !!r2_col, RelativeMSE) %>%
  pivot_wider(names_from = Feature, values_from = c(!!r2_col, RelativeMSE), names_sep = "_")

# Combine the bold vectors
bold_cells <- rbind(bold_cells_ols, bold_cells_rf)

# Create the table using kable and kableExtra
table_overview_ols_rf <- results_wide %>%
  select(Model,
    !!paste0(r2_col, "_base"), RelativeMSE_base,
    !!paste0(r2_col, "_ugi"), RelativeMSE_ugi,
    !!paste0(r2_col, "_people"), RelativeMSE_people,
    !!paste0(r2_col, "_cars"), RelativeMSE_cars,
    !!paste0(r2_col, "_bicycles"), RelativeMSE_bicycles,
    !!paste0(r2_col, "_all"), RelativeMSE_all) %>%
  kbl(format = "latex", booktabs = TRUE, digits = 3,
      col.names = c("Model", rep(c("R²", "MSE"), 6))) %>%
  kable_styling(latex_options = c("striped", "scale_down"), full_width = FALSE, font_size = 10) %>%
  add_header_above(c(" " = 1, "Baseline" = 2, "UGI" = 2, "People" = 2, "Cars" = 2, "Bicycles" = 2, "All" = 2)) %>%
  column_spec(1, bold = TRUE)

# Apply bold formatting to each column
for (i in 2:ncol(results_wide)) {
  table_overview_ols_rf <- table_overview_ols_rf %>%
    column_spec(i, bold = bold_cells[, i-1])
}

# Add footnote
table_overview_ols_rf <- table_overview_ols_rf %>%
  footnote(general = "MSE values are relative to the OLS Baseline, which is set to 1. Lower is better.",
           general_title = "Note:",
           threeparttable = F,
           footnote_as_chunk = F)

# Print the table
print(table_overview_ols_rf)

# Save the final plot object
save(table_overview_ols_rf, file = "table_overview_ols_rf.RData")

############################################################################################################################
####################### Part 2: OLS Coefficients Table for different House Types and Image Features Plot ###################
############################################################################################################################

# Assuming 'datasets' is already defined with 'all', 'apartments', and 'houses' data

# Define the formula
formula <- log(price_sold) ~ living_area + energy_label + room + year_built + insulation + heating + 
           UGI_1000m + people_1000m + cars_1000m + bicycles_1000m

# Fit OLS models
model_all <- lm(formula, data = datasets$all)
model_apartments <- lm(formula, data = datasets$apartments)
model_houses <- lm(formula, data = datasets$houses)

# Function to create formatted table
create_formatted_table <- function(model, label) {
  tbl_regression(model,
    estimate_fun = function (x) style_number(x, digits = 3),
    label = list(
      living_area = "Living Area (m²)",
      energy_label = "Energy Label",
      room = "Rooms",
      year_built = "Construction Year",
      insulation = "Insulation",
      heating = "Heating",
      UGI_1000m = "UGI",
      people_1000m = "People",
      cars_1000m = "Cars",
      bicycles_1000m = "Bicycles"
    ),
    intercept = TRUE,
    conf.int = TRUE
  ) %>%
  bold_labels() %>%
  italicize_levels() %>%
  add_glance_table(include = c(r.squared, adj.r.squared, p.value, nobs),
                   label = list(r.squared = "R²", adj.r.squared = "R² (adj.)", p.value = "p-value", nobs = "N")) %>%
  #add_glance_source_note(include = c(statistic, nobs, df, df.residual)) %>%
  add_significance_stars(
    pattern = "{estimate}{stars}",
    hide_ci = FALSE,
    hide_p = TRUE,
    hide_se = TRUE
  ) %>%
  add_vif(statistic = "aGVIF") %>%
  modify_header(
    label ~ "Variable",
    estimate ~ "Coefficient",
    ci ~ "95% CI "
  ) %>%
      modify_table_body(
        ~.x %>%
          mutate(
            ci = ifelse(is.na(ci), NA, sprintf("(%s)", ci))
          )
      ) %>%
      modify_table_body(
         ~.x %>%
          mutate(
            label = ifelse(label == "Higher", "Higher (A+, A, B, C)",
                         ifelse(label == "Lower", "Lower (D, E, F, G)", label))
              )
          )
}


# Create tables
table_all <- create_formatted_table(model_all, "All Data")
table_apartments <- create_formatted_table(model_apartments, "Apartments Only")
table_houses <- create_formatted_table(model_houses, "Houses Only")

# Merge tables
table_comparison_ols_house_types <- tbl_merge(
  tbls = list(table_all, table_apartments, table_houses),
  tab_spanner = c("**All Data**", "**Apartments Only**", "**Houses Only**")
)
# Display merged table
table_comparison_ols_house_types


# Save the final plot object
save(table_comparison_ols_house_types, file = "table_comparison_ols_house_types.RData")

# save table_all as well
table_ols_vif <- table_all
save(table_ols_vif, file = "table_ols_vif.RData")


##############################
### Plot of Image Features ###
##############################

# Create coefficient plot
plot_data <- parameters::compare_parameters(model_all, model_apartments, model_houses, 
                                standardize = "refit")


# Subset to include only the last four parameters
plot_data <- plot_data %>%
  filter(Parameter %in% c("UGI 1000m", "people 1000m", "cars 1000m", "bicycles 1000m"))

plot_data$Parameter <- c("UGI", "People", "Cars", "Bicycles")

# Create coefficient plot
coefficient_plot_image_features <- plot(plot_data, dodge_position = 0.4) +
  geom_errorbar(aes(xmin = CI_low, xmax = CI_high),
                width = 0.25, position = position_dodge(0.4)) +
  theme_bw() +
  theme(
    #panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 12)
  ) +
  labs(
    title = "Image Feature Coefficients Across House Types",
    x = "Coefficient Estimate",
    y = "Variable"
  ) +
  scale_color_manual(values = c("#008deb", "#f2a500", "#08bf48"),
  labels = c("All Data", "Apartments Only", "Houses Only"))
  #coord_flip()

# Display coefficient plot
print(coefficient_plot_image_features)

# Save the final plot object
save(coefficient_plot_image_features, file = "coefficient_plot_image_features.RData")

############################################################################################################################
######################################## Part 3: ##############################################################################
############################################################################################################################

# Import from trained_models.RData
load("Data/trained_models.RData")

# Define the models, radii, and metrics
models <- c("OLS", "RF")
radii <- c("250m", "500m", "1000m")
metrics <- c("MSE", "MAPE", "R2", "R2_adj")

# Define which metrics you want to include
which_metrics <- c("Test") # Change this as needed: c("Train"), c("Test"), or c("Train", "Test")
which_metrics_to_include <- c("MAPE", "R2") # Change this as needed

# Create a named vector for metric labels
metric_labels <- c(MSE = "MSE", MAPE = "MAPE", R2 = "R²", R2_adj = "R² (adj.)")

# Create an empty list to store the results
results_list <- list()

# Loop through models and radii
for (model in models) {
  for (radius in radii) {
    # Construct the object name
    obj_name <- paste0(model, "_all_", radius)
    
    # Extract metrics from the model_performance object
    metrics_values <- sapply(metrics, function(m) model_performance[[obj_name]][[m]])
    
    # Create a row for the results
    row <- c(Model = model, Radius = radius, metrics_values)
    results_list[[length(results_list) + 1]] <- row
  }
}

# Convert the list to a tibble
results_df <- tibble::as_tibble(do.call(rbind, results_list))

# Rename columns to match the original format
colnames(results_df) <- c("Model", "Radius", paste0("Test_", metrics))

# Convert Model and Radius columns to factors and the others to numeric
results_df$Model <- factor(results_df$Model)
results_df$Radius <- factor(results_df$Radius, levels = c("250m", "500m", "1000m"))
results_df[, -c(1:2)] <- sapply(results_df[, -c(1:2)], as.numeric)

# Reshape the data
table_data <- results_df %>%
  pivot_longer(
    cols = starts_with(which_metrics),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Train_Test = case_when(
      str_detect(Metric, "^Train") ~ "Train",
      str_detect(Metric, "^Test") ~ "Test",
      TRUE ~ NA_character_
    ),
    Metric = str_remove(Metric, "^(Train|Test)_")
  ) %>%
  filter(Metric %in% which_metrics_to_include) %>%
  mutate(
    Metric = factor(Metric, levels = which_metrics_to_include),
    Metric = recode(Metric, !!!metric_labels[which_metrics_to_include]),
    Train_Test = factor(Train_Test, levels = which_metrics)
  ) %>%
  arrange(Metric, Train_Test) %>%
  #unite("Metric", Train_Test, Metric, sep = " ") %>%
  pivot_wider(names_from = c(Model, Radius), values_from = Value) %>%
  # Get rid of the Train_test column
  select(-Train_Test)

# add bolding of cells
row_1 <- c(FALSE, FALSE, T, FALSE, FALSE, T)
row_2 <- c(FALSE, FALSE, T, FALSE, FALSE, T)
#row_3 <- c(FALSE, FALSE, T, FALSE, FALSE, T)
#row_4 <- c(FALSE, FALSE, T, FALSE, FALSE, T)
bold_cells <- rbind(row_1, row_2)#, row_3, row_4)

# Create the table 
table_comparison_radii <- table_data %>%
  kbl(format = "html", booktabs = TRUE, digits = 3,
      col.names = c("Metric", rep(radii, 2))) %>%
  kable_styling(latex_options = c("striped", "scale_down"), font_size = 10) %>%
  add_header_above(c(" " = 1, "OLS" = 3, "RF" = 3)) %>%
  column_spec(1, bold = TRUE) %>%
  {
    tbl <- .
    for (i in 2:ncol(table_data)) {
      tbl <- column_spec(tbl, i, bold = bold_cells[, i-1])
    }
    tbl
  }

print(table_comparison_radii)

# Save the final plot object
save(table_comparison_radii, file = "table_comparison_radii.RData")

############################################################################################################################
################################# Plot comparing coefficients of image features across radii ###############################
############################################################################################################################

# Define base variables
base_vars <- c("living_area", "energy_label", "room", "year_built", "insulation", "heating")

# Function to create formula for a specific radius
create_formula <- function(radius) {
  image_vars <- paste0(c("UGI", "people", "cars", "bicycles"), "_", radius)
  as.formula(paste("log(price_sold) ~", 
                   paste(c(base_vars, image_vars), collapse = " + ")))
}

# Fit OLS models for each radius
model_250m <- lm(create_formula("250m"), data = datasets$all)
model_500m <- lm(create_formula("500m"), data = datasets$all)
model_1000m <- lm(create_formula("1000m"), data = datasets$all)

# Create coefficient plot
plot_data <- compare_parameters(model_250m, model_500m, model_1000m,
                                standardize = "refit")

# Define the desired order for feature types and radii
feature_order <- c("UGI", "people", "cars", "bicycles")
radius_order <- c("250m", "500m", "1000m")

# Subset and reorder
plot_data <- plot_data %>%
  filter(Parameter %in% c("UGI 250m", "people 250m", "cars 250m", "bicycles 250m",
                          "UGI 500m", "people 500m", "cars 500m", "bicycles 500m",
                          "UGI 1000m", "people 1000m", "cars 1000m", "bicycles 1000m")) %>%
  mutate(
    feature_type = str_extract(Parameter, paste(feature_order, collapse = "|")),
    radius = str_extract(Parameter, paste(radius_order, collapse = "|"))
  ) %>%
  arrange(
    factor(feature_type, levels = feature_order),
    factor(radius, levels = radius_order)
  ) %>%
  select(-feature_type, -radius)

# Correct the labels for the plot
plot_data$Parameter <- rep(c("UGI", "People", "Cars", "Bicycles"), each = 3)



coefficient_plot_radii <- plot(plot_data, dodge_position = 0.5) +
  geom_errorbar(aes(xmin = CI_low, xmax = CI_high),
                width = 0.3, position = position_dodge(0.5)) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 12)
  ) +
  labs(
    title = "Image Feature Coefficients Across Different Radii",
    x = "Coefficient Estimate",
    y = "Variable",
    color = "Radius"
  ) +
  scale_color_manual(values = c("#008deb", "#f2a500", "#08bf48"),
                     labels = c("250m", "500m", "1000m"))

# Display coefficient plot
print(coefficient_plot_radii)

# Save the final plot object
save(coefficient_plot_radii, file = "coefficient_plot_radii.RData")


############################################################################################################################
############################################################################################################################
############################################################################################################################

####################
### Part 4: SHAP ###
####################

load("Data/shap_values.Rdata")

data <- datasets$all %>%
  # select relevant columns
  select(c(price_sold, living_area, energy_label, room, year_built, insulation, heating, 
           UGI_1000m, people_1000m, cars_1000m, bicycles_1000m))

# Select features for SHAP analysis
xvars <- colnames(data)[-1]

# Sampke rows to be explained
set.seed(123)
X <- data[sample(nrow(data), 500), xvars]

# Select background data
bg_X <- data[sample(nrow(data), 200), ]

# SHAP values
shap_values <- kernelshap(trained_models$RF_houses_1000m, X = X, bg_X = bg_X)

# Saev the SHAP values to a .Rdata file
save(shap_values, file = "Data/shap_values.Rdata")

#######################
### Plot using SHAP ###
#######################

# Create a custom theme
custom_theme <- theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    legend.text = element_text(size = 12)
  )

# Plot SHAP values using shapviz
sv <- shapviz(shap_values)
sv_importance(sv)
sv_importance(sv, kind = "beeswarm") + custom_theme

# interesting to color by people_1000m, ...

# Dependence plots with custom theme and titles
dependence_plots <- sv_dependence(sv, size = 2,
  c("UGI_1000m", "people_1000m", "cars_1000m", "bicycles_1000m"),
  interactions = FALSE, color_var = "living_area", color = "#0078e1") +
labs(title = "SHAP Dependence Plot", x = "Feature Value", y = "SHAP Value") +
geom_hline(yintercept = 0, linetype = "dashed", color = "#555555") +
custom_theme

  
dependence_plots & custom_theme 



############################################################################################################################
############################################################################################################################
############################################################################################################################

###############
### Var Imp ###
###############

# Create the base plot
var_imp <- vip(trained_models$RF_all_1000m, geom = "point", num_features = 20)  # Adjust num_features as needed

# Optionally, you can manually rename specific variables
variable_mapping <- c(
  "living_area" = "Living Area",
  "room" = "Rooms",
  "year_built" = "Construction Year",
  "energy_labelLower" = "Energy Label (Lower)",
  "insulationFully Insulated" = "Insulation (Fully Insulated)",
  "insulationOther" = "Insulation (Other)",
  "heatingDistrict Heating" = "Heating (District Heating)",
  "heatingOther" = "Heating (Other)",
  "UGI_1000m" = "Urban Greenery Index",
  "people_1000m" = "People",
  "cars_1000m" = "Cars",
  "bicycles_1000m" = "Bicycles"
)

var_imp$data <- var_imp$data %>%
  mutate(Variable = recode(Variable, !!!variable_mapping))

# Customize the plot
RF_vip_plot <- var_imp +
  theme_bw() +
  geom_segment(aes(x = Variable, xend = Variable, y = 0, yend = Importance),
               color = "#f2a500", size = 2.5) +  # Thin lines
  geom_point(aes(x = Variable, y = Importance), 
             color = "#f2a500", size = 4.5) +  # Round dots at the end
  coord_flip() +  # Flip coordinates for horizontal bars
  theme(
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  labs(
    title = "Variable Importance",
    y = "Importance (Impurity)"
  )

# Display the plot
print(RF_vip_plot)

# Save the final plot object
save(RF_vip_plot, file = "RF_vip_plot.RData")

############################################################################################################################
############################################################################################################################
############################################################################################################################

### PDP Plots ###

# use pdp::partial() to create partial dependence plots

pdp_vars_of_interest <- c("UGI_1000m", "people_1000m", "cars_1000m", "bicycles_1000m")

# Create the base plots
pdp_plot_ugi <- partial(trained_models$RF_all_1000m, pred.var = "UGI_1000m", plot = TRUE, plot.engine = "ggplot2")
pdp_plot_people <- partial(trained_models$RF_all_1000m, pred.var = "people_1000m", plot = TRUE, plot.engine = "ggplot2")
pdp_plot_cars <- partial(trained_models$RF_all_1000m, pred.var = "cars_1000m", plot = TRUE, plot.engine = "ggplot2")
pdp_plot_bicycles <- partial(trained_models$RF_all_1000m, pred.var = "bicycles_1000m", plot = TRUE, plot.engine = "ggplot2")

# Customize the plot
customized_pdp_plot <- pdp_plot_ugi +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 12)
  ) +
  labs(
    title = "Partial Dependence Plot",
    x = "Feature Value",
    y = "Predicted Price"
  )

# Display the plot
print(customized_pdp_plot)

############################################################################################################################
###### Grid of PDP and SHAP plots for all variables ######
############################################################################################################################

library(patchwork)

# Custom theme based on theme_bw
custom_theme <- theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(5, 5, 5, 5)
  )

# Function to create SHAP and PDP plots for a variable
create_plots <- function(rf_model, shap_values, variable, x_label) {
  # SHAP plot
  sv <- shapviz(shap_values)
  shap_plot <- sv_dependence(sv, variable, color = "#571356", color_var = "people_1000m") +
    labs(x = NULL, y = NULL) +
    custom_theme
  
  # PDP plot
  pdp_plot <- partial(rf_model, pred.var = variable, plot = TRUE, plot.engine = "ggplot2") +
    labs(x = x_label, y = NULL) +
    custom_theme
  
  return(list(shap = shap_plot, pdp = pdp_plot))
}

# Create plots for each variable
plots_ugi <- create_plots(trained_models$RF_all_1000m, shap_values, "UGI_1000m", "Urban Greenery Index")
plots_people <- create_plots(trained_models$RF_all_1000m, shap_values, "people_1000m", "People Count")
plots_cars <- create_plots(trained_models$RF_all_1000m, shap_values, "cars_1000m", "Car Count")
plots_bicycles <- create_plots(trained_models$RF_all_1000m, shap_values, "bicycles_1000m", "Bicycle Count")

# Arrange all plots in a grid
final_plot <- (
  plots_ugi$shap + plots_people$shap + plots_cars$shap + plots_bicycles$shap +
  plots_ugi$pdp + plots_people$pdp + plots_cars$pdp + plots_bicycles$pdp
) +
  plot_layout(ncol = 4, nrow = 2, widths = rep(1, 4), heights = rep(0.2, 2)) #+
  # plot_annotation(
  #   title = "Impact of Urban Environmental Factors on House Prices",
  #   theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  # )

# Add y-axis labels only to the leftmost plots
final_plot[[1]] <- final_plot[[1]] + ylab("SHAP Value")
final_plot[[5]] <- final_plot[[5]] + ylab("Predicted log(Price)")

# Remove x-axis labels from the top row
for(i in 1:4) {
  final_plot[[i]] <- final_plot[[i]] + theme(axis.title.x = element_blank())
}

# Display the final plot
print(final_plot)

# rename final_plot object
shap_pdp_grid_plot <- final_plot

# Save the final plot object
save(shap_pdp_grid_plot, file = "shap_pdp_grid_plot.RData")







#### supplementary figure X ####

sv_houses <- sv

supp_fig_u_shape_houses <- sv_dependence(sv, v = "year_built", color = "#571356", color_var = NULL, size = 2) +
  custom_theme + labs(x = "Construction Year", y = "SHAP Value")


save(supp_fig_u_shape_houses, file = "supp_fig_u_shape_houses.RData")



### Supplementary Figure: Distribution of numeric features of datasets$all ###

library(ggdist)

# Create a data frame with all numeric features
numeric_features <- datasets$all %>%
  select(where(is.numeric)) %>%
  # add the log price column
  mutate(log_price_sold = log(price_sold))
# Load necessary libraries
library(tidyverse)
library(ggdist)
library(ggridges)

# Select the first five numeric features and log_price_sold
numeric_features <- datasets$all %>%
  select(price_sold, living_area, room, year_built, income_low) %>%
  mutate(log_price_sold = log(price_sold))

# Create a long format data frame for ggplot
numeric_long <- numeric_features %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Create the distribution plot using ggdist
ggplot(numeric_long, aes(x = value, y = variable, fill = variable)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1) +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(title = "Distribution of Numeric Features", x = "Value", y = "Variable") +
  theme(legend.position = "none")


# Load necessary libraries
library(tidyverse)

# Assuming 'datasets$all' is already loaded and contains the data

# Select the first five numeric features and log_price_sold
numeric_features <- datasets$all %>%
  select(price_sold, living_area, room, year_built, income_low) %>%
  mutate(log_price_sold = log(price_sold))

# Create a long format data frame for ggplot
numeric_long <- numeric_features %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Create the facet-wrapped density plot
ggplot(numeric_long, aes(x = value)) +
  geom_density(fill = "skyblue", color = "black", alpha = 0.7) +
  facet_wrap(~ variable, scales = "free") +
  theme_bw() +
  labs(title = "Distribution of Selected Numeric Features", x = "Value", y = "Density") +
  theme(strip.text = element_text(size = 12), plot.title = element_text(hjust = 0.5))

