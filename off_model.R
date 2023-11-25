library(dials)
library(tidyverse)
library(tidymodels)
library(finetune) 
library(SHAPforxgboost)

fb <- c("Sinker", "Fastball", "Four-Seam", "FourSeamFastball", 
        "TwoSeamFastball", "OneSeamFastball", "Cutter")
bb <- c("Slider", "Curveball")
off <- c("Changeup", "Splitter")

ml_data <- na.omit(ml_data)

off_2022 <- ml_data %>% 
  filter(tagged_pitch_type %in% off,
         year(pitch_date) == 2022) %>% 
  select(-c(tagged_pitch_type, pitch_date, player_name, team_name))

off_2023 <- ml_data %>% 
  filter(tagged_pitch_type %in% off,
         year(pitch_date) == 2023) %>% 
  select(-pitch_date)

off_2023_test <- off_2023 %>% 
  select(-c(player_name, team_name, tagged_pitch_type))

# Define the recipe for preprocessing the data for the model
recipe_spec <- recipe(pitch_rv ~ ., data = off_2022)

# Define the model specification with hyperparameters to tune using XGBoost
xgb_spec <- boost_tree(
  mode = "regression",
  engine = "xgboost"
) %>% 
  set_engine("xgboost") %>%
  set_args(trees = tune(), 
           tree_depth = tune(), 
           learn_rate = tune(), 
           loss_reduction = tune(), 
           sample_size = tune(), 
           min_n = tune())

# Extract the parameter set to be tuned from the model specification
params <- extract_parameter_set_dials(xgb_spec)

# Generate a random grid of hyperparameter values to sample from
grid_vals <- grid_random(params, size = 200)
grid_vals <- grid_vals[1:50, ]

# Perform hyperparameter tuning using racing methods with ANOVA to prune poorly 
# performing candidates
racing_results <- tune_race_anova(
  xgb_spec,
  recipe_spec,
  resamples = bootstraps(off_2022, times = 5),
  grid = grid_vals
)

best_params <- select_best(racing_results, "rmse")

# Finalize the model with the selected best hyperparameters
final_model <- finalize_model(xgb_spec, best_params)

# Fit the final model to the oversampled training data using a workflow
fit_final_model <- workflow() %>%
  add_recipe(recipe_spec) %>%
  add_model(final_model) %>%
  fit(off_2022)

# Make predictions on the original, non-oversampled test data
predictions <- fit_final_model %>%
  predict(off_2023_test %>% select(-pitch_rv)) %>%
  bind_cols(off_2023)

xgb_booster <- fit_final_model$fit$fit$fit

xgb.save(xgb_booster, "xgb_off_model.model")

write_csv(predictions, 'off_preds.csv')

racing_metrics <- racing_results %>% 
  collect_metrics() %>% 
  filter(.metric %in% c("rmse", "mae"))

print(racing_metrics)



final_model <- pull_workflow_fit(fit_final_model)$fit

# # Convert training data to a matrix format for SHAP value computation
X_train_matrix <- as.matrix(off_2022 %>% select(-pitch_rv))

# # Compute SHAP values for the model interpretation
shap_values <- shap.values(xgb_model = final_model, X_train = X_train_matrix)

# # Prepare the SHAP values for plotting
shap_long <- shap.prep(xgb_model = final_model, X_train = X_train_matrix)

# # Generate and print the SHAP summary plot for visual interpretation of
# feature importance
shap_summary_plot <- shap.plot.summary(shap_long)
print(shap_summary_plot)
