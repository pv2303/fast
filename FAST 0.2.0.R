library(tidyverse)
library(fpp3)

# Function to fit univariate models
fit_univariate_models <- function(data, response) {
  
  library(fpp3)
  
  # Fit all univariate models simultaneously
  fitted_models <- data %>%
    model(
      ARIMA = ARIMA(!!sym(response)),
      ETS = ETS(!!sym(response)),
      SNAIVE = SNAIVE(!!sym(response)),
      RW = RW(!!sym(response)),
      RW = RW(!!sym(response) ~ drift()),
      MEAN = MEAN(!!sym(response)),
      NNETAR = NNETAR(!!sym(response)),
      THETA = THETA(!!sym(response))
    )
  
  # Return the fitted models and their forecasts
  return(list(models = fitted_models))
}

# Function to fit models with all combinations of covariates
fit_covariate_models <- function(data, response, covariates, h = "12 months") {
  
  library(fpp3)
  
  # Ensure covariates exist in the data
  if (!all(covariates %in% names(data))) {
    stop("Not all covariates exist in the data.")
  }
  
  # Generate all possible combinations of covariates
  covariate_combinations <- list()
  for (i in 1:length(covariates)) {
    covariate_combinations <- c(covariate_combinations, combn(covariates, i, simplify = FALSE))
  }
  
  # List to store models and results
  all_fitted_models <- list()
  
  # Fit models for each covariate combination
  for (covariate_set in covariate_combinations) {
    
    # Build the formula for ARIMA and TSLM using reformulate
    arima_formula <- reformulate(termlabels = covariate_set, response = response)
    tslm_formula <- reformulate(termlabels = covariate_set, response = response)
    
    # Fit models with the current set of covariates
    fitted_models <- data %>%
      model(
        ARIMA = ARIMA(arima_formula),
        TSLM = TSLM(tslm_formula)
      )
    
    # Store the fitted models in the list with the combination as the key
    combination_name <- paste(covariate_set, collapse = "_")
    all_fitted_models[[combination_name]] <- list(models = fitted_models)
  }
  
  # Return the list of all fitted models
  return(all_fitted_models)
}


# Function to select the best model based on MAPE after Ljung-Box test
select_best_model <- function(data, response, covariates = NULL, h = 12) {
  
  # Split data into training and test sets
  train_data <- data %>%
    slice(1:(n() - h))
  
  test_data <- data %>%
    slice((n() - h + 1):n())
  
  # Fit models on training data
  if (is.null(covariates)) {
    # Fit univariate models
    fitted_models <- fit_univariate_models(data = train_data, response = response)$models
  } else {
    # Fit models with covariates
    covariate_results <- fit_covariate_models(data = train_data, response = response, covariates = covariates)
    
    # Combine all models into one mable
    fitted_models <- map_dfr(covariate_results, ~ .x$models, .id = "combination")
  }
  
  # Perform Ljung-Box test on residuals for lags 1 to 10
  models_passed <- fitted_models %>%
    mutate(
      residuals = map(.model, ~ residuals(.x, type = "innovation")),
      ljung_box = map(residuals, ~ {
        map_dbl(1:10, ~ Ljung_Box_Test(.x, lag = .x)$p.value)
      }),
      passes_test = map_lgl(ljung_box, ~ all(.x > 0.05))
    ) %>%
    filter(passes_test)
  
  # Check if any models passed the Ljung-Box test
  if (nrow(models_passed) == 0) {
    stop("No models passed the Ljung-Box test.")
  }
  
  # Forecast on test data and calculate MAPE
  models_with_forecasts <- models_passed %>%
    mutate(
      forecast = map(.model, ~ forecast(.x, new_data = test_data)),
      accuracy = map(forecast, ~ accuracy(.x, test_data)),
      mape = map_dbl(accuracy, ~ .x %>% filter(.type == "Test") %>% pull(MAPE))
    )
  
  # Select the best model
  best_model_row <- models_with_forecasts %>%
    filter(mape == min(mape)) %>%
    slice(1)
  
  best_model <- best_model_row$.model[[1]]
  best_model_name <- ifelse(is.null(covariates),
                            names(best_model_row)[1],
                            paste(best_model_row$combination, names(best_model_row)[1], sep = "_"))
  
  # Return the best model and its MAPE
  list(
    best_model_name = best_model_name,
    best_model = best_model,
    mape = best_model_row$mape,
    all_mape = models_with_forecasts %>% select(.model, mape)
  )
}

# Example usage with the tourism data
# Define covariates
covariates <- c("unemployment_rate", "income", "holiday_season")

# Apply the function
best_model_result <- select_best_model(
  data = tourism_tsibble,
  response = "Trips",
  covariates = covariates,
  h = 8  # Number of periods for test set
)

# View the best model name and MAPE
best_model_result$best_model_name
best_model_result$mape

# View all MAPE values
best_model_result$all_mape