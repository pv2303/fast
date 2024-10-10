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


#### Teste ####
# Example dataset: tourism data
data("tourism")

# Subset the tourism dataset for the region "Adelaide"
tourism_tsibble <- tourism %>%
  filter(Region == "Adelaide", Purpose == 'Business') %>%
  mutate(
    unemployment_rate = runif(n(), 5, 10),  # Example covariate
    income = runif(n(), 40000, 80000),      # Example covariate
    holiday_season = ifelse(Quarter %in% c("Q4", "Q1"), 1, 0)  # Dummy for holiday season
  ) %>%
  as_tsibble(index = Quarter)

# Fit univariate models and forecast
univariate_results <- fit_univariate_models(
  data = tourism_tsibble,
  response = "Trips"
)

# Access the fitted models
univariate_results$models

# Define covariates
covariates <- c("unemployment_rate", "income")

# Fit models with all combinations of covariates
covariate_results <- fit_covariate_models(
  data = tourism_tsibble,
  response = "Trips",
  covariates = covariates
)


# Access the fitted models
covariate_results 
