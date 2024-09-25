# Load necessary packages
library(fable)
library(fabletools)
library(tsibble)
library(dplyr)
library(purrr)
library(rlang)
library(tidyr)

#' Basic Time Series Modeling Function
#'
#' @param df A tsibble containing the data.
#' @param var_y The name of the dependent variable (as a string).
#' @return A mable (model table) with fitted models.
modelling_basic <- function(df, var_y) {
  # Ensure 'time' column is in yearmonth format
  df <- df %>% mutate(time = yearmonth(time))
  
  # Define possible transformations
  transformations <- list(
    nivel = identity,           # No transformation
    log = function(x) log(x)    # Logarithm
  )
  
  # Define models to be tested
  model_functions <- list(
    arima = ARIMA,              # ARIMA
    ets = ETS,                  # Exponential Smoothing State Space (ETS)
    snaive = SNAIVE,            # Seasonal Naive
    nnetar = NNETAR,            # Neural Network Autoregressive
    tslm = TSLM                 # Time Series Linear Model
  )
  
  # Generate combinations of transformations and models
  combinations <- expand_grid(
    trans_name = names(transformations),
    model_name = names(model_functions)
  )
  
  # Generate model specifications
  model_specs <- pmap(combinations, function(trans_name, model_name) {
    trans_func <- transformations[[trans_name]]
    model_func <- model_functions[[model_name]]
    
    # Apply transformation to dependent variable
    df_transformed <- df %>%
      mutate(!!var_y := trans_func(!!sym(var_y))) %>%
      filter(!is.na(!!sym(var_y)))
    
    # Create model formula
    model_formula <- model_func(!!sym(var_y))
    
    # Model label
    model_label <- paste0(trans_name, "_", model_name)
    
    # Return named list of model definitions
    set_names(list(model_formula), model_label)
  }) %>% list_flatten()
  
  # Fit models to data
  df %>% model(!!!model_specs)
}

#' Time Series Modeling Function with Dummy Variables
#'
#' @param df A tsibble containing the data.
#' @param var_y The name of the dependent variable (as a string).
#' @param dummy_vars A vector of dummy variable names (as strings).
#' @return A mable (model table) with fitted models.
modelling_dummy <- function(df, var_y, dummy_vars) {
  # Ensure 'time' column is in yearmonth format
  df <- df %>% mutate(time = yearmonth(time))
  
  # Define possible transformations
  transformations <- list(
    nivel = identity,           # No transformation
    log = function(x) log(x)    # Logarithm
  )
  
  # Define models to be tested
  model_functions <- list(
    arima = ARIMA,
    tslm = TSLM,
    nnetar = NNETAR
  )
  
  # Generate combinations of transformations and models
  combinations <- expand_grid(
    trans_name = names(transformations),
    model_name = names(model_functions)
  )
  
  # Generate model specifications
  model_specs <- pmap(combinations, function(trans_name, model_name) {
    trans_func <- transformations[[trans_name]]
    model_func <- model_functions[[model_name]]
    
    # Apply transformation to dependent variable
    df_transformed <- df %>%
      mutate(!!var_y := trans_func(!!sym(var_y))) %>%
      filter(!is.na(!!sym(var_y)))
    
    # Build formula with dummy variables
    formula_str <- paste(var_y, "~", paste(dummy_vars, collapse = " + "))
    model_formula <- as.formula(formula_str)
    
    # Model label
    model_label <- paste0(trans_name, "_", model_name, "_dummy")
    
    # Create model definition
    model_definition <- model_func(model_formula)
    
    # Return named list of model definitions
    set_names(list(model_definition), model_label)
  }) %>% list_flatten()
  
  # Fit models to data
  df %>% model(!!!model_specs)
}

#' Time Series Modeling Function with Lagged Exogenous Variables
#'
#' @param df A tsibble containing the data.
#' @param var_y The name of the dependent variable (as a string).
#' @param var_exogenous A vector of exogenous variable names (as strings).
#' @param lag Number of periods to lag the exogenous variables.
#' @param dummy_vars A vector of dummy variable names (as strings).
#' @return A list containing mables with fitted models for different transformations.
modelling_lagged <- function(df, var_y, var_exogenous, lag = 0, dummy_vars = NULL) {
  # Ensure 'time' column is in yearmonth format
  df <- df %>% mutate(time = yearmonth(time))
  
  # Fill implicit gaps in tsibble
  df <- df %>% fill_gaps()
  
  # Apply lag to exogenous variables if lag > 0
  if (lag > 0) {
    df <- df %>%
      mutate(across(all_of(var_exogenous), ~ lag(.x, lag)))
  }
  
  # Define possible transformations
  transformations <- list(
    nivel = identity,                              # No transformation
    log = function(x) ifelse(x > 0, log(x), NA)    # Logarithm for positive values
  )
  
  # Define models to be tested
  model_functions <- list(
    arima = ARIMA,
    tslm = TSLM
  )
  
  # Generate combinations of transformations and models
  combinations <- expand_grid(
    trans_name = names(transformations),
    model_name = names(model_functions)
  )
  
  # Generate model specifications
  model_specs <- pmap(combinations, function(trans_name, model_name) {
    trans_func <- transformations[[trans_name]]
    model_func <- model_functions[[model_name]]
    
    # Apply transformation to dependent variable
    df_transformed <- df %>%
      mutate(!!var_y := trans_func(!!sym(var_y))) %>%
      filter(!is.na(!!sym(var_y)))
    
    # Build formula with exogenous and dummy variables
    predictors <- c(var_exogenous, dummy_vars)
    formula_str <- paste(var_y, "~", paste(predictors, collapse = " + "))
    model_formula <- as.formula(formula_str)
    
    # Model label
    model_label <- paste(trans_name, model_name, sep = "_")
    
    # Create model definition
    model_definition <- model_func(model_formula)
    
    # Return named list of model definitions
    set_names(list(model_definition), model_label)
  }) %>% list_flatten()
  
  # Fit models for 'nivel' transformation
  df_nivel <- df %>%
    mutate(!!var_y := transformations$nivel(!!sym(var_y))) %>%
    filter(!is.na(!!sym(var_y)))
  
  mable_nivel <- df_nivel %>%
    model(!!!model_specs[grepl("^nivel", names(model_specs))])
  
  # Fit models for 'log' transformation
  df_log <- df %>%
    mutate(!!var_y := transformations$log(!!sym(var_y))) %>%
    filter(!is.na(!!sym(var_y)))
  
  mable_log <- df_log %>%
    model(!!!model_specs[grepl("^log", names(model_specs))])
  
  return(list(mable_nivel = mable_nivel, mable_log = mable_log))
}

#' Ljung-Box Test for Autocorrelation of Residuals
#'
#' @param model A fitted model (mable).
#' @param lags Number of lags to test.
#' @return A tibble with Ljung-Box test statistics.
ljung_box_auto <- function(model, lags = 10) {
  model %>%
    augment() %>%
    features(.resid, ljung_box, lag = lags)
}

#' Complete Time Series Modeling Function
#'
#' @param df A tsibble containing the data.
#' @param var_y The name of the dependent variable (as a string).
#' @param var_exogenous A vector of exogenous variable names (as strings).
#' @param lags A vector of lags to apply to exogenous variables.
#' @param dummy_vars A vector of dummy variable names (as strings).
#' @param train_set A date or year defining the training set end.
#' @return A list containing the best model, its forecast, and accuracy measures.
modelling_complete <- function(df,
                               var_y,
                               var_exogenous = NULL,
                               lags = 0,
                               dummy_vars = NULL,
                               train_set) {
  # Convert 'time' column to yearmonth format
  df <- df %>% mutate(time = yearmonth(time))
  
  # Parse 'train_set' to get training and testing periods
  if (is.character(train_set)) {
    training_end <- yearmonth(as.Date(train_set))
  } else if (is.numeric(train_set)) {
    training_end <- yearmonth(as.Date(paste0(train_set, "-12-31")))
  } else {
    stop("train_set must be a character date or numeric year")
  }
  testing_start <- training_end + 1
  
  # Split data into training and testing sets
  df_train <- df %>% filter(time <= training_end)
  df_test <- df %>% filter(time >= testing_start)
  
  # Define possible transformations
  transformations <- list(
    nivel = identity,           # No transformation
    log = function(x) ifelse(x > 0, log(x), NA)  # Logarithm for positive values
  )
  
  # Initialize list to store forecasts
  forecast_list <- list()
  
  # Loop over transformations
  for (trans_name in names(transformations)) {
    trans_func <- transformations[[trans_name]]
    
    # Apply transformation to dependent variable in training set
    df_train_transformed <- df_train %>%
      mutate(!!var_y := trans_func(!!sym(var_y))) %>%
      filter(!is.na(!!sym(var_y)))
    
    # Initialize list to store models for this transformation
    models <- list()
    
    # Basic models
    models$basic <- modelling_basic(df_train_transformed, var_y)
    
    # Models with dummy variables, if any
    if (!is.null(dummy_vars)) {
      models$dummy <- modelling_dummy(df_train_transformed, var_y, dummy_vars)
    }
    
    # Models with lagged exogenous variables, if any
    if (!is.null(var_exogenous)) {
      models$lagged <- map(lags, function(lag_value) {
        modelling_lagged(
          df = df_train_transformed,
          var_y = var_y,
          var_exogenous = var_exogenous,
          lag = lag_value,
          dummy_vars = dummy_vars
        )
      }) %>% flatten()
    }
    
    # Forecast with each model set
    forecasts <- map(models, function(model_set) {
      model_set %>% forecast(new_data = df_test)
    }) %>% flatten()
    
    # Store forecasts
    forecast_list[[trans_name]] <- forecasts
  }
  
  # Combine all forecasts
  all_forecasts <- bind_rows(forecast_list %>% flatten())
  
  # Calculate accuracy
  accuracies <- all_forecasts %>%
    accuracy(df_test) %>%
    arrange(MAPE)
  
  # Select best model based on MAPE
  best_model_name <- accuracies$.model[1]
  best_forecast <- all_forecasts %>% filter(.model == best_model_name)
  
  # Return results
  list(
    best_model_name = best_model_name,
    best_forecast = best_forecast,
    accuracies = accuracies
  )
}

# Simulate a time series ARIMA data and convert to tsibble
data <- tibble(
  time = seq(as.Date("2000-01-01"), by = "month", length.out = 200),
  y = arima.sim(model = list(ar = c(0.8897, -0.1858), ma = c(-0.2279, 1.2488), seasonal = list(order = c(1, 0, 2), period = 12)), n = 200) %>%
    as.numeric() + 10 + (time >= as.Date("2013-01-01")) * 10.5,
  covariada_1 = arima.sim(model = list(ar = c(0.5, -0.05), ma = c(-0.2, 0.8)), n = 200) %>%
    as.numeric() + 6.5 + (time >= as.Date("2013-01-01")) * 5.5,
  covariada_2 = arima.sim(model = list(ar = c(0.15, -0.005), ma = c(-0.02, 0.08)), n = 200) %>%
    as.numeric() + 8.5 - (time >= as.Date("2013-01-01")) * 5.5,
  quebra = if_else(time >= as.Date("2013-01-01"), 1, 0)
) %>%
  as_tsibble(index = time)

autoplot(data, color = 'steelblue') +
  autolayer(data, covariada_1, color = 'blue', alpha = 0.5) +
  autolayer(data, covariada_2, color = 'blue', alpha = 0.5) +
  autolayer(data, quebra, color = 'blue', alpha = 0.5)

modelling_complete(df = data,
                   var_y = 'y',
                   var_exogenous = c('covariada_1','covariada_2'),
                   dummy = 'quebra',
                   train_set = '2012-07-01')
