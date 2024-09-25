# Loading required libraries

library(fpp3)        # For time series forecasting
library(tidyverse)   # For data manipulation
library(magrittr)    # For using the pipe operator

# Function for basic time series modelling
modelling_basic <- function(df, var_y) {
  df %>%
    dplyr::select(tidyselect::everything(), var_y = {{var_y}}) %>%
    fabletools::model(
      # Model specifications 
      
      # level
      .nivel_arima = fable::ARIMA(var_y),
      .nivel_ets = fable::ETS(var_y),
      .nivel_snaive = fable::SNAIVE(var_y),
      .nivel_nnetar = fable::NNETAR(var_y),
      .nivel_tslm = fable::TSLM(var_y),
      
      # log
      .log_arima = fable::ARIMA(log(var_y)),
      .log_ets = fable::ETS(log(var_y)),
      .log_snaive = fable::SNAIVE(log(var_y)),
      .log_nnetar = fable::NNETAR(log(var_y)),
      .log_tslm = fable::TSLM(log(var_y)),
    ) %>% return()
}

# Function for modelling with dummy variables
modelling_dummy <- function(df, var_y, dummy) {
  # Creating formula strings with dummy variables
  form_n_n_d = stats::formula(stringr::str_c(var_y, " ~ ",
                                             stringr::str_c(dummy, collapse = " + ")))
  form_l_n_d = stats::formula(stringr::str_c("log(", var_y, ") ~ ",
                                             stringr::str_c(dummy, collapse = " + ")))
  
  # Time series models with only dummy variables
  model_only_dummy <- df %>%
    fabletools::model(
      # Model specifications 
      
      # level
      .nivel_arimax_only_dummy = fable::ARIMA(form_n_n_d),
      .nivel_tslm_only_dummy = fable::TSLM(form_n_n_d),
      .nivel_nnetar_only_dummy = fable::NNETAR(form_n_n_d),
      
      # log
      .log_arimax_only_dummy = fable::ARIMA(form_l_n_d),
      .log_tslm_only_dummy = fable::TSLM(form_l_n_d),
      .log_nnetar_only_dummy = fable::NNETAR(form_l_n_d)
    )
  return(model_only_dummy)
}

# Function for creating time series models with lagged variables
modelling_lagged <- function(df,
                                   var_y,
                                   var_exogena,
                                   lag = 0,
                                   dummy = NULL) {
  
    # formulas for models without lag (lag = 0)
    form_n_n_lag_0 = stats::formula(stringr::str_c(var_y, " ~ ", 
                                            stringr::str_c(var_exogena,
                                                           collapse = " + ")))
    form_l_n_lag_0 = stats::formula(stringr::str_c("log(", var_y, ") ~ ",
                                            stringr::str_c(var_exogena, 
                                                           collapse = " + ")))
    form_n_l_lag_0 = stats::formula(stringr::str_c(var_y, " ~ ",
                                            stringr::str_c("log(", var_exogena, ")", 
                                                           collapse = " + ")))
    form_l_l_lag_0 = stats::formula(stringr::str_c("log(", var_y, ") ~ ", 
                                            stringr::str_c("log(", var_exogena, ")", 
                                                           collapse = " + ")))
    
    # formulas for models with lag (lag > 0)
    form_n_n = stats::formula(stringr::str_c(var_y, " ~ ", 
                                      stringr::str_c("lag(", var_exogena, ",", lag, ")", 
                                                     collapse = " + ")))
    form_l_n = stats::formula(stringr::str_c("log(", var_y, ") ~ ",
                                      stringr::str_c("lag(", var_exogena, ",", lag, ")", 
                                                     collapse = " + ")))
    form_n_l = stats::formula(stringr::str_c(var_y, " ~ ", 
                                      stringr::str_c("log(lag(", var_exogena, ",", lag, "))", 
                                                     collapse = " + ")))
    form_l_l = stats::formula(stringr::str_c("log(", var_y, ") ~ ", 
                                      stringr::str_c("log(lag(", var_exogena, ",", lag, "))",
                                                     collapse = " + ")))
    
    # Creating model column names dynamically based on the lag value
    col_arima_n_n <- paste0(".n_n_arimax_lag_", dplyr::quo_name(lag))
    col_tslm_n_n <- paste0(".n_n_tslm_lag_", dplyr::quo_name(lag))
    col_arima_l_n <- paste0(".l_n_arimax_lag_", dplyr::quo_name(lag))
    col_tslm_l_n <- paste0(".l_n_tslm_lag_", dplyr::quo_name(lag))
    col_arima_n_l <- paste0(".n_l_arimax_lag_", dplyr::quo_name(lag))
    col_tslm_n_l <- paste0(".n_l_tslm_lag_", dplyr::quo_name(lag))
    col_arima_l_l <- paste0(".l_l_arimax_lag_", dplyr::quo_name(lag))
    col_tslm_l_l <- paste0(".l_l_tslm_lag_", dplyr::quo_name(lag))
    
    # Building models based on whether lag is 0 or greater
    if (lag == 0) {
      model_sem_dummy <- df %>%
        fabletools::model(
          # Model configurations for level-level, log-level, level-log, and log-log (no lag)
          
          # level-level
          !!(col_arima_n_n) := fable::ARIMA(form_n_n_lag_0),
          !!(col_tslm_n_n) := fable::TSLM(form_n_n_lag_0),
          .n_n_nnetar_X = fable::NNETAR(form_n_n_lag_0),
          
          # log-level
          !!(col_arima_l_n) := fable::ARIMA(form_l_n_lag_0),
          !!(col_tslm_l_n) := fable::TSLM(form_l_n_lag_0),
          .l_n_nnetar_X = fable::NNETAR(form_l_n_lag_0),
          
          # level-log
          !!(col_arima_n_l) := fable::ARIMA(form_n_l_lag_0),
          !!(col_tslm_n_l) := fable::TSLM(form_n_l_lag_0),
          .n_l_nnetar_X = fable::NNETAR(form_n_l_lag_0),
          
          # log-log
          !!(col_arima_l_l) := fable::ARIMA(form_l_l_lag_0),
          !!(col_tslm_l_l) := fable::TSLM(form_l_l_lag_0),
          .l_l_nnetar_X = fable::NNETAR(form_l_l_lag_0)
        )
    } else {
      model_sem_dummy <- df %>%
        fabletools::model(
          # Model configurations for different transformations with lag
          
          # level-level
          !!(col_arima_n_n) := fable::ARIMA(form_n_n),
          !!(col_tslm_n_n) := fable::TSLM(form_n_n),
          
          # log-level
          !!(col_arima_l_n) := fable::ARIMA(form_l_n),
          !!(col_tslm_l_n) := fable::TSLM(form_l_n),
          
          # level-log
          !!(col_arima_n_l) := fable::ARIMA(form_n_l),
          !!(col_tslm_n_l) := fable::TSLM(form_n_l),
          
          # log-log
          !!(col_arima_l_l) := fable::ARIMA(form_l_l),
          !!(col_tslm_l_l) := fable::TSLM(form_l_l)
        )
    }
    
    # if dummy variables are provided, create additional models incorporating these variables
    if (is.null(dummy)) {
      return(model_sem_dummy)
    } else {
      # formulas for models with dummy variables and no lag
      form_n_n_x_d_lag_0 = stats::formula(stringr::str_c(var_y, " ~ ",
                                                  stringr::str_c(var_exogena, collapse = " + "),
                                                  " + ", stringr::str_c(dummy, collapse = " + ")))
      form_l_n_x_d_lag_0 = stats::formula(stringr::str_c("log(", var_y, ") ~ ",
                                                  stringr::str_c(var_exogena, collapse = " + "),
                                                  " + ", stringr::str_c(dummy, collapse = " + ")))
      form_n_l_x_d_lag_0 = stats::formula(stringr::str_c(var_y, " ~ ",
                                                  stringr::str_c("log(", var_exogena, ")", collapse = " + "),
                                                  " + ", stringr::str_c(dummy, collapse = " + ")))
      form_l_l_x_d_lag_0 = stats::formula(stringr::str_c("log(", var_y, ") ~ ",
                                                  stringr::str_c("log(", var_exogena, ")", collapse = " + "),
                                                  " + ", stringr::str_c(dummy, collapse = " + ")))
      
      # formulas for models with dummy variables and lag (lag > 0)
      form_n_n_x_d = stats::formula(stringr::str_c(var_y, " ~ ",
                                            stringr::str_c("lag(", var_exogena, ",", lag, ")", collapse = " + "),
                                            " + ", stringr::str_c(dummy, collapse = " + ")))
      form_l_n_x_d = stats::formula(stringr::str_c("log(", var_y, ") ~ ",
                                             stringr::str_c("lag(", var_exogena, ",", lag, ")", collapse = " + "),
                                             " + ", stringr::str_c(dummy, collapse = " + ")))
      form_n_l_x_d = stats::formula(stringr::str_c(var_y, " ~ ",
                                            stringr::str_c("log(lag(", var_exogena, ",", lag, "))", collapse = " + "),
                                            " + ", stringr::str_c(dummy, collapse = " + ")))
      form_l_l_x_d = stats::formula(stringr::str_c("log(", var_y, ") ~ ",
                                            stringr::str_c("log(lag(", var_exogena, ",", lag, "))", collapse = " + "),
                                            " + ", stringr::str_c(dummy, collapse = " + ")))
      
      # Generating dynamic column names for models incorporating dummy variables
      col_arima_n_n_d <- paste0(".n_n_arimax_dummy_X_lag_", dplyr::quo_name(lag))
      col_tslm_n_n_d  <- paste0(".n_n_tslm_dummy_X_lag_", dplyr::quo_name(lag))
      col_arima_l_n_d <- paste0(".l_n_arimax_dummy_X_lag_", dplyr::quo_name(lag))
      col_tslm_l_n_d  <- paste0(".l_n_tslm_dummy_X_lag_", dplyr::quo_name(lag))
      col_arima_n_l_d <- paste0(".n_l_arimax_dummy_X_lag_", dplyr::quo_name(lag))
      col_tslm_n_l_d  <- paste0(".n_l_tslm_dummy_X_lag_", dplyr::quo_name(lag))
      col_arima_l_l_d <- paste0(".l_l_arimax_dummy_X_lag_", dplyr::quo_name(lag))
      col_tslm_l_l_d  <- paste0(".l_l_tslm_dummy_X_lag_", dplyr::quo_name(lag))
      
      
      # Building models with dummy variables
      # different model configurations are created based on the presence of lag and dummy variables
      if (lag == 0) {
        model_with_dummy <- df %>%
          fabletools::model(
            # Model configurations for different transformations (with dummy variables and no lag)
            
            # level-level
            !!(col_arima_n_n_d) := fable::ARIMA(form_n_n_x_d_lag_0),
            !!(col_tslm_n_n_d) := fable::TSLM(form_n_n_x_d_lag_0),
            .n_n_nnetar_dummy_X = fable::NNETAR(form_n_n_x_d_lag_0),
            
            # log-level
            !!(col_arima_l_n_d) := fable::ARIMA(form_l_n_x_d_lag_0),
            !!(col_tslm_l_n_d) := fable::TSLM(form_l_n_x_d_lag_0),
            .l_n_nnetar_dummy_X = fable::NNETAR(form_l_n_x_d_lag_0),
            # level-log
            !!(col_arima_n_l_d) := fable::ARIMA(form_n_l_x_d_lag_0),
            !!(col_tslm_n_l_d) := fable::TSLM(form_n_l_x_d_lag_0),
            .n_l_nnetar_dummy_X = fable::NNETAR(form_n_l_x_d_lag_0),
            # log-log
            !!(col_arima_l_l_d) := fable::ARIMA(form_l_l_x_d_lag_0),
            !!(col_tslm_l_l_d) := fable::TSLM(form_l_l_x_d_lag_0),
            .l_l_nnetar_dummy_X = fable::NNETAR(form_l_l_x_d_lag_0)
          )
      } else {
        model_with_dummy <- df %>%
          fabletools::model(
            # Model configurations for different transformations (with dummy variables and lag)
            
            # level-level
            !!(col_arima_n_n_d) := fable::ARIMA(form_n_n_x_d),
            !!(col_tslm_n_n_d)  := fable::TSLM(form_n_n_x_d),
            
            # log-level
            !!(col_arima_l_n_d) := fable::ARIMA(form_l_n_x_d),
            !!(col_tslm_l_n_d)  := fable::TSLM(form_l_n_x_d),
            
            # level-log
            !!(col_arima_n_l_d) := fable::ARIMA(form_n_l_x_d),
            !!(col_tslm_n_l_d)  := fable::TSLM(form_n_l_x_d),
            
            # log-log
            !!(col_arima_l_l_d) := fable::ARIMA(form_l_l_x_d),
            !!(col_tslm_l_l_d)  := fable::TSLM(form_l_l_x_d)
          )
      }
      
      # Combining models with and without dummy variables
      dplyr::bind_cols(model_sem_dummy, model_with_dummy) %>%
        return()
    }
  }

# Function for performing the Ljung-Box test on a given model
ljung_box_auto <- function(modelo, lag) {
  modelo %>%
    # Augments the model with additional information useful for analysis
    fabletools::augment() %>%
    # Computes Ljung-Box test statistics for the specified lags
    fabletools::features(.innov, ljung_box, lag = lag)
}

# Function to complete the modelling process including training, testing, and model selection
modelling_complete <- function(df,
                               var_y,
                               var_exogena = NULL,
                               lags = 0,
                               dummy = NULL,
                               train_set) {
  # Initialize variables and select the target variable
  predicted_var <- var_y
  df_basic <- df %>% dplyr::select(everything(), var_y = {{var_y}})
  
  # Determine the training and testing set based on the type of train_set input (character or numeric)
  if (is.character(train_set) == T) {
    training <- train_set
    testing <- lubridate::ym(train_set) %m+% months(1) %>%
      as.character()
  } else if (is.numeric(train_set) == T) {
    training <- train_set
    testing <- train_set + 1
  } else {
    rlang::abort(
      "the train_set arg must be a character for monthly data (YYYY-MM) or numeric for yearly data"
    )
  }
  
  # Filter the dataframe for the training period
  df_treino <- df_basic %>%
    tsibble::filter_index(. ~ training)
  
  # Calculate the horizon for forecasting
  h <- df %>%
    tsibble::filter_index(testing ~ .) %>%
    dplyr::count() %>%
    base::as.double()
  
  # Build basic models using the training data
  model_basic <- modelling_basic(df_treino, "var_y")
  models <- model_basic
  
  # if dummy variables are not null, add dummy variable models
  if (!is.null(dummy)) {
    model_dummy <- modelling_dummy(df_treino, "var_y", dummy)
    models <- bind_cols(models, model_dummy)
  }
  
  # if external variables are provided, add lagged variable models
  if (!is.null(var_exogena)) {
    model_lagged <- tibble::tibble(
      exg_lag = lags,
      model_lagged = purrr::map(
        exg_lag,
        modelling_lagged,
        df = df_treino,
        var_y = "var_y",
        var_exogena = var_exogena,
        dummy = dummy
      )
    ) %>%
      dplyr::select(model_lagged) %$%
      purrr::reduce(model_lagged, dplyr::bind_cols)
    models <- dplyr::bind_cols(models, model_lagged)
  }
  
  # Apply Ljung-Box test to the models and filter out the ones that pass the test
  models_lb <- tibble::tibble(lag_teste = 1:10,
                              models = list(models)) %>%
    dplyr::mutate(teste_lb = purrr::map2(models, lag_teste, ljung_box_auto)) %>%
    dplyr::select(-models) %>%
    dplyr::unnest(teste_lb)
  
  # Select models that passed the Ljung-Box test
  models_passed <- models_lb %>%
    dplyr::mutate(maior_p_5 = if_else(lb_pvalue >= .05, 1, 0)) %>%
    dplyr::summarise(maior_p_5 = sum(maior_p_5),
                     .by = c(.model)) %>%
    dplyr::filter(maior_p_5 == 10)
  
  # Select models that passed the Ljung-Box test
  models <- models %>%
    dplyr::select(tidyselect::any_of(models_passed$.model))
  
  # Forecast using the selected models
  models_forecast <- models %>% fabletools::forecast(new_data = df_basic %>%
                                                       tsibble::filter_index(testing ~ .))
  
  # Calculate accuracy of the forecasts
  models_acc <- models_forecast %>%
    fabletools::accuracy(df_basic) %>%
    dplyr::arrange(MAPE)
  
  # Identify the best model based on MAPE
  model_best_name <- models_acc %>%
    dplyr::arrange(MAPE) %>%
    dplyr::top_n(1, -MAPE) %>%
    dplyr::pull(.model)
  
  # Select the best model and its prediction
  best_model <- dplyr::select(models, {{model_best_name}})
  best_prediction <- dplyr::filter(models_forecast, .model == model_best_name)
  
  # Arrange final accuracies
  acc_final <- dplyr::arrange(models_acc, MAPE)
  
  # Compile final results
  final <- base::list(
    best_model_name = model_best_name,
    best_model = best_model,
    best_prediction = best_prediction,
    accuracies = acc_final,
    df = df_basic,
    y = predicted_var
  )
  
  # return the final list containing model and forecast details
  return(final)
}


# Function for generating forecasts from a list of models
forecasting <- function(modelling_list,
                        h = 12,
                        new_data = NULL) {
  # Refitting the best model from the modelling list to the entire dataset
  refitting <- modelling_list %$%
    fabletools::refit(best_model, df)
  
  # Check if new data is provided and is in tsibble format
  if (tsibble::is_tsibble(new_data) == TRUE) {
    # if new data is provided, generate forecasts using the new data
    results <- fabletools::forecast(refitting,
                                    new_data = new_data)
  } else {
    # if no new data is provided, generate forecasts for a specified horizon (h)
    results <- fabletools::forecast(refitting,
                                    h = h)
  }
  
  # return the forecast results
  return(results)
}
