# Libraries

library(fpp3)
library(tidyverse)
library(magrittr)

# Funcoes de modelagem basica ---------
modelling_basic <- function(df, var_y){
  df %>%
    select(everything(), var_y = {{var_y}}) %>% 
    fabletools::model(
      # Modelagem em nivel
      .nivel_arima = fable::ARIMA(var_y),
      .nivel_ets = fable::ETS(var_y),
      .nivel_snaive = fable::SNAIVE(var_y),
      .nivel_nnetar = fable::NNETAR(var_y),
      .nivel_tslm = fable::TSLM(var_y),
      # em log
      .log_arima = fable::ARIMA(log(var_y)),
      .log_ets = fable::ETS(log(var_y)),
      .log_snaive = fable::SNAIVE(log(var_y)),
      .log_nnetar = fable::NNETAR(log(var_y)),
      .log_tslm = fable::TSLM(log(var_y)),
    ) %>% return()
}
modelling_dummy <- function(df, var_y, dummy){
  # so a dummy
  form_n_n_d = formula(stringr::str_c(var_y, " ~ ", stringr::str_c(dummy, collapse = " + ")))
  form_l_n_d = formula(stringr::str_c("log(",var_y,") ~ ", stringr::str_c(dummy, collapse = " + ")))
  
  model_only_dummy <- df %>% 
    fabletools::model(
      #nivel-nivel
      .nivel_arimax_only_dummy = fable::ARIMA(form_n_n_d),
      .nivel_tslm_only_dummy = fable::TSLM(form_n_n_d),
      .nivel_nnetar_only_dummy = fable::NNETAR(form_n_n_d),
      #log-nivel
      .log_arimax_only_dummy = fable::ARIMA(form_l_n_d),
      .log_tslm_only_dummy = fable::TSLM(form_l_n_d),
      .log_nnetar_only_dummy = fable::NNETAR(form_l_n_d)
    )
  return(model_only_dummy)
}
modelling_lagged <- function(df, var_y, var_exogena, lag = 0, dummy = NULL){
  # formulas lag = 0
  form_n_n_lag_0 = formula(stringr::str_c(var_y, " ~ ", stringr::str_c(var_exogena,
                                                                 collapse = " + ")))
  form_l_n_lag_0 = formula(stringr::str_c("log(",var_y,") ~ ", stringr::str_c(var_exogena,
                                                                        collapse = " + ")))
  form_n_l_lag_0 = formula(stringr::str_c(var_y," ~ ", stringr::str_c("log(",var_exogena,")",
                                                                collapse = " + ")))
  form_l_l_lag_0 = formula(stringr::str_c("log(",var_y,") ~ ", stringr::str_c("log(", var_exogena,")",
                                                                        collapse = " + ")))
  # formulas lag > 0  
  form_n_n = formula(stringr::str_c(var_y, " ~ ", stringr::str_c("lag(",var_exogena,",",lag,")",
                                                                 collapse = " + ")))
  form_l_n = formula(stringr::str_c("log(",var_y,") ~ ", stringr::str_c("lag(",var_exogena,",",lag,")",
                                                                        collapse = " + ")))
  form_n_l = formula(stringr::str_c(var_y," ~ ", stringr::str_c("log(lag(",var_exogena,",",lag,"))",
                                                                collapse = " + ")))
  form_l_l = formula(stringr::str_c("log(",var_y,") ~ ", stringr::str_c("log(lag(",var_exogena,",",lag,"))",
                                                                        collapse = " + ")))
  col_arima_n_n <- paste0(".n_n_arimax_lag_", dplyr::quo_name(lag))
  col_tslm_n_n <- paste0(".n_n_tslm_lag_", dplyr::quo_name(lag))
  col_arima_l_n <- paste0(".l_n_arimax_lag_", dplyr::quo_name(lag))
  col_tslm_l_n <- paste0(".l_n_tslm_lag_", dplyr::quo_name(lag))    
  col_arima_n_l <- paste0(".n_l_arimax_lag_", dplyr::quo_name(lag))
  col_tslm_n_l <- paste0(".n_l_tslm_lag_", dplyr::quo_name(lag))    
  col_arima_l_l <- paste0(".l_l_arimax_lag_", dplyr::quo_name(lag))
  col_tslm_l_l <- paste0(".l_l_tslm_lag_", dplyr::quo_name(lag))
  
  if (lag == 0){
    model_sem_dummy <- df %>% 
      fabletools::model(
        #nivel-nivel
        !!(col_arima_n_n) := fable::ARIMA(form_n_n_lag_0),
        !!(col_tslm_n_n) := fable::TSLM(form_n_n_lag_0),
        .n_n_nnetar_X = fable::NNETAR(form_n_n_lag_0),
        #log-nivel
        !!(col_arima_l_n) := fable::ARIMA(form_l_n_lag_0),
        !!(col_tslm_l_n) := fable::TSLM(form_l_n_lag_0),
        .l_n_nnetar_X = fable::NNETAR(form_l_n_lag_0),
        #nivel-log
        !!(col_arima_n_l) := fable::ARIMA(form_n_l_lag_0),
        !!(col_tslm_n_l) := fable::TSLM(form_n_l_lag_0),
        .n_l_nnetar_X = fable::NNETAR(form_n_l_lag_0),
        #log-log
        !!(col_arima_l_l) := fable::ARIMA(form_l_l_lag_0),
        !!(col_tslm_l_l) := fable::TSLM(form_l_l_lag_0),
        .l_l_nnetar_X = fable::NNETAR(form_l_l_lag_0))} else {
          model_sem_dummy <- df %>% 
            fabletools::model(
              #nivel-nivel
              !!(col_arima_n_n) := fable::ARIMA(form_n_n),
              !!(col_tslm_n_n) := fable::TSLM(form_n_n),
              #log-nivel
              !!(col_arima_l_n) := fable::ARIMA(form_l_n),
              !!(col_tslm_l_n) := fable::TSLM(form_l_n),
              #nivel-log
              !!(col_arima_n_l) := fable::ARIMA(form_n_l),
              !!(col_tslm_n_l) := fable::TSLM(form_n_l),
              #log-log
              !!(col_arima_l_l) := fable::ARIMA(form_l_l),
              !!(col_tslm_l_l) := fable::TSLM(form_l_l)) }
  if (is.null(dummy) == TRUE) {
    return(model_sem_dummy)
  } else {
    # Formulas lag = 0
    form_n_n_x_d_lag_0 = formula(stringr::str_c(var_y, " ~ ", 
                                                stringr::str_c(var_exogena, collapse = " + "), " + ",
                                                stringr::str_c(dummy, collapse = " + ")))
    form_l_n_x_d_lag_0 = formula(stringr::str_c("log(",var_y,") ~ ", 
                                                stringr::str_c(var_exogena, collapse = " + "), " + ",
                                                stringr::str_c(dummy, collapse = " + ")))
    form_n_l_x_d_lag_0 = formula(stringr::str_c(var_y," ~ ", 
                                                stringr::str_c("log(",var_exogena,")", collapse = " + "), " + ",
                                                stringr::str_c(dummy, collapse = " + ")))
    form_l_l_x_d_lag_0 = formula(stringr::str_c("log(",var_y,") ~ ", 
                                                stringr::str_c("log(",var_exogena,")", collapse = " + "), " + ",
                                                stringr::str_c(dummy, collapse = " + ")))
    
    # Formulas lag > 0
    form_n_n_x_d = formula(stringr::str_c(var_y, " ~ ", stringr::str_c("lag(",var_exogena,",",lag,")",
                                                                       collapse = " + "), " + ",
                                          stringr::str_c(dummy, collapse = " + ")))
    form_l_n_x_d = formula(stringr::str_c("log(",var_y,") ~ ", stringr::str_c("lag(",var_exogena,",",lag,")",
                                                                              collapse = " + "), " + ",
                                          stringr::str_c(dummy, collapse = " + ")))
    form_n_l_x_d = formula(stringr::str_c(var_y," ~ ", stringr::str_c("log(lag(",var_exogena,",",lag,"))",
                                                                      collapse = " + "), " + ",
                                          stringr::str_c(dummy, collapse = " + ")))
    form_l_l_x_d = formula(stringr::str_c("log(",var_y,") ~ ", stringr::str_c("log(lag(",var_exogena,",",lag,"))",
                                                                              collapse = " + "), " + ",
                                          stringr::str_c(dummy, collapse = " + ")))
    
    col_arima_n_n_d <- paste0(".n_n_arimax_dummy_X_lag_", dplyr::quo_name(lag))
    col_tslm_n_n_d  <- paste0(".n_n_tslm_dummy_X_lag_", dplyr::quo_name(lag))
    col_arima_l_n_d <- paste0(".l_n_arimax_dummy_X_lag_", dplyr::quo_name(lag))
    col_tslm_l_n_d  <- paste0(".l_n_tslm_dummy_X_lag_", dplyr::quo_name(lag))    
    col_arima_n_l_d <- paste0(".n_l_arimax_dummy_X_lag_", dplyr::quo_name(lag))
    col_tslm_n_l_d  <- paste0(".n_l_tslm_dummy_X_lag_", dplyr::quo_name(lag))    
    col_arima_l_l_d <- paste0(".l_l_arimax_dummy_X_lag_", dplyr::quo_name(lag))
    col_tslm_l_l_d  <- paste0(".l_l_tslm_dummy_X_lag_", dplyr::quo_name(lag))
    
    
    if (lag == 0) {
      model_with_dummy <- df %>% 
        fabletools::model(
          #nivel-nivel
          !!(col_arima_n_n_d) := fable::ARIMA(form_n_n_x_d_lag_0),
          !!(col_tslm_n_n_d) := fable::TSLM(form_n_n_x_d_lag_0),
          .n_n_nnetar_dummy_X = fable::NNETAR(form_n_n_x_d_lag_0),
          #log-nivel
          !!(col_arima_l_n_d) := fable::ARIMA(form_l_n_x_d_lag_0),
          !!(col_tslm_l_n_d) := fable::TSLM(form_l_n_x_d_lag_0),
          .l_n_nnetar_dummy_X = fable::NNETAR(form_l_n_x_d_lag_0),
          #nivel-log
          !!(col_arima_n_l_d) := fable::ARIMA(form_n_l_x_d_lag_0),
          !!(col_tslm_n_l_d) := fable::TSLM(form_n_l_x_d_lag_0),
          .n_l_nnetar_dummy_X = fable::NNETAR(form_n_l_x_d_lag_0),
          #log-log
          !!(col_arima_l_l_d) := fable::ARIMA(form_l_l_x_d_lag_0),
          !!(col_tslm_l_l_d) := fable::TSLM(form_l_l_x_d_lag_0),
          .l_l_nnetar_dummy_X = fable::NNETAR(form_l_l_x_d_lag_0))
    } else {
      model_with_dummy <- df %>% 
        fabletools::model(
          #nivel-nivel
          !!(col_arima_n_n_d) := fable::ARIMA(form_n_n_x_d),
          !!(col_tslm_n_n_d)  := fable::TSLM(form_n_n_x_d),
          #log-nivel
          !!(col_arima_l_n_d) := fable::ARIMA(form_l_n_x_d),
          !!(col_tslm_l_n_d)  := fable::TSLM(form_l_n_x_d),
          #nivel-log
          !!(col_arima_n_l_d) := fable::ARIMA(form_n_l_x_d),
          !!(col_tslm_n_l_d)  := fable::TSLM(form_n_l_x_d),
          #log-log
          !!(col_arima_l_l_d) := fable::ARIMA(form_l_l_x_d),
          !!(col_tslm_l_l_d)  := fable::TSLM(form_l_l_x_d))}
    
    dplyr::bind_cols(model_sem_dummy, model_with_dummy) %>%
      return()
  }
}

ljung_box_auto <- function(modelo, lag){
  modelo %>% 
    fabletools::augment() %>%
    fabletools::features(.innov, ljung_box, lag = lag)
}

modelling_complete <- function(df, var_y, var_exogena = NULL, 
                               lags = 0, dummy = NULL,
                               train_set){
  # Treino/Teste set
  predicted_var <- var_y
  df_basic <- df %>% dplyr::select(everything(), var_y = {{var_y}})
  if (is.character(train_set) == T) {
    training <- train_set
    testing <- lubridate::ym(train_set) %m+% months(1) %>% 
      as.character()
  } else if (is.numeric(train_set) == T) {
    training <- train_set
    testing <- train_set + 1
  } else {
    rlang::abort("the train_set arg must be a character for monthly data (YYYY-MM) or numeric for yearly data")
  }
  
  df_treino <- df_basic %>% 
    tsibble::filter_index(. ~ training)
  
  h <- df %>% 
    tsibble::filter_index(testing ~ .) %>% 
    dplyr::count() %>% 
    as.double()
  
  # Diagnostico modelo basico
  model_basic    <- modelling_basic(df_treino, "var_y")
  
  models <- model_basic
  
  if (is.null(dummy) == FALSE){
    model_dummy <- modelling_dummy(df_treino, "var_y", dummy)
    models <- bind_cols(models, model_dummy)
  }
  
  if (is.null(var_exogena) == FALSE) {
    model_lagged <- tibble::tibble(exg_lag = lags,
                                   model_lagged = purrr::map(exg_lag, modelling_lagged,
                                                             df = df_treino, 
                                                             var_y = "var_y", 
                                                             var_exogena = var_exogena,
                                                             dummy = dummy)) %>%
      dplyr::select(model_lagged) %$% 
      purrr::reduce(model_lagged, dplyr::bind_cols)
    models <- bind_cols(models, model_lagged)
  }
  
  models_lb <- tibble::tibble(lag_teste = 1:10,
                              models = list(models)) %>% 
    mutate(teste_lb = purrr::map2(models, lag_teste, ljung_box_auto)) %>% 
    select(-models) %>% 
    unnest(teste_lb)
  
  models_passed <- models_lb %>% 
    dplyr::mutate(maior_p_5 = if_else(lb_pvalue >= .05, 1, 0)) %>% 
    dplyr::summarise(maior_p_5 = sum(maior_p_5),
                     .by = c(.model)) %>% 
    dplyr::filter(maior_p_5 == 10)
  
  models <- models %>% 
    dplyr::select(tidyselect::any_of(models_passed$.model))
  
  models_forecast <- models %>% forecast(new_data = df_basic %>% 
                                           tsibble::filter_index(testing ~ .))
  
  models_acc <- models_forecast %>% 
    fabletools::accuracy(df_basic) %>% 
    dplyr::arrange(MAPE)
  
  model_best_name <- models_acc %>% 
    dplyr::arrange(MAPE) %>% 
    dplyr::top_n(1, -MAPE) %>% 
    dplyr::pull(.model)

  best_model <- select(models, {{model_best_name}})
  best_prediction <- filter(models_forecast, .model == model_best_name)
    
  acc_final <- arrange(models_acc, MAPE)
  
  final <- list(best_model_name = model_best_name,
                best_model = best_model,
                best_prediction = best_prediction,
                accuracies = acc_final,
                df = df_basic,
                y = predicted_var)
  
  return(final)
}


forecasting <- function(modelling_list, h = 12, new_data = NULL) {

  
  refitting <- modelling_list %$%
    fabletools::refit(best_model, df)
  
  if (is_tsibble(new_data) == TRUE) {
    results <- fabletools::forecast(refitting,
                                    new_data = new_data)
  } else {
    results <- fabletools::forecast(refitting,
                                    h = h)
  }
  return(results)
}


