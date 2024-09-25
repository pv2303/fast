library(tidyverse)

df <- readxl::read_excel("df.xlsx") %>% 
  mutate(mes = as.Date(mes) %>% tsibble::yearmonth()) %>% 
  tsibble::as_tsibble()

df_treino <- filter_index(df, . ~ '2023-08')

teste_basic <- modelling_basic(df_treino, "var_y")
teste_basic %>% forecast(new_data = filter_index(df, '2023-09' ~ '2023-11'))

teste_dummy <- df_treino %>% modelling_dummy("var_y", "quebra")

teste_lagged <- df_treino %>% modelling_lagged("var_y", c('contagem', 'nl'), dummy = 'quebra')

teste_geral <- bind_cols(teste_basic, teste_dummy, teste_lagged)
  
teste_geral_lb <- tibble::tibble(lag_teste = 1:10,
                                 models = list(teste_geral)) %>% 
  mutate(teste_lb = purrr::map2(models, lag_teste, ljung_box_auto)) %>% 
  select(-models) %>% 
  unnest(teste_lb)

teste_geral_passed <- teste_geral_lb %>% 
  dplyr::mutate(maior_p_5 = if_else(lb_pvalue >= .05, 1, 0)) %>% 
  dplyr::summarise(maior_p_5 = sum(maior_p_5),
                   .by = c(.model)) %>% 
  dplyr::filter(maior_p_5 == 10)

teste_geral_2 <- teste_geral %>% 
  dplyr::select(tidyselect::any_of(teste_geral_passed$.model))

teste_forecast <- teste_geral_2 %>% 
  forecast(new_data = filter_index(df, '2023-09' ~ '2023-11'))

teste_acc <- teste_forecast %>% 
  accuracy(df) %>% 
  arrange(MAPE)

teste_acc %>% 
  top_n(1, - MAPE)

teste_dummy_lb <- tibble::tibble(lag_teste = 1:10,
                                 models = list(teste_dummy)) %>% 
  mutate(teste_lb = purrr::map2(models, lag_teste, ljung_box_auto)) %>% 
  select(-models) %>% 
  unnest(teste_lb)

teste_dummy_passed <- teste_dummy_lb %>% 
  dplyr::mutate(maior_p_5 = if_else(lb_pvalue >= .05, 1, 0)) %>% 
  dplyr::summarise(maior_p_5 = sum(maior_p_5),
                   .by = c(.model)) %>% 
  dplyr::filter(maior_p_5 == 10)

teste_dummy_passed

liq_modelo <- modelling_complete(
  df %>%
    dplyr::filter(mes <= tsibble::yearmonth('2023-11-01')),
  var_y = 'var_y',
  var_exogena = c('contagem', 'nl'),
  train_set = '2023-08'
)

nl_mod <- modelling_complete(
  df %>%
    dplyr::filter(mes <= tsibble::yearmonth('2023-11-01')) %>% 
    select(everything(), -var_y),
  var_y = 'nl',
  dummy = 'quebra',
  train_set = '2023-08'
)


nl_for <- forecasting(nl_mod,
                      new_data = df %>%
                        dplyr::filter(mes > tsibble::yearmonth('2023-11-01')))


nl_for %>% autoplot(nl_mod$df)


liq_for <- forecasting(
  liq_modelo,
  new_data = nl_for %>%
    select(mes, var_y, nl = .mean, contagem) %>% 
    tsibble::filter_index("2023-11" ~ .)
  )

liq_for %>% 
  autoplot(df)
