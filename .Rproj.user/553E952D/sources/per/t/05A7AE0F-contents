library(fpp3)
library(tidyverse)
library(lubridate)

# Structure to be modified
#
# if (is.character(train_set) == T) {
#   if (str_detect(train_set, "Q")) {
#     training <- train_set
#     testing <- lubridate::ym(train_set) %q+% quarters(1)
#   } else {
#     training <- train_set
#     testing <- lubridate::ym(train_set) %m+% months(1) %>%
#       as.character()}
# } else if (is.numeric(train_set) == T) {
#   training <- train_set
#   testing <- train_set + 1
# } else {
#   rlang::abort(
#     "the train_set arg must be a character for monthly data (YYYY-MM), quarterly data (YYYY-QQ) or numeric for yearly data"
#   )
# }

# Structure to monthly data

monthly_test <- tsibble(data = ym("2020-01") %m+% months(0:3))
monthly_test

# Trying monthly based test
quarter_test <- tsibble(data = yearquarter("2020-01") + 0:15)

quarter_test

train_set <- yearquarter("2022-01")
testing <- (train_set + 1) %>% as.character()
testing

quarter_test %>% 
  filter_index(. ~ as.character(train_set))

quarter_test %>% 
  filter_index(as.character(testing) ~ .)

