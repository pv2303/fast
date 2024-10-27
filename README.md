# FAST (Forecasting and Statistical Tools)

FAST is an R script for fitting and comparing various time series forecasting models. Utilizing the fpp3 and tidyverse packages, this tool allows users to fit both univariate models and models with covariates in an automated and scalable way. It’s ideal for users who need to run multiple forecasting models to find the best predictive model for their time series data.

## Features

* Univariate Models: Quickly fit multiple univariate time series models, including ARIMA, ETS, SNAIVE, Random Walk (RW), Mean, NNETAR, and Theta.
* Covariate Models: Fit models with any combination of specified covariates, supporting flexible forecasting with covariate selection.
* Custom Forecast Horizon: Specify a forecast horizon for prediction, set to a default of 12 months.

## Usage

The FAST script provides two main functions:

1. Fitting Univariate Models: To fit a selection of univariate models to a single response variable in your dataset, use the `fit_univariate_models()` function:


```r
# Usage
fit_univariate_models(data, response)
```

Parameters:

* `data`: The input dataset.
* `response`: The name of the response variable (e.g., "sales").

<br>

2. Fitting Models with Covariates
For multivariate forecasting with selected covariates, use `fit_covariate_models()`:

```r
# Usage
fit_covariate_models(data, response, covariates, h = "12 months")
```

Parameters:

* `data`: The input dataset.
* `response`: The name of the response variable.
* `covariates`: A vector of covariate names to include in the models.
* `h`: Forecast horizon (default is "12 months").

# Examples
Below are examples demonstrating how to use `fit_univariate_models` and `fit_covariate_models`.

### Example 1: Fitting Univariate Models
```r

# Load necessary libraries
library(tidyverse)
library(fpp3)

# Prepare a sample dataset (e.g., using inbuilt `aus_retail` dataset)
data <- aus_retail %>% filter(Industry == "Food retailing")

# Fit univariate models
result <- fit_univariate_models(data, response = "Turnover")
print(result)
```

### Example 2: Fitting Models with Covariates

```r
# Load necessary libraries
library(tidyverse)
library(fpp3)

# Define dataset and covariates
data <- your_dataset
covariates <- c("covariate1", "covariate2")

# Fit covariate models with a forecast horizon of 6 months
result <- fit_covariate_models(data, response = "sales", covariates = covariates, h = "6 months")
print(result)
```
