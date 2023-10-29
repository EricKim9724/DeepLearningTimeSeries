# Author: Eric Kim
# Date: 20/07/2023
# Functions to fit arfima model on datasets

# load packages
library(tidyverse)
library(fpp3)
library(forecast)
library(imputeTS)

#' Process data from a CSV file and impute missing values
#'
#' This function reads a CSV file, adds an index, and imputes missing values for a specified target variable.
#'
#' @param file_name The name of the CSV file.
#' @param target_variable The name of the target variable for which missing values will be imputed.
#'
#' @return A data frame with the processed data including the imputed values.
#' 
dataPreprocess <- function(file_name, target_variable) {
  # Import data from CSV
  data <- read.csv(file_name) 
  data()$index <- 1:nrow(data)
  
  # Impute missing data
  data$imputed_target <- na_interpolation(data[[target_variable]], option = "linear")
  
  return(data)
}

#' Split a dataframe into training and testing data
#'
#' This function splits the input dataframe into training and testing data based on the specified number of rows.
#'
#' @param data The dataframe to be split.
#' @param num_valid The number of rows to allocate to the validation data
#'
#' @return A list containing the training and testing dataframes.
#'
split_data <- function(data, num_valid) {
  training_data <- data %>% 
    dplyr::filter(index <= nrow(data) - split_rows)
  
  testing_data <- data %>% 
    dplyr::filter(index > nrow(data) - split_rows)
  
  return(list(training_data = training_data, testing_data = testing_data))
}

#' Fit ARFIMA model and produce forecasts
#'
#' This function fits an ARFIMA model to the input data and produces long term forecasts and 1-step rolling forecasts.
#'
#' @param data The input data to be used for fitting the ARFIMA model.
#' @param ic Information criterion used for model selection.
#' @param window The number of steps to forecast into the future.
#'
#' @return A list containing the fitted ARFIMA model, long term forecast, and 1-step rolling forecast.
#'
arfima_forecast <- function(training_data, data, ic, window) {
  # Fit ARFIMA model
  model <- training_data %>%
    arfima(drange = c(0, 0.5), estim = c("mle"), ic = ic)
  
  # Produce long term forecast
  long_forecast <- forecast(model, h = window)$mean 
  
  # 1-step Rolling forecasts
  rolling_forecast <- arfima(data,model = model)
  rolling_forecast <- tail(rolling_forecast$fitted, window)
  return(list(model = model,long_forecast=long_forecast,rolling_forecast=rolling_forecast))
}
