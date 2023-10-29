# Author: Eric Kim
# Date: 20/07/2023
# Fit an ARFIMA model and export the results

# Required Packages
packages <- c("tidyverse", "fpp3", "imputeTS","forecast")

# Install packages and load
new_packages <- packages[!(packages %in% installed.packages())] 

sapply(packages, require, character.only = TRUE)

source("arfima_residual")

# List all the files in the directory with a specific file extension (e.g., .csv)
file_list <- list.files(path = "./raw_data",pattern = "\\.csv$")

target_variable <- "Close"
forecast_window <- 100

# Iterate over the files in the data folder
for (file in file_list){
  data <- dataPreprocess(paste0("./raw_data/", file),target_variable)
  split_data<-split_data(data,forecast_window)
  training_data <-split_data[[1]] 
  aic <- arfima_forecast(training_data,data,"aic",forecast_window)
  bic <- arfima_forecast(training_data,data,"bic",forecast_window)
  data_export <- data.frame(matrix(nrow=length(data)))
  data_export$actual <- data[[target_variable]]
  data_export <- data_export %>% 
    mutate(aic_fit = c(fitted(aic_model),replicate(forecast_window, NA)),
           bic_fit = c(fitted(bic_model), replicate(forecast_window, NA)),
           l_aic = c(replicate(length(training_data), NA), aic[[2]]),
           l_bic = c(replicate(length(training_data), NA), bic[[2]]),
           r_aic = c(replicate(length(training_data), NA), aic[[3]]),
           r_bic = c(replicate(length(training_data), NA), bic[[3]]))
  # Export data
  write.csv(data_export, file = paste0("./processed_data/",file, "_export"), row.names = FALSE)
}
