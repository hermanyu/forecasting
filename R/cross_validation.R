library(dplyr)

fit_model_on_training_period <- function(model_fitter, data, start_time, end_time){
  training_data <- data %>% 
    filter(
      time >= start_time,
      time <= end_time
    )
  
  return(model_fitter(training_data))
}

get_residuals_for_validation_time <- function(model, predicter, data, validation_time){
  
  validation_data <- data %>% 
    filter(
      time == validation_time
    )
  
  predictions <- predicter(model, validation_data)
  
  actuals <- validation_data$y
  
  return(actuals - predictions)
}


get_residuals_for_rolling_validation_period <- function(model_fitter, predicter, data,
                                                        training_start_time, training_end_time,
                                                        validation_times){
  
  residuals <- c()
  model <- fit_model_on_training_period(model_fitter, data, training_start_time, training_end_time)
  
  for (i in seq(1, length(validation_times))){
    residuals <- c(
      residuals,
      get_residuals_for_validation_period(
        model, 
        predicter, 
        data, 
        validation_times[i]
      )
    )
    if (i != length(validation_times)){
      model <- fit_model_on_training_period(model_fitter, data, training_start_time, validation_times[i])
    }
  }
  
  return(residuals)
}
