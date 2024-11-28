library(dplyr)
library(lubridate)

generate_ar1_vector <- function(num_points = 50, y_start = 0, 
                                 epsilon_mean = 0, epsilon_sd = 1, 
                                 phi = NA, phi_low = -0.99, phi_high = 0.99, 
                                 seed = NA){
  if (!is.na(seed)){
    set.seed(seed)
  }
  
  if (is.na(phi)){
    phi <- runif(1, phi_low, phi_high)
  }
  
  epsilon <- rnorm(num_points, epsilon_mean, epsilon_sd)
  y_prev <- y_start
  
  y <- c()
  
  for (i in seq(1, num_points)){
    y_current <- (y_prev * phi) + epsilon[i]
    y <- c(y, y_current)
    
    y_prev <- y_current
  }
  return(y)
  
}