

# Loading of the libraries
library(tidyverse)

# Specifying the files to be load
filenames = c("simus_1.csv")

# Reading the data
totalData = filenames %>%
  map( function(x){return(if(file.exists(x)){x} else {NULL})} ) %>%
  unlist() %>%
  map_dfr( read.csv, sep = ";", dec = ".", header = TRUE)


# Summarizing the simulation
# to create the mean squared error (MSE)
# and mean computation time
summarisedData = totalData %>%
  group_by(n, modelName, mean_x, sd_x, sd_epsilon, x0, h) %>%
  summarise(MSE = mean(estimError^2),
            Bias = mean(estimError),
            Sd_MSE = sd(estimError^2, na.rm = TRUE),
            
            nReplications = n(),
            
            MSE_q05 = MSE - 1.96 * Sd_MSE / nReplications,
            MSE_q95 = MSE + 1.96 * Sd_MSE / nReplications,
            
            meanComputationTime = mean(computationTime),
            sdComputationTime = sd(computationTime),
            
            .groups = "keep") %>%
  ungroup()

