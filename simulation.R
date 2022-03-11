
# 1- Loading the libraries ==================================
library(pbapply)
source("functions.R")


# 2- Creating the file if it doesn't exists =================
nameFile = "simus_1.csv"

if (!file.exists(nameFile)){
  
  caracDGP = c("n", "modelName", "mean_x", "sd_x", "sd_epsilon")
  caracEstimation = c("x0", "trueCondMean")
  caracEstimator = c("h")
  caracResult = c("estimError", "computationTime")
  
  write.table(
    x = paste0(c(caracDGP, caracEstimation,
                 caracEstimator, caracResult), collapse=";") ,
    
    file = nameFile,
    append = F, sep = ";", col.names = FALSE, row.names = FALSE,
    quote = FALSE)
}


# 3- Characteristics of the simulation  =====================

n = 50000

# Data-generating process:
modelName = "Y = X^2 + epsilon"
mean_x = 5
sd_x = 2
sd_epsilon = 0.1


# 4- Characteristics of the estimation ======================

grid_h = c(0.001, 0.002, 0.005,
           0.01, 0.02, 0.05,
           0.1, 0.2, 0.5)
x0 = 1


# 5-  Simulations ===========================================

Nreplications = 100
number_steps = Nreplications * length(grid_h)
  
pb = pbapply::startpb(min = 0, max = number_steps)
i_step = 0
for (iReplication in 1:Nreplications)
{
  for (i_h in 1:length(grid_h))
  {
    h = grid_h[i_h]
    
    ## Generating data ---------------------------------------
    
    vec_x = rnorm(n = n, mean = mean_x, sd = sd_x)
    epsilon = rnorm(n = n, mean = 0, sd = sd_epsilon)
    vec_y = vec_x^2 + epsilon
    
    trueCondMean = x0^2
    
    ## Estimation --------------------------------------------
    
    time1 = proc.time()
    estimatedCondMean = EstimCondMean(vec_x = vec_x, vec_y = vec_y,
                                      h = h, x0 = x0)
    time2 = proc.time()
    computationTime = (time2 - time1)[3]
    
    ## Storing the result ------------------------------------
    
    estimError = estimatedCondMean - trueCondMean
    
    toWrite1 = c(n, modelName, mean_x,  sd_x, sd_epsilon,
                 x0, trueCondMean,
                 h,
                 estimError, computationTime)
    
    write.table(
      x = matrix(toWrite1, nrow = 1) ,
      file = nameFile ,
      append = T, sep = ";", col.names = FALSE, row.names = FALSE
    )
    
    i_step = i_step + 1
    pbapply::setpb(pb, i_step)
  }
}

pbapply::closepb(pb)

