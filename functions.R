

# Estimator =====================================



#' Estimate the conditional mean of Y given X = x0
#' 
#' @param vec_x observed data of X
#' @param vec_y observed data of Y
#' @param h the bandwidth used for kernel smoothing
#' @param x0 the point at which the estimation is done
#' 
#' @return an estimator of the conditional mean
#'  
EstimCondMean <- function(vec_x, vec_y, h, x0)
{
  diff_x = (vec_x - x0)
  weights = (1/h) * exp( - (diff_x / h)^2 / 2)
  
  estimator = sum(vec_y * weights) / sum(weights)
  return(estimator)
}

