#' Samples selected quantile of continuous variable
#' @export
#' @param y numeric vector
#' @param qntl upper and lower quantile to sample

sample.cont <- function(y,qntl){

  # throw error if quantile specified is >50%
  # if(qntl>0.5) stop("Argument 'qntl' should be less than 0.5.")
  # throw error if specified x vector is not continuous
  # if(x!="continuous") stop("Argument 'x' should be 'continuous'.")

  N = length(y)
  n = floor(N/(qntl*2))    # number of subjects


  # y1 and y2 are cutoffs for extreme value sampling process

  q1 = qntl;   # quantile 1 for extremely low
  q2 = 1-qntl;   # quantile 2 for extremely high
  y1 = quantile(y,q1)
  y2 = quantile(y,q2)

  # extreme value sampling design
  data = data.frame(x)
  ok = which(data$x>y2|data$x<y1)
  data.mat = as.matrix(data[ok,])

  return(data.mat)

}




# need to extend to unbalanced sampling
