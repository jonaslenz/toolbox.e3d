#' calculation of cumulative runoff [l] from time [s] and runoff rate [l/s]
#'
#' This function calculates cummulative runoff from a timeline with runoff rates.
#'
#' @export
#' @examples
#'
calc_cumq_cumsed <- function(timesteps, qsteps)
{
  iterend <- length(timesteps)-1;
  cumqsteps <- 0;
  sumcumqsteps <- 0;
  for(x in 1:iterend)
  {
    cumqsteps[x+1] <- (timesteps[x+1]-timesteps[x])*(qsteps[x+1]+qsteps[x])/2;
    sumcumqsteps[x+1] <- sum(cumqsteps[1:x+1]);
  }

  return(cbind(cumqsteps, sumcumqsteps));
}
