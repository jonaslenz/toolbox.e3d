#' calculation of cumulative runoff [l] from time [s] and runoff rate [l/s]
#'
#' Technically this returns the number of "code points", in a string. One
#' code point usually corresponds to one character, but not always. For example,
#' an u with a umlaut might be represented as a single character or as the
#' combination a u and an umlaut.
#'
#' @inheritParams str_detect
#' @return A numeric vector giving number of characters (code points) in each
#'    element of the character vector. Missing string have missing length.
#' @seealso [stringi::stri_length()] which this function wraps.
#' @export
#' @examples
#' str_length(letters)
#' str_length(NA)
#' str_length(factor("abc"))
#' str_length(c("i", "like", "programming", NA))
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
