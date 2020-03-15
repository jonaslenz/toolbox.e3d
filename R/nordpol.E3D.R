#' calculate Van-Genuchten n
#'
#' calculates n-parameter according to Vereecken et. al 1989 (doi:10.1097/00010694-198912000-00001)
#' for Van-Genuchten soil-water retention curve (Van-genuchten 1980 - doi:10.2136/sssaj1980.03615995004400050002x)
#' -> first equation in Table 7 for log(n)
#'
#' @param Sa  Gravimetrical content of sand particles in fine soil (0-100)
#' @param Cl  Gravimetrical content of clay particles in fine soil (0-100)
#' @return n
#' @examples
#' nordpol.E3D(30,20)
#' @export
#'
#' @export

nordpol.E3D <- function(Sa,Cl)
{
  return(exp(0.053 - 0.009 * Sa - 0.013 * Cl + 0.00015 * Sa^2));
}
