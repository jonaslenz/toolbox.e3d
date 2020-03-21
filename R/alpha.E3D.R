#' calculate Van-Genuchten ALPHA
#'
#' calculates alpha-parameter according to Vereecken et. al 1989 (doi:10.1097/00010694-198912000-00001)
#' for Van-Genuchten soil-water retention curve (Van-Genuchten 1980 - doi:10.2136/sssaj1980.03615995004400050002x)
#' -> first equation in Table 7 for log(alpha)
#'
#' @param Sa  Gravimetric content of sand particles in fine soil (0-100)
#' @param Cl  Gravimetric content of clay particles in fine soil (0-100)
#' @param Bulk  bulk density of dry soil [kg/m^3]
#' @param Corg  Gravimetric content of organic bound carbon in fine soil (0-100)
#' @return alpha
#' @examples
#' alpha.E3D(30,20,1300,1.3)
#' @export
alpha.E3D <- function(Sa,Cl,Bulk,Corg)
{
  return(exp(-2.486 + 0.025 * Sa - 0.351 * Corg - 2.617 / 1000 * Bulk - 0.023*Cl));
}
