#' calculate THETA_R
#'
#' calculates THETA_R-parameter (minimum soil moisture) according to Vereecken et. al 1989 (doi:10.1097/00010694-198912000-00001)
#' -> equation in Table 7
#' @param Cl  Gravimetric content of clay particles in fine soil (0-100)
#' @param Corg Gravimetric content of organic bound carbon in fine soil (0-100)
#' @return THETA_R
#' @export
#' @examples
#' theta_r.E3D(20,1.3)

theta_r.E3D <- function(Cl,Corg)
{
  return(0.015 + 0.005*Cl + 0.014*Corg);
}
