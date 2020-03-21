#' calculate THETA_S
#'
#' calculates THETA_S-parameter (maximum soil moisture) according to Vereecken et. al 1989 (doi:10.1097/00010694-198912000-00001)
#' -> equation in Table 7
#' @param Cl  Gravimetric content of clay particles in fine soil (0-100)
#' @param Bulk  bulk density of dry soil [kg/m^3]
#' @return THETA_S
#' @export
#' @examples
#' theta_s.E3D(20,1300)

theta_s.E3D <- function(Cl,Bulk)
{
  return(0.81 - 0.283/1000*Bulk + 0.001*Cl);
}
