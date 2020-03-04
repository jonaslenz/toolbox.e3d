#' calculate THETA_R
#'
#' calculates THETA_R-parameter (minimum soil moisture) according to Vereecken
#' @inheritParams
#' @return THETA_R
#' @seealso
#' @export
#' @examples
#'

theta_r.E3D <- function(Cl,Corg)
{
  return(0.015 + 0.005*Cl + 0.014*Corg);
}
