#' calculate THETA_S
#'
#' calculates THETA_S-parameter (maximum soil moisture) according to Vereecken
#' @inheritParams
#' @return THETA_S
#' @seealso
#' @export
#' @examples
#'

theta_s.E3D <- function(Cl,Bulk)
{
  return(0.81 - 0.283/1000*Bulk + 0.001*Cl);
}
