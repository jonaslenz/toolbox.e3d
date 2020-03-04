#' calculate matrix potential according to Vereecken
#'
#' calculates matrix potential PsimO [hPa] according to Vereecken
#' @inheritParams
#' @return psimo
#' @seealso
#' @export
#' @examples
#'

psimo.E3D <- function(THETA_S,THETA_R, INITMOIST, ALPHA,NORDPOL)
{
  return((((THETA_S - THETA_R) / (INITMOIST/100 - THETA_R) - 1) / ALPHA ^ NORDPOL) ^ (1/NORDPOL));
}
