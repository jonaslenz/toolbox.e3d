#' calculate  matrix potential
#'
#' calculates matrix potential PsimO [hPa] according to Van-Genuchten 1980 - doi:10.2136/sssaj1980.03615995004400050002x
#' as cited in Schmidt 1996 (ISBN 978-3-88009-062-0)
#'
#' @param THETA_S Maximum volumetric water content (0-100)
#' @param THETA_R Minimum volumetric water content (0-100)
#' @param Initmoist actual volumetric water content (0-100)
#' @param ALPHA Van-Genuchten parameter
#' @param NORDPOL Van-Genuchten parameter
#' @return psimo
#' @seealso alpha.E3D, nordpol.E3D, theta_s.E3D, theta_r.E3D
#' @export

psimo.E3D <- function(THETA_S,THETA_R, INITMOIST, ALPHA,NORDPOL)
{
  return((((THETA_S - THETA_R) / (INITMOIST/100 - THETA_R) - 1) / ALPHA ^ NORDPOL) ^ (1/NORDPOL));
}
