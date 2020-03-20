#' calculate water content to achieve given pF for E3D
#'
#' calculates water content from given pF-value (matrix potential).
#' a reverse function of psimo.E3D
#'
#' @param THETA_S Maximum volumetric water content (0-100)
#' @param THETA_R Minimum volumetric water content (0-100)
#' @param pF target pF value
#' @param ALPHA Van-Genuchten parameter
#' @param NORDPOL Van-Genuchten parameter
#' @param digits integer, number of decimal digits
#' @return moist
#' @seealso psimo.E3D
#' @export

pF_to_moist.E3D <- function(THETA_S,THETA_R, pF, ALPHA,NORDPOL, digits = 2)
{
  target_Psi <- 10^(pF)
  return(round(((THETA_S - THETA_R) /((target_Psi)^NORDPOL * ALPHA^NORDPOL + 1) + THETA_R)*100, digits));
  ## alternative calculation using root function - in case psimo.E3D changes
  #f1 <- function(x){psimo.E3D(THETA_S = THETA_S,THETA_R = THETA_R,NORDPOL = NORDPOL,ALPHA = ALPHA,INITMOIST = x)-target_Psi}
  #a$INITMOIST2[i] <- round(uniroot(f1,lower=THETA_R*100+0.01,upper = THETA_S*100-0.01)$root,2)
}
