#' calculate Nordpol
#'
#' calculates n-parameter according to Vereecken
#' @inheritParams str_detect
#' @return nordpol
#' @seealso
#' @export
#' @examples
#'

nordpol.E3D <- function(Sa,Cl)
{
  return(exp(0.053 - 0.009 * Sa - 0.013 * Cl + 0.00015 * Sa^2));
}
