#' calculate ALPHA
#'
#' calculates alpha-parameter accordng to Vereecken
#' @inheritParams str_detect
#' @return alpha
#' @seealso
#' @export
#' @examples
#'

alpha.E3D <- function(Sa,Cl,Bulk,Corg)
{
  return(exp(-2.486 + 0.025 * Sa - 0.351 * Corg - 2.617 / 1000 * Bulk - 0.023*Cl));
}
