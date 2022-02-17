#' calculate water content to achieve given pF for E3D
#'
#' Reversely calculates E3D-parameter bulk density, corg, initial moisture and skinfactor in such a way, that target Green-Ampt parameters are met when running E3D.
#' Depends on soil texture main classes.
#'
#' @param Cl, M-%
#' @param Si, M-%
#' @param Sa, M-%
#' @param suction target suction at wetting front psi
#' @param ks target ks
#' @param delta_theta target water deficit in pore space
#' @return values for bulk density, corg and skinfactor to meet target values
#' @return
#' @seealso psimo.E3D
#' @export

set_GA_params.E3D <- function(Cl, Si, Sa, suction, ks, delta_theta)
{
 # Cl = 30
 # Si = 40
 # Sa = 30
 # suction = 30
 # ks = 0.005
 # delta_theta = 14

  # delta_theta is used as forcing parameter to define initmoist in dependency to calcuted theta_s

  # define function, which can be used for optimization of bulk density and corg to achive desired psi
  calc_dt_psi_opti <- function(x, target_psi, d_theta)
  {
    # rescale to match scale of corg for better optimization results
    x[1] <- x[1]*1000
    nord <- toolbox.e3d::nordpol.E3D(Sa = Sa, Cl = Cl)
    theta_r <- toolbox.e3d::theta_r.E3D(Cl = Cl, Corg = x[2])
    theta_s <- toolbox.e3d::theta_s.E3D(Cl = Cl, Bulk = x[1])
    initmoist <- theta_s*100 - d_theta
    if(initmoist/100 <= theta_r){return(1000000)}
    alpha <- toolbox.e3d::alpha.E3D(Sa = Sa, Cl = Cl, Bulk = x[1], Corg = x[2])
    psi <- toolbox.e3d::psimo.E3D(THETA_S = theta_s, THETA_R = theta_r, INITMOIST = initmoist, ALPHA = alpha, NORDPOL = nord)

    result <- (target_psi - psi)^2
    return(result)
  }


  # use optimization function https://www.rdocumentation.org/packages/optimx/versions/2021-10.12/topics/optimx  - Method may be changed in further versions
  aa <- optimx::optimx(par = c(1.3,1.0),fn = calc_dt_psi_opti, d_theta = delta_theta, target_psi = suction
                       , method = "L-BFGS-B", lower = c(0.8,0), upper = c(2.3,8)
                       #, method = "Nelder-Mead"
                 )

  if(aa$convcode != 0)
  {warning(paste0("Optimization did not converge - optimx code: ",aa$convcode))}


  # define function, which can evaluate psi value optimized parameters bulk density and corg
  calc_dt_psi <- function(x, d_theta, initmoist)
  {
    nord <- toolbox.e3d::nordpol.E3D(Sa = Sa, Cl = Cl)
    theta_r <- toolbox.e3d::theta_r.E3D(Cl = Cl, Corg = x[2])
    theta_s <- toolbox.e3d::theta_s.E3D(Cl = Cl, Bulk = x[1])
    alpha <- toolbox.e3d::alpha.E3D(Sa = Sa, Cl = Cl, Bulk = x[1], Corg = x[2])
    psi <- toolbox.e3d::psimo.E3D(THETA_S = theta_s, THETA_R = theta_r, INITMOIST = initmoist, ALPHA = alpha, NORDPOL = nord)
    return(psi)
  }

  ### round to accuracy available in EROSION-3D

  aa$p1 <- round(aa$p1*1000,0)
  aa$p2 <- round(aa$p2,2)

  bulk <- aa$p1
  corg <- aa$p2
  theta_s <- toolbox.e3d::theta_s.E3D(Cl = Cl, bulk)
  initmoist <- round(theta_s*100 - delta_theta, 1)

  D = exp(Cl / 100 * log(0.001) + Si / 100 * log(0.026) + Sa / 100 * log(1.025))
  SigP2 = exp(sqrt( Cl / 100 * log(0.001)^2 + Si / 100 * log(0.026)^2 + Sa / 100 * log(1.025)^2 - log(D)^2))

  b <- D ^ (-0.5) + 0.2 * SigP2
  kscampbell <- 0.004 * (1.3 / (bulk / 1000)) ^ (1.3 * b) * exp(-0.069 * Cl - 0.037 * Si)
  skin <- ks/(kscampbell*9.81e-3)


  opti_psi <- calc_dt_psi(c(aa$p1,aa$p2), delta_theta, initmoist)

  if((opti_psi-suction)^2 > 1)
  {warning("Please check optimized Psi - larger differences to target value from rounding.")}

  result <- cbind.data.frame("bulk" = bulk,
                             "corg" = corg,
                             "initmoist"=initmoist,
                             "skin" = skin,
                             "opti_psi" = opti_psi,
                             "theta_s" = theta_s)

  return(result)
}
