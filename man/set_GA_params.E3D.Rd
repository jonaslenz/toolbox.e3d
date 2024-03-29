% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_GA_params.E3D.R
\name{set_GA_params.E3D}
\alias{set_GA_params.E3D}
\title{calculate water content to achieve given pF for E3D}
\usage{
set_GA_params.E3D(Cl, Si, Sa, suction, ks, delta_theta)
}
\arguments{
\item{Cl, }{M-%}

\item{Si, }{M-%}

\item{Sa, }{M-%}

\item{suction}{target suction at wetting front psi}

\item{ks}{target ks}

\item{delta_theta}{target water deficit in pore space}
}
\value{
values for bulk density, corg and skinfactor to meet target values


}
\description{
Reversely calculates E3D-parameter bulk density, corg, initial moisture and skinfactor in such a way, that target Green-Ampt parameters are met when running E3D.
Depends on soil texture main classes.
}
\seealso{
psimo.E3D
}
