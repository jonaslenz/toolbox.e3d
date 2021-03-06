% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write.rainfile.E3D.R
\name{write.rainfile.E3D}
\alias{write.rainfile.E3D}
\title{Write an EROSION-3D rainfall intensity input file}
\usage{
write.rainfile.E3D(
  time,
  intens,
  path,
  date = "1990-01-01",
  filename = "/model/rain_e3d.csv",
  timestep = 60
)
}
\arguments{
\item{time}{numeric vector, containing timestamps in seconds when intensity changes, must be multiple of parameter timestep}

\item{intens}{numeric vector, gives rainfall intensity value valid from corresponding timestamp in mm/min}

\item{path}{folder path in which file will be written}

\item{date}{date to be written in file output}

\item{filename}{name of created file}

\item{timestep}{time resolution in seconds written to output file, at current stage E3D supports resolution in multiples of 60 seconds}
}
\value{
None, file is written
}
\description{
This function writes a rainfall intensity input file, which can be used as input in EROSION-3D.
Rainfall intensity value in mm/min is valid from corresponding timestamp.
Last timestamp defines starting time of last interval of timestep length.
}
\examples{
write.rainfile.E3D(time=c(0,60,120,240,600),intens=c(0.1,0.6,1.2,2.4,0), path = "C:/E3Dmodel/", date = "1990-10-27", filename = "/model/rain_e3d.csv", timestep = 60)
write.rainfile.E3D(time=c(0,3600), intens=c(0.6,0), path = "C:/E3Dmodel/", date = "1990-10-27", filename = "/model/rain_e3d.csv", timestep = 60)
write.rainfile.E3D(time=c(0,60,1200,240,600),intens=c(0.1,0.6,1.2,2.4,0), path = "C:/E3Dmodel/", date = "1990-10-27", filename = "/model/rain_e3d.csv", timestep = 60)
}
