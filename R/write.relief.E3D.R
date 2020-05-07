#' Write an synthetic EROSION-3D relief input file
#'
#' This function writes a relief file for simultaneously modeling of independent hill slopes (EROSION-2D) in EROSION-3D.
#' One row represents one EROSION-2D slope, when flow routing is set to one neighbor in EROSION-3D.
#'
#' @param POLY_ID numeric vector, number of elements defines columns of created relief file. Refers to POLY_ID in soil_params.csv of E3D.
#' @param length numeric value, defining slope length in whole meters
#' @param slope numeric value, defining slope inclination in whole percent
#' @param path folder path in which file will be written
#' @param filename name of created file
#' @param resolution set spatial resoultion
#' @return None, file is written
#' @importFrom raster writeRaster
#' @importFrom raster raster
#' @export
#' @examples
#' write.relief.E3D(POLY_ID = c(1,2,3),length = 50,slope = 11, path = "C:/E3Dmodel/", filename = "dem.asc")

write.relief.E3D <- function(POLY_ID, length, slope, path, filename = "dem.asc", resolution = 1)
{
  #EROSION-3D requires at least two rows
  if (length(POLY_ID) < 2){POLY_ID <- rep(POLY_ID, each=2);}
  if(!(length%%resolution==0 || abs((length%%resolution)/resolution-1) < 0.00001)){stop("length must be a multiple of resolution (default resolution = 1).")}

  rows <- length(POLY_ID)

writeRaster(
  raster(resolution = resolution, xmn=0, xmx=length, ymn=0, ymx=rows*resolution,crs="+notaCRS", vals=rep((1:(length/resolution))*slope/100*resolution, times=rows)),
  filename = file.path(path,filename),
  format ="ascii",
  overwrite=TRUE
)
}
