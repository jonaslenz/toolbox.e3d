#' Write an synthetic EROSION-3D land classification input file
#'
#' This function writes a land-use/land-cover file for simultaneously modeling of EROSION-2D slopes in EROSION-3D.
#' One row represents one EROSION-2D slope, when flow routing is set to one neighbor in EROSION-3D.
#'
#' @param POLY_ID numeric vector, number of elements defines columns of created relief file. Refers to POLY_ID in soil_params.csv of E3D.
#' @param length numeric value, defining slope length in whole meters
#' @param path folder path in which file will be written
#' @param filename name of created file
#' @return None, file is written
#' @export
#' @importFrom raster writeRaster
#' @importFrom raster raster
#' @examples
#' write.landuse.E3D(path = "C:/E3Dmodel/")

write.landuse.E3D <- function(POLY_ID = c(1,2,3),length=50, path, filename = "/model/landuse.asc")
{
    #EROSION-3D requires at least two rows
  if (length(POLY_ID) < 2){POLY_ID <- rep(POLY_ID, each=2);}

  rows <- length(POLY_ID)

writeRaster(
  raster(nrows=rows, ncols=length, xmn=0, xmx=length, ymn=0, ymx=rows,crs="+notaCRS", vals=rep(POLY_ID, each=length)),
  filename = file.path(path,filename),
  format ="ascii",
  overwrite=TRUE,
  datatype = "INT2U"
)
}
