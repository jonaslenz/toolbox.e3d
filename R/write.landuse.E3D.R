#' Write an synthetic EROSION-3D landclassification input file
#'
#' This function writes a landuse/landcover file for simultaneously modelling of EROSION-2D slopes in EROSION-3D.
#' One row represents one homogeneous soil parameter set.
#' Length defines slope length.
#' @inheritParams
#' @return No return value, but file is written
#' @seealso
#' @export
#' @examples
#' write.landuse.E3D(path = "C:/")

write.landuse.E3D <- function(POLY_ID = c(1,2,3),length=50, path, filename = "/model/soil/landuse.asc")
{
require(raster)
    #EROSION-3D requires at least two rows
  if (length(POLY_ID) < 2){POLY_ID <- rep(POLY_ID, each=2);}

  rows <- length(POLY_ID)
writeRaster(
  raster(nrows=rows, ncols=length, xmn=0, xmx=length, ymn=0, ymx=rows,crs="+notaCRS", vals=rep(POLY_ID, each=length)),
  filename = paste0(path,filename),
  format ="ascii",
  overwrite=TRUE,
  datatype = "INT2U"
)
}
