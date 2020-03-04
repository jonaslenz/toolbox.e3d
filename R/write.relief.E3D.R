#' Write an synthetic EROSION-3D relief input file
#'
#' This function writes a relief file for simultaneously modelling of EROSION-2D slopes in EROSION-3D.
#' One row represents one EROSION-2D slope.
#' Length defines slope length.
#' Slope is given in %.
#' @inheritParams
#' @return No return value, but file is written
#' @seealso
#' @export
#' @examples
#' write.relief.E3D(path = "C:/")

write.relief.E3D <- function(POLY_ID = c(1,2,3),length=50,slope = 11, path, filename = "dem.asc")
{
require(raster)
  #EROSION-3D requires at least two rows
  if (length(POLY_ID) < 2){POLY_ID <- rep(POLY_ID, each=2);}

  rows <- length(POLY_ID)

writeRaster(
  raster(nrows=rows, ncols=length, xmn=0, xmx=length, ymn=0, ymx=rows,crs="+notaCRS", vals=rep((1:length)*slope/100, times=rows)),
  filename = paste0(path,"dem.asc"),
  format ="ascii",
  overwrite=TRUE
)
}
