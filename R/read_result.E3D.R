#' read E3D result set
#'
#' Internal function to read result set of E3D
#'
#' @param result_layer, output layer of EROSION-3D
#' @param modelpath, path to calculated model folder
#' @export
#'
#' @return none
read_result.E3D <- function(result_layer , modelpath = tempdir())
{
  if (!file.exists(file.path(modelpath, paste0("model/result/",result_layer,".asc")))) {
    if (!file.exists(file.path(modelpath, paste0("model/result/",result_layer,".sdat")))) {
      if (!file.exists(file.path(modelpath, paste0("model/result/",result_layer,".tif")))) {
        stop("Can't read result set. Please check that standard output files in E3D are either *.asc, *.tif or *.sdat.")
      } else {
        result <- raster::raster(file.path(modelpath, paste0("model/result/",result_layer,".tif")))
      }
    } else {
      result <- raster::raster(file.path(modelpath, paste0("model/result/",result_layer,".sdat")))
    }
  } else {
    result <- raster::raster(file.path(modelpath, paste0("model/result/",result_layer,".asc")))
  }
  return(result)
}
