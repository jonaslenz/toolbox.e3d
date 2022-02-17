#' reads version of E3D installation
#'
#' Returns the version number of the E3D installation given in the relief.log file in relief set created by E3D.
#' @return string E3D version number
#' @param build logical, TRUE returns a string including build information
#' @param relief.v logical, TRUE returns also version of terrain.dll
#' @param install_path, path to installation directory of EROSION-3D
#' @export
#' @importFrom ini read.ini
#' @examples
#' get_version.E3D()

get_version.E3D <- function(build = FALSE, install_path = NA, relief.v = FALSE)
{
  if(check_setup.E3D(install_path = install_path)!=0){stop("cannot access e3d instalation")}
  calc_soilset()
  reliefversion <- ini::read.ini(normalizePath(file.path(tempdir(),"model/relief/relief.log")))$Relief$Version
  e3dversion <- ini::read.ini(normalizePath(file.path(tempdir(),"model/result/result.log")))$Result$Version

  if(!build)
  {
    e3dversion <- gsub(" .*","",e3dversion)
    reliefversion <- gsub(" .*","",reliefversion)
  }
  if(relief.v)
  {
    e3dversion <- paste0(
      "E3D: ",e3dversion,
      "; ",
      "Terrain: ",reliefversion
      )
  }
  return(e3dversion);
}
