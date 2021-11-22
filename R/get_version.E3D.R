#' reads version of E3D installation
#'
#' Returns the version number of the E3D installation given in the relief.log file in relief set created by E3D.
#' @return string E3D version number
#' @param build logical, TRUE returns a string including build information
#' @param install_path, path to installation directory of EROSION-3D
#' @export
#' @importFrom ini read.ini
#' @examples
#' get_version.E3D()

get_version.E3D <- function(build = FALSE, install_path = NA)
{
  if(check_setup.E3D(install_path = install_path)!=0){stop("cannot access e3d instalation")}
  path = create_folders.E3D(path = paste0(tempdir(),"\\version"), overwrite = TRUE)
  system2("e3d", paste0('/r "',normalizePath(file.path(path,"run.par")),'"'), wait=TRUE)
  version <- ini::read.ini(normalizePath(file.path(path,"relief/relief.log")))$Relief$Version

  if(!build)
  {
    version <- gsub(" .*","",version)
  }
  return(version);
}
