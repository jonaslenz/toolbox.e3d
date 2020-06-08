#' reads version of E3D installation
#'
#' Returns the version number of the E3D installation given in the relief.log file in relief set created by E3D.
#' @return string E3D version number
#' @export
#' @importFrom ini read.ini
#' @examples
#' get_version.E3D()

get_version.E3D <- function()
{
  if(check_setup.E3D()!=0){stop("cannot access e3d instalation")}
  path = create_folders.E3D(path = paste0(tempdir(),"\\version"), overwrite = TRUE)
  system2("e3d", paste0('/r "',normalizePath(file.path(path,"run.par")),'"'), wait=TRUE)
  version <- ini::read.ini(normalizePath(file.path(path,"relief/relief.log")))$Relief$Version
  return(version);
}
