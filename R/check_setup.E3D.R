#' check e3d setup
#'
#' Internal function, use get_version.E3D() instead.
#' Checks, if e3d is available from global %PATH%, given path or can be found in registry.
#' If registry entry is found, it will be added to PATH for this session.
#' @param install_path, path to installation directory of EROSION-3D
#' @export
#'
#' @return none
check_setup.E3D <- function(install_path = NA)
{
  # get current environment
  old_PATH <- Sys.getenv("PATH")

  # paste manual install path, if provided
  if(!is.na(install_path) & is.character(install_path))
  {
    if(grepl("Erosion-3D",old_PATH))
    {message("EROSION-3D allready in system %PATH% - please check version, which gets used.")}
    install_path <- normalizePath(install_path)
    Sys.setenv(PATH = paste(old_PATH, install_path, sep = ";"))
  }

  # search for install path in registry, if no path is provided and not found in PATH
  if(is.na(install_path) & !grepl("Erosion-3D",old_PATH))
  {
    message("e3d not found in system %PATH% - checking for instalation in registry and adding to %PATH% for this session")
    # get installation path
    aa <- system2("reg", "query HKLM\\Software\\ /s /f Erosion-3D /k /v InstallDir", stdout = TRUE)
    if(is.character(aa)){
      aa <- aa[grep("InstallDir",aa)]
      instdir <- substr(aa,regexec(":",aa)[[1]][1]-1,nchar(aa))
      Sys.setenv(PATH = paste(old_PATH, instdir, sep = ";"))
    }else{stop("Could not find installed version of EROSION-3D in Registry. Please check instalation and run again or add e3d to %PATH% manually.")}

    message(paste("Please consider adding e3d instalation directory (",instdir,") to %PATH% in system environment to save time at this step."))
  }
  return(0)
}
