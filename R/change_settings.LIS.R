#' change model settings for openLISEM
#'
#' This function changes the settings for openLISEM models in the *.run file
#'
#' @param module character vector, defines sub module in which setting will be changed
#' @param setting character vector, defines setting to be changed
#' @param value character vector, defines value to be changed to
#' @param path folder path in which file will be written
#' @param filename name of created file, should be *.run
#' @param setpath boolean, shortcut to set working directory for E3D - if TRUE the path will be written as project path for rain, DEM, soil and result
#' @export
#' @return none, *.run file is written
#' @importFrom ini read.ini
#' @importFrom ini write.ini
#'

change_settings.LIS <- function(path = NA, filename = "lisem/run.par", module = NA, setting = NA, value = NA, setpath = TRUE)
{
  standard_ini <- ini::read.ini(system.file("lisem683.run", package = "toolbox.e3d"))

  if(!missing(setting) & !missing(value) & !missing(module) & length(module) ==length(setting) &length(setting)==length(value))
  {
    for (i in 1:length(setting))
    {
      standard_ini[[module[i]]][[setting[i]]] <- as.character(value[i])
    }
  }

  if(setpath)
  {
    standard_ini[["Input"]][["Map Directory"]] <- normalizePath(file.path(path,"lisem/"))
    standard_ini[["Input"]][["Rainfall Directory"]] <- normalizePath(file.path(path,"lisem/"))
    standard_ini[["Input"]][["Rainfall Map Directory"]] <- normalizePath(file.path(path,"lisem/"))
    standard_ini[["Output"]][["Result Directory"]] <- normalizePath(file.path(path,"lisem/output/"))
  }

  ini::write.ini(standard_ini,filepath = file.path(path,filename))
}
