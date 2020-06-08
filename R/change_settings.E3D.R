#' change model settings for E3D
#'
#' This function changes the settings for E3D models in the *.par file
#'
#' @param module character vector, defines sub module in which setting will be changed
#' @param setting character vector, defines setting to be changed
#' @param value character vector, defines value to be changed to
#' @param path folder path in which file will be written
#' @param filename name of created file, should be *.par
#' @param setpath boolean, shortcut to set working directory for E3D - if TRUE the path will be written as project path for rain, DEM, soil and result
#' @export
#' @return none, *.par file is written
#' @importFrom ini read.ini
#' @importFrom ini write.ini
#'

change_settings.E3D <- function(path = NA, filename = "model/run.par", module = NA, setting = NA, value = NA, setpath = TRUE)
{
  standard_ini <- ini::read.ini(system.file("run.par", package = "toolbox.e3d"))

  if(!missing(setting) & !missing(value) & !missing(module) & length(module) ==length(setting) &length(setting)==length(value))
  {
    for (i in 1:length(setting))
    {
      standard_ini[[module[i]]][[setting[i]]] <- as.character(value[i])
    }
  }

  if(setpath)
  {
    standard_ini[["Meteo"]][["Rain"]] <- normalizePath(file.path(path,"model/rain_e3d.csv"))
    standard_ini[["Relief_Hydro"]][["Relief"]] <- paste0(normalizePath(file.path(path,"model/relief")),"\\")
    standard_ini[["Relief_Hydro"]][["DEM"]] <- normalizePath(file.path(path,"model/dem.asc"))
    standard_ini[["Soil_landuse"]][["Soil"]] <- paste0(normalizePath(file.path(path,"model/soil")),"\\")
    standard_ini[["Soil_landuse"]][["rdb_dat"]] <- paste0(normalizePath(file.path(path,"model/soil_params.csv")))
    standard_ini[["Soil_landuse"]][["rdb_grd"]] <- paste0(normalizePath(file.path(path,"model/landuse.asc")))
    standard_ini[["Result"]][["Result"]] <- paste0(normalizePath(file.path(path,"model/result")),"\\")
  }

  ini::write.ini(standard_ini,filepath = file.path(path,filename))
}
