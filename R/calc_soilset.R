#' evaluate soil parameters on hillslopes in EROSION-3D
#'
#' This function takes a dataframe including all soil parameters of EROSION-3D and models one hillslope for each of given parameter sets.
#' It returns a dataframe with POLY_ID as identifier and calculated cumulative runoff [litres] and soil loss [kg].
#'
#' @param soils dataframe, holding all EROSION-3D soil parameters in named columns
#'
#' @param plotlength integer value, length of experimental plot, needs to be an integer due to spatial resolution of 1 meter in E3D
#' @param resolution set spatial resoultion
#' @param slope integer value, mean slope of experimental plot in percent
#' @param intensity numeric vector, rainfall intensity of preceding time interval - corespondeces to endmin
#' @param endmin numeric vector, duration since start of rainfall experiment in full minutes, length must equal length of intensity
#' @param ponding logical TRUE means ponding option is used, FALSE - is not used in E3D, ponding limits amount of infiltrating water to available water
#' @param path path to modeling directory, default is a temporary directory
#' @importFrom raster raster
#' @importFrom utils read.csv
#' @export
#' @examples calc_soilset(soil_params)
#'

calc_soilset <- function(soils = dummy_soilset(), intensity = 0.7, plotlength = 22, slope = 9, endmin = 30, resolution = 1, ponding = FALSE, path = tempdir())
{
  nms <- c("POLY_ID" ,"BLKDENSITY", "CORG", "INITMOIST", "FT", "MT", "GT", "FU", "MU", "GU", "FS", "MS", "GS", "SKINFACTOR", "ROUGHNESS", "COVER")
  Missing <- setdiff(nms, names(soils))  # Find names of missing columns
  if (length(Missing>0)){stop(paste("missing soil parameters:",Missing))}

  soils$THETA_R <- 0
  soils$THETA_S <- 0
  soils$ALPHA <- 0
  soils$NORDPOL <- 0

  toolbox.e3d::create_folders.E3D(path, overwrite = TRUE)
  nsoils <- nrow(soils)
  if (nsoils==1)
  {
    soils[2,] <- soils[1,]
    soils$POLY_ID <- 1:2
    print(message("Duplicating single soil entry to allow calculation."))
  }

  write.relief.E3D(POLY_ID = soils$POLY_ID, plotlength, round(slope),
                   file.path(path, "model/"), resolution = resolution)
  system2("e3d", paste0("/r \"", normalizePath(file.path(path,
                                                         "model/run.par")), "\""), wait = TRUE)
  utils::write.csv(soils, file.path(path, "model/soil/soil_params.csv"),
                   row.names = FALSE, quote = FALSE)
  write.landuse.E3D(POLY_ID = soils$POLY_ID, length = plotlength,
                    path = file.path(path, "model/soil/"), filename = "landuse.asc",
                    resolution = resolution)
  write.rainfile.E3D(time = c(0, endmin * 60), intens = c(intensity,
                                                          0), path, filename = "model/rain_e3d.csv")
  if (!ponding) {
    change_settings.E3D(path, filename = "model/run.par",
                        module = "Infiltration_model", setting = "Ponding",
                        value = "0")
  }

  utils::write.csv(soils, file.path(path, "model/soil/soil_params.csv"),
                   row.names = FALSE, quote = FALSE)
  system2("e3d", paste0("/c \"", normalizePath(file.path(path,
                                                         "model/run.par")), "\""), wait = TRUE)


  if (!file.exists(file.path(path, "model/result/sum_q.asc"))) {
    if (!file.exists(file.path(path, "model/result/sum_q.sdat"))) {
      stop("Can't read result set. Please check that standard output files in E3D are either *.asc or *.sdat.")
    }
    else {
      runoff <- raster::raster(file.path(path, "model/result/sum_q.sdat"))[, 1] * 1000
    }
  }
  else {
    runoff <- raster::raster(file.path(path, "model/result/sum_q.asc"))[, 1] * 1000
  }
  if (!file.exists(file.path(path, "model/result/sum_sedvol.asc"))) {
    if (!file.exists(file.path(path, "model/result/sum_sedvol.sdat"))) {
      stop("Can't read result set. Please check that standard output files in E3D are either *.asc or *.sdat.")
    }
    else {
      sed <- raster::raster(file.path(path, "model/result/sum_sedvol.sdat"))[, 1]
    }
  }
  else {
    sed <- raster::raster(file.path(path, "model/result/sum_sedvol.asc"))[, 1]
  }
  return(cbind.data.frame(soils$POLY_ID, runoff, sed))
}
