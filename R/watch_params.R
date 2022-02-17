#' watch results of EROSION-3D
#'
#' This function sets the flag, that a parameter value is recorded in each time step by EROSION-3D
#' @param watchcellid integer, id of parameter of interest - explanation and possible valuesin e3d_watchcell_IDs
#' @param path_to_ini, path to "e3d.ini" in standard instatlation stored in "C:\\Users\\<user>\\AppData\\Roaming\\GeoGnostics\\Erosion3D-330\\"
#' @importFrom ini read.ini
#' @importFrom ini write.ini
#' @export
#' @examples e3d_watchcell_set <- function(2, path_to_ini = "C:\\Users\\Jonas.Lenz\\AppData\\Roaming\\GeoGnostics\\Erosion3D-330\\")
#'

e3d_watchcell_set <- function(watchcellid, path_to_ini = "C:\\Users\\Jonas.Lenz\\AppData\\Roaming\\GeoGnostics\\Erosion3D-330\\")
{
  e3d_settings <- read.ini(paste0(path_to_ini,"e3d.ini"))
  e3d_settings$Simulation$testpara_grid = watchcellid
  write.ini(e3d_settings, paste0(path_to_ini,"e3d.ini"))

  lookupstring <- e3d_watchcell_IDs()
  lookupstring <- lookupstring$string[lookupstring$ID == watchcellid]


  print(paste0("watching on ",lookupstring))
}

#' get watched results of EROSION-3D
#'
#' This function gets the results of an watched parameter and stacks them in a raster stack. Make sure to set watch method before running e3d by e3d_watchcell_set()!
#' @param watchcellid integer, id of parameter of interest - explanation and possible valuesin e3d_watchcell_IDs
#' @param path path model folder
#' @importFrom raster raster
#' @importFrom raster stack
#' @export
#' @examples e3d_watchcell_set <- function(2, path = tempdir())
#'

e3d_watchcell_get_result <- function(watchcellid, path = tempdir())
{
  lookupstring <- e3d_watchcell_IDs()
  lookupstring <- lookupstring$string[lookupstring$ID == watchcellid]

  all <- list.files(paste0(path,"model\\result\\"))
  all <- all[grepl(paste0(lookupstring,"1"), all)]
  all <- all[grepl(".tif", all)]
  all <- gsub(".tif","",all)

  result_stack <- read_result.E3D(all[1],path)
  for (z in all[-1])
  {
    new <- read_result.E3D(z,path)
    result_stack <- raster::stack(result_stack, new)
  }
  return(result_stack)
}

#' Get parameter watch ID of EROSION-3D
#'
#' @export
#'
e3d_watchcell_IDs <- function()
{
  data.frame(ID = 0:19,
             string = c("no parameter",
                        "v_film",
                        "soilp_infil",
                        "filmdicke",
                        "filmdicke_F",
                        "v_film_F",
                        "q_in","ch_sum_q_in",
                        "ch_sum_q",
                        "v_tropfen",
                        "imp_tropfen",
                        "eroskenn",
                        "fracht",
                        "real_fracht",
                        "delta_real_fracht",
                        "delta_sum_kornfracht",
                        "delta_eff_fracht",
                        "fracht1_in",
                        "max_v_film",
                        "max_filmdicke"
             ),
             explanation = c("No parameter watched.",
                             "Filmgeschwindigkeit [m/s]",
                             "Infiltration [mm/min]",
                             "Filmdicke [m]",
                             "Filmdicke (Furche) [m]",
                             "Filmgeschwindigkeit (Furche) [m]",
                             "Zufluss (Oberflächenabfluss) aus den Nachbarzellen [m^3 / s]",
                             "Zufluss (Vorfluterabfluss) aus den Nachbarzellen (kumulativ) [m^3 / s]",
                             "Oberflächenabfluss + Vorfluterabfluss (kumulativ) [m^3 / s]",
                             "Tropfengeschwindigkeit [m/s]",
                             "Impulsstrom der Tropfen [kg/(m*s²)]",
                             "Erosionskoeffizient [-]",
                             "Sedimentfracht [kg/m]",
                             "real_fracht [kg/m]",
                             "delta_real_fracht [kg/m]",
                             "delta_sum_kornfracht [kg/m]",
                             "delta_eff_fracht [kg/m]",
                             "fracht1_in [kg/m]",
                             "Max. of v_film per cell",
                             "Max. of filmdicke per cell"
             )
  )
}
