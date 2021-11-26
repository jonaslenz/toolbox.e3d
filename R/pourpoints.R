#' watch results of EROSION-3D using pour points
#'
#' This function sets the flag, that a parameter value is recorded in each time step by EROSION-3D
#' @param watchcellid integer, id of parameter of interest - explanation and possible valuesin e3d_watchcell_IDs
#' @param path_to_ini, path to "e3d.ini" in standard instatlation stored in "C:\\Users\\<user>\\AppData\\Roaming\\GeoGnostics\\Erosion3D-330\\"
#' @importFrom ini read.ini
#' @importFrom ini write.ini
#' @export
#' @examples activate_pourpoints <- function(activate_pour = TRUE, path_to_ini = "C:\\Users\\Jonas.Lenz\\AppData\\Roaming\\GeoGnostics\\Erosion3D-330\\")
#'

set_pourpoints <- function(activate_pour = TRUE, path_to_ini = "C:\\Users\\Jonas.Lenz\\AppData\\Roaming\\GeoGnostics\\Erosion3D-330\\")
{
  e3d_settings <- read.ini(paste0(path_to_ini,"e3d.ini"))
  if(activate_pour)
  {
    e3d_settings$Simulation$save_wsdpp = 2
  }else
  {
    e3d_settings$Simulation$save_wsdpp = 0
  }
  write.ini(e3d_settings, paste0(path_to_ini,"e3d.ini"))

  if(read.ini(paste0(path_to_ini,"e3d.ini"))$Simulation$save_wsdpp == 2)
  {
    print("POUR points active")
  }else{
    print("POUR points not active")
  }
}

#' Write POUR point raster
#'
#' Create a pour point raster to watch virtual slopes at outlet (lowest point of each slope)
#' @param POLY_ID numeric vector, number of elements defines columns of created relief file. Refers to POLY_ID in soil_params.csv of E3D.
#' @param length numeric value, defining slope length in whole meters
#' @param path folder path in which file will be written
#' @param filename name of created file
#' @param resolution set spatial resolution
#' @return None, file is written
#' @importFrom raster writeRaster
#' @importFrom raster raster
#' @export
#' @examples
#' write.pourpoint.E3D(POLY_ID = c(1,2,3),length = 50, path = "C:/E3Dmodel/relief/", filename = "dem.asc")

write.pourpoint.E3D <- function(POLY_ID, length, path, filename = "POUR.asc", resolution = 1)
{
  #EROSION-3D requires at least four rows
  if (length(POLY_ID) < 4){POLY_ID <- rep(POLY_ID, each=4);}
  if(!(length%%resolution==0 || abs((length%%resolution)/resolution-1) < 0.00001)){stop("length must be a multiple of resolution (default resolution = 1).")}

  rows <- length(POLY_ID)

  rast <- raster(resolution = resolution,
                 xmn=0,xmx=length,
                 ymn=0, ymx=rows*resolution,
                 crs="+proj=robin +datum=WGS84",
                 vals=0
                 )
  rast[,1] <- POLY_ID
  writeRaster(
    rast,
    filename = file.path(path,filename),
    format ="ascii",
    overwrite=TRUE,
    datatype = "INT2U"
  )
}
