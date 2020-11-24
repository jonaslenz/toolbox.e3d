#' Skinfactor determination
#'
#' This function calibrates parameter skinfactor that calculated cumulative runoff equals the measured value from a rainfall experiment.
#' Therefore it models a rainfall-experiment plot using EROSION-3D.
#'
#' @param Cl numeric
#' @param Si numeric
#' @param Sa numeric
#'
#' @param Corg numeric
#' @param Bulk numeric
#' @param Moist numeric
#'
#' @param CumRunoff numeric value, cumulative runoff is the fitting target of this function
#' @param plotwidth numeric value, width of the experimental plot, CumRunoff will be normalized to one meter width using this parameter
#' @param plotlength integer value, length of experimental plot, needs to be an integer due to spatial resolution of 1 meter in E3D
#' @param slope integer value, mean slope of experimental plot in percent
#' @param intensity numeric vector, rainfall intensity of preceding time interval - corespondeces to endmin
#' @param endmin numeric vector, duration since start of rainfall experiment in full minutes, length must equal length of intensity
#' @param ponding logical TRUE means ponding option is used, FALSE - is not used in E3D, ponding limits amount of infiltrating water to available water
#' @param simlines integer value, number of parallel calculated plots, higher numbers decrease number of iteration steps with E3D, but increases number of write-read operations
#' @param path path to modeling directory, default is a temporary directory
#' @param silent logical, if TRUE skinfactor iteration steps will be written as message
#' @param version version number can be set manually if known to reduce calls to E3D and save processing time
#' @importFrom raster raster
#' @importFrom utils read.csv
#' @export
#' @examples determine.skin.runoff.E3D(Cl = 30, Si = 40, Sa = 30, Corg = 1.3, Bulk = 1300, Moist = 22, CumRunoff = 100, intensity = 0.5, plotwidth = 1, plotlength = 10, slope = 10, endmin = 30, ponding = TRUE, silent = FALSE)
#'
determine.skin.runoff.E3D <- function(Cl, Si, Sa, Corg, Bulk, Moist, CumRunoff, intensity, plotwidth, plotlength, slope, endmin, ponding =FALSE, simlines = 100, path = tempdir(), silent=TRUE, version = get_version.E3D())
{
  if(!ponding & numeric_version(version)<"3.2.0.9"){stop("Ponding option can be turned off only in E3D-version after 3.2.0.9")}

  create_folders.E3D(path, overwrite = TRUE)

  soils <- read.csv(file.path(path,"model/soil/soil_params.csv"))[1,]
  soils$BLKDENSITY <- Bulk
  soils$CORG <- Corg
  soils$INITMOIST <- Moist
  soils$FT <- 0
  soils$MT <- Cl
  soils$GT <- 0
  soils$FU <- 0
  soils$MU <- Si
  soils$GU <- 0
  soils$FS <- 0
  soils$MS <- Sa
  soils$GS <- 0
  soils$THETA_R <- 0
  soils$THETA_S <- 0
  soils$ALPHA <- 0
  soils$NORDPOL <- 0

  soils[2:simlines,] <- soils[1,]
  soils$POLY_ID<- 1:simlines

  write.relief.E3D(POLY_ID = soils$POLY_ID,plotlength,round(slope),file.path(path,"model/"))
  system2("e3d", paste0('/r "',normalizePath(file.path(path,"model/run.par")),'"'), wait=TRUE)

  utils::write.csv(soils,file.path(path,"model/soil/soil_params.csv"), row.names = FALSE, quote = FALSE)
  write.landuse.E3D(POLY_ID = soils$POLY_ID,length = plotlength, path = file.path(path,"model/soil/"), filename = "landuse.asc")

  write.rainfile.E3D(time = c(0,endmin*60), intens = c(intensity,0), path, filename = "model/rain_e3d.csv")

  if(!ponding){change_settings.E3D(path, filename = "model/run.par", module = "Infiltration_model", setting = "Ponding", value = "0")}

  #iteration of Skinfaktor
  skinupper=100
  skinlower=0.00000001
  i=1;                                           #counter for iteration steps

  #all rainfall contributes to runoff, when Skinfactor == 0
  runoff_noinfil = sum(diff(c(0,endmin)) * intensity)* plotlength

  CumRunoff <- CumRunoff / plotwidth;

  if(runoff_noinfil<CumRunoff)
  {print("Measured cumulative runoff is higher than available water from rainfall."); Skin = NA;}else
  {
    repeat{
      if(!silent){message(paste(i,":",skinlower,skinupper));}
      soils$SKINFACTOR <- 10^seq(log10(skinlower),log10(skinupper), length.out = simlines);

      utils::write.csv(soils,file.path(path,"model/soil/soil_params.csv"), row.names = FALSE, quote = FALSE)

      system2("e3d", paste0('/c "',normalizePath(file.path(path,"model/run.par")),'"'), wait=TRUE)

      #check and read possible output formats of E3D
      if(!file.exists(file.path(path,"model/result/sum_q.asc")))
        {
          if(!file.exists(file.path(path,"model/result/sum_q.sdat")))
            {stop("Can't read result set. Please check that standard output files in E3D are either *.asc or *.sdat.")}else
            { runoff <- raster::raster(file.path(path,"model/result/sum_q.sdat"))[,1]*1000 }
        }else
        { runoff <- raster::raster(file.path(path,"model/result/sum_q.asc"))[,1]*1000 }

      skinlower <- soils$SKINFACTOR[Position(function(x){CumRunoff<x},runoff, right = TRUE)]
      skinupper <- soils$SKINFACTOR[Position(function(x){CumRunoff>x},runoff)]


      if(i>100) {print("no iteration"); break;} #end if not iterating
      if(skinupper/skinlower>0.999 && skinupper/skinlower<1.001) break; #end if iterating
      i=i+1;                                       #increase iteration counter
    }
  }



  return(mean(c(skinupper,skinlower)));
}
