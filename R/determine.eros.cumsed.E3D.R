#' resistance to erosion determination
#'
#' This function calibrates parameter resistance to erosion that calculated cumulative sediment loss equals the measured value from a rainfall experiment.
#' Therefore it models a rainfall-experiment plot using EROSION-3D.
#'
#' @param FCl numeric
#' @param MCl numeric
#' @param CCl numeric
#' @param FSi numeric
#' @param MSi numeric
#' @param CSi numeric
#' @param FSa numeric
#' @param MSa numeric
#' @param CSa numeric
#'
#' @param Corg numeric
#' @param Bulk numeric
#' @param Moist numeric
#' @param Skin numeric
#' @param Roughness numeric
#' @param Cover numeric
#'
#' @param Soilloss numeric value, cumulative sediment loss in kg is the fitting target of this function
#' @param intensity numeric value, a constant rainfall intensity during the experiment
#' @param plotwidth numeric value, width of the experimental plot, CumRunoff will be normalized to one meter width using this parameter
#' @param plotlength integer value, length of experimental plot, needs to be an integer due to spatial resolution of 1 meter in E3D
#' @param resolution set spatial resoultion
#' @param slope integer value, mean slope of experimental plot in percent
#' @param endmin integer value, duration of rainfall experiment in full minutes rainfall was applied.
#' @param ponding logical TRUE means ponding option is used, FALSE - is not used in E3D, ponding limits amount of infiltrating water to available water
#' @param simlines integer value, number of parallel calculated plots, higher numbers decrease number of iteration steps with E3D, but increases number of write-read operations
#' @param path path to modeling directory, default is a temporary directory
#' @param silent logical, if TRUE skinfactor iteration steps will be written as message
#' @param version character, default is "3.2" which is actual official released version, other values are assuming version with implemented soil set creation /s
#' @importFrom raster raster
#' @importFrom utils read.csv
#' @export
#' @examples determine.eros.cumsed.E3D(FCl=5,MCl=10,CCl=15, FSi=10,MSi=20,CSi=10, FSa=15,MSa=10,CSa=5, Corg = 1.3, Bulk = 1300, Moist = 22, Skin = 0.005,Roughness=0.05, Cover = 20, Soilloss = 1, intensity = 0.5, plotwidth = 1, plotlength = 10, slope = 10, endmin = 30, ponding = TRUE)
#' @examples determine.eros.cumsed.E3D(FCl=5,MCl=10,CCl=15, FSi=10,MSi=20,CSi=10, FSa=15,MSa=10,CSa=5, Corg = 1.3, Bulk = 1300, Moist = 22, Skin = 0.005,Roughness=0.05, Cover = 20, Soilloss = 1, intensity = 0.5, plotwidth = 1, plotlength = 10, slope = 10, endmin = 30, ponding = FALSE)
#'
determine.eros.cumsed.E3D <- function(FCl,MCl,CCl, FSi,MSi,CSi, FSa,MSa,CSa, Corg, Bulk, Moist, Skin, Roughness, Cover, Soilloss, intensity, plotwidth, plotlength, slope, endmin, ponding =FALSE, simlines = 100, path = tempdir(), silent=TRUE, resolution = 1, version="3.2")
{
  if(check_setup.E3D()!=0){stop("cannot access e3d instalation")}

  create_folders.E3D(path, overwrite = TRUE)

  soils <- read.csv(file.path(path,"model/soil/soil_params.csv"))[1,]
  soils$BLKDENSITY <- Bulk
  soils$CORG <- Corg
  soils$INITMOIST <- Moist
  soils$FT <- FCl
  soils$MT <- MCl
  soils$GT <- CCl
  soils$FU <- FSi
  soils$MU <- MSi
  soils$GU <- CSi
  soils$FS <- FSa
  soils$MS <- MSa
  soils$GS <- CSa
  soils$THETA_R <- 0
  soils$THETA_S <- 0
  soils$ALPHA <- 0
  soils$NORDPOL <- 0
  soils$SKINFACTOR <- Skin
  soils$ROUGHNESS <- Roughness
  soils$COVER <- Cover

  soils[2:simlines,] <- soils[1,]
  soils$POLY_ID<- 1:simlines

  write.relief.E3D(POLY_ID = soils$POLY_ID,plotlength,round(slope),file.path(path,"model/"), resolution = resolution)
  system2("e3d", paste0('/r "',normalizePath(file.path(path,"model/run.par")),'"'), wait=TRUE)


if (version == "3.2")
{#---
# for e3d version 3.2 soil set creation is not implemented, so soil_params and landuse need to be written directly in this folder
  utils::write.csv(soils,file.path(path,"model/soil/soil_params.csv"), row.names = FALSE, quote = FALSE)
  write.landuse.E3D(POLY_ID = soils$POLY_ID,length = plotlength, path = file.path(path,"model/soil/"), filename = "landuse.asc", resolution = resolution)
  write.landuse.E3D(POLY_ID = soils$POLY_ID,length = plotlength, path = file.path(path,"model/"), filename = "landuse.asc", resolution = resolution)
  #---
}else{
  write.landuse.E3D(POLY_ID = soils$POLY_ID,length = plotlength, path = file.path(path,"model/"), filename = "landuse.asc", resolution = resolution)
  utils::write.csv(soils,file.path(path,"model/soil_params.csv"), row.names = FALSE, quote = FALSE)
  system2("e3d", paste0('/s "',normalizePath(file.path(path,"model/run.par")),'"'), wait=TRUE)
}

  write.rainfile.E3D(time = c(0,endmin*60), intens = c(intensity,0), path, filename = "model/rain_e3d.csv")

  if(!ponding){change_settings.E3D(path, filename = "model/run.par", module = "Infiltration_model", setting = "Ponding", value = "0")}

  #iteration of resistance to erosion
  erosupper=100
  eroslower=0.00000001
  i=1;                                           #counter for iteration steps

  Soilloss <- Soilloss / plotwidth;

    repeat{
      if(!silent){message(paste(i,":",eroslower,erosupper));}
      soils$ERODIBIL <- 10^seq(log10(eroslower),log10(erosupper), length.out = simlines);

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

      if(!file.exists(file.path(path,"model/result/sum_sedvol.asc")))
      {
        if(!file.exists(file.path(path,"model/result/sum_sedvol.sdat")))
        {stop("Can't read result set. Please check that standard output files in E3D are either *.asc or *.sdat.")}else
        { sed <- raster::raster(file.path(path,"model/result/sum_sedvol.sdat"))[,1]}
      }else
      { sed <- raster::raster(file.path(path,"model/result/sum_sedvol.asc"))[,1]}

      if(runoff[1]<=0)
      {message("No Runoff - No Soilloss"); return(NA);}

      conc <- round(sed/runoff*1000)

      if(conc[1]>=300 & sed[1] < Soilloss)
      {message("Measured Soilloss requires sediment concentration higher than 300 kg/m^3. Outside set limits in EROSION-3D."); return(NA);}

      eroslower <- soils$ERODIBIL[Position(function(x){Soilloss<x},sed, right = TRUE)]
      erosupper <- soils$ERODIBIL[Position(function(x){Soilloss>x},sed)]

      if(i>100) {print("no iteration"); break;} #end if not iterating
      if(erosupper/eroslower>0.999 && erosupper/eroslower<1.001) break; #end if iterating
      i=i+1;                                       #increase iteration counter
    }

  return(mean(c(erosupper,eroslower)));
}
