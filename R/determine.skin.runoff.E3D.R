#' Skinfactor determination
#'
#' This function models a rainfall-experiment plot in EROSION-3D.
#' The calibration parameter skinfactor is determined that calculated cummulative runoff equals the measured value.
#'
#' @export
#' @examples
#'
determine.skin.runoff.E3D <- function(Cl, Si, Sa, Corg, Bulk, Moist, CumRunoff, intensity, plotwidth, plotlength, slope, endmin, ponding =FALSE, simlines = 100, path = "C:/E3Dmodel/", silent=TRUE)
{
  create_folders.E3D(path, overwrite = TRUE)

  soils <- read.csv(paste0(path,"model/soil/soil_params.csv"))[1,]
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

  write.relief.E3D(POLY_ID = soils$POLY_ID,plotlength,round(slope),paste0(path,"model/"))
  system2("C:/Program Files (x86)/Erosion-3D_32-320/e3d", paste0('/r "',path,'model/run.par"'), wait=TRUE)

  write.landuse.E3D(POLY_ID = soils$POLY_ID,length = plotlength, path = paste0(path,"model/soil/"), filename = "landuse.asc")

  write.rainfile.E3D(cbind.data.frame(time = c(0,(endmin-1)*60), intens = intensity), path, filename = "model/rain_e3d.csv")



  #iteration of Skinfaktor
  skinupper=100
  skinlower=0.00000001
  i=1;                                           #counter for iteration steps

  runoff_noinfil=intensity*plotlength*endmin;      #all rainfall contributes to runoff, when Skinfactor == 0

  CumRunoff <- CumRunoff / plotwidth;

  if(runoff_noinfil<CumRunoff)
  {print("Measured cumulative runoff is higher than available water from rainfall. Did somebody pee in your measurement cup?"); Skin = NA;}else
  {
    repeat{
      if(!silent){message(paste(i,":",skinlower,skinupper));}
      soils$SKINFACTOR <- 10^seq(log10(skinlower),log10(skinupper), length.out = simlines);

      write.csv(soils,paste0(path,"model/soil/soil_params.csv"), row.names = FALSE, quote = FALSE)

      if(ponding){system2("C:/Program Files (x86)/Erosion-3D_32-320/e3d", paste0('/c "',path,'model/run.par"'), wait=TRUE)}else{
      system2("C:/Program Files (x86)/Erosion-3D_32-320/e3d", paste0('/c "',path,'model/run_noponding.par"'), wait=TRUE)}

      runoff <- raster(read.asciigrid(paste(path,"model/result/sum_q.asc", sep="")))[,1]*1000

      skinlower <- soils$SKINFACTOR[Position(function(x){CumRunoff<x},runoff, right = TRUE)]
      skinupper <- soils$SKINFACTOR[Position(function(x){CumRunoff>x},runoff)]


      if(i>100) {print("no iteration"); break;} #end if not iterating
      if(skinupper/skinlower>0.999 && skinupper/skinlower<1.001) break; #end if iterating
      i=i+1;                                       #increase iteration counter

    }
  }



  return(mean(c(skinupper,skinlower)));
}
