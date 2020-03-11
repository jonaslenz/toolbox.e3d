#' Skinfactor determination
#'
#' This function models a rainfall-experiment plot in EROSION-3D.
#' The calibration parameter skinfactor is determined that calculated cummulative runoff equals the measured value.
#'
#' @export
#' @examples
#'
determine.skin.runoff.E3D <- function(Cl, Si, Sa, Corg, Bulk, Moist, CumRunoff, intensity, plotwidth, plotlength, slope, endmin, ponding =FALSE)
{
  path <- "C:/Users/Jonas Lenz/Desktop/e3d_parametrisierung_test"
  unzip(zipfile= system.file("model.zip", package = "liberos"),exdir = paste0(path,""))

  soils <- read.csv(paste0(path,"/model/soil/soil_params.csv"))[1,]
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
  soils$POLY_ID <- 1


  write.relief.E3D(POLY_ID = c(1,1,1),plotlength,round(slope),paste0(path,"/model/"))
  system2("C:/Program Files (x86)/Erosion-3D_32-320/e3d", paste0('/r "',path,'/model/run.par"'), wait=TRUE)

  write.lulc.E3D(POLY_ID = c(1,1,1),length = plotlength, path = paste0(path,"/model/soil/"), filename = "landuse.asc")

  write.rainfile.E3D(cbind.data.frame(time = c(0,endmin*60), intens = intensity), path, filename = "/model/rain_e3d.csv")





  #iteration of Skinfaktor
  soils$SKINFACTOR = 0;                                      #Skinfactor==0 means no infiltration
  i=1;                                           #counter for iteration steps
  deci=1;                                        #decimal operator for iteration of Skinfactor
  runoff_highSkin=0;                             #high Skinfactor means no runoff
  runoff_lowSkin=intensity*plotlength*endmin;      #all rainfall contributes to runoff, when Skinfactor == 0

  CumRunoff <- CumRunoff / plotwidth;

  if(runoff_lowSkin<CumRunoff)
  {print("Measured cumulative runoff is higher than available water from rainfall. Did somebody pee in your measurement cup?"); Skin = NA;}else
  {
    repeat{
      soils$SKINFACTOR <- soils$SKINFACTOR+1*deci;    #increase higher skinfactor to decrease runoff
      #message(paste(i,": ", soils$SKINFACTOR))
      write.csv(soils,paste0(path,"/model/soil/soil_params.csv"), row.names = FALSE, quote = FALSE)

      if(ponding){system2("C:/Program Files (x86)/Erosion-3D_32-320/e3d", paste0('/c "',path,'/model/run.par"'), wait=TRUE)}else{
      system2("C:/Program Files (x86)/Erosion-3D_32-320/e3d", paste0('/c "',path,'/model/run_noponding.par"'), wait=TRUE)}

      runoff_highSkin <- unique(raster(read.asciigrid(paste(path,"/model/result/sum_q.asc", sep="")))[,1])*1000




      if(i>100) {print("no iteration"); break;} #end if not iterating
      if(runoff_highSkin/CumRunoff>0.999999 && runoff_highSkin/CumRunoff<1.000001) break; #end if iterating
      i=i+1;                                       #increase iteration counter

      if(CumRunoff>runoff_highSkin &&              #if Cumrunoff is in between lower and higher calcuated runoff, actual digit of Skin is 1 higher than target skin
         CumRunoff<runoff_lowSkin)
      {soils$SKINFACTOR=soils$SKINFACTOR-1.00000000000001*deci; deci=deci*0.1;}else       #calculate next digit of Skin (decrease actual SKin; decrease decimal digit)
      {runoff_lowSkin = runoff_highSkin;}     #if runoff_lowSkin is lower than CumRunoff, set higher runoff as lower runoff
      #message(runoff_lowSkin);
      runoff_highSkin = 0;                         #reset lower Runoff, for next iteration step

    }
  }



  return(soils$SKINFACTOR);
}
