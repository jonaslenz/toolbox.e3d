#' Write an EROSION-3D rainintensity input file
#'
#' This function writes a rainfall intensity input file, which can be used as input in EROSION-3D.
#' Rainfall intensity value in mm/min is valid from corresponding timestamp.
#' Last timestamp defines starting time of last intervall of timestep length.
#'
#' @param time numeric vector, containing timestamps in seconds when intensity changes, must be multiple of parameter timestep
#' @param intens numeric vector, gives rainfall intensity value valid from corresponding timestamp in mm/min
#' @param path folder path in which file will be written
#' @param filename name of created file
#' @param date date to be written in file output
#' @param timestep time resolution in seconds written to output file, at current stage E3D supports resolution in multiples of 60 seconds
#' @return None, file is written
#' @export
#' @examples
#' write.rainfile.E3D(time=c(0,60,120,240,600),intens=c(0.1,0.6,1.2,2.4,0), path = "C:/E3Dmodel/", date = "1990-10-27", filename = "/model/rain_e3d.csv", timestep = 60)
#' write.rainfile.E3D(time=c(0,3600), intens=c(0.6,0), path = "C:/E3Dmodel/", date = "1990-10-27", filename = "/model/rain_e3d.csv", timestep = 60)
#' write.rainfile.E3D(time=c(0,60,1200,240,600),intens=c(0.1,0.6,1.2,2.4,0), path = "C:/E3Dmodel/", date = "1990-10-27", filename = "/model/rain_e3d.csv", timestep = 60)

write.rainfile.E3D <- function(time, intens, path, date = "1990-01-01", filename = "/model/rain_e3d.csv", timestep = 60)
{
  if(length(intens) != length(time)){stop("number of elements in time and intens must be equal")}
  if(is.unsorted(time))
    {
      ordering <- order(time);
      time <- time[ordering];
      intens <- intens[ordering];
      message("timeline was ordered.")
    }

  #create header for E3D rainfall file
  a <- c("Y","M","D","H","M","0")
  a <- rbind(a,a,a,a,c("Y","M","D","H","M","1"))

  for (i in 0:(max(time)/timestep))
  {
    a <- rbind(a,c(unlist(strsplit(as.character(format(as.POSIXct(date)+(i*timestep),"%Y,%m,%d,%H,%M")), ",")),
                   if(i*timestep<max(time)){
                     intens[Position(function(x) x>(i*timestep), time)-1]
                   }else{intens[time==max(time)]}
               ))
  }
  write.table(a,file = paste0(path,filename), row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
}
