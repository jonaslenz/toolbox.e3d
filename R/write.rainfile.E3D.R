#' Write an EROSION-3D rainintensity input file
#'
#' This function writes a rainintensity input file, which can be used as input in EROSION-3D.
#' x shall be a dataframe containing a timeline
#' @inheritParams str_detect
#' @return No return value, but file is written
#' @seealso
#' @export
#' @examples
#' write.rainfile.E3D()

write.rainfile.E3D <- function(x = cbind.data.frame(time=c(0,60,120,240),intens=c(0.1,0.6,1.2,2.4)), date = "1990-10-27", filename = "rain_e3d.reg", timestep = 60)
{
  if(!"intens" %in% names(x)){stop("missing intensity, check input")}
  if(!"time" %in% names(x)){stop("missing timeline, check input")}
  if(is.unsorted(x$time)){stop("timeline is disturbed")}
  if(x$intens[1]==0){stop("please start timeline with start of rainfall")}

  #create header for E3D rainfall file
  a <- c("Y","M","D","H","M","0")
  a <- rbind(a,a,a,a,c("Y","M","D","H","M","1"))

  for (i in 0:(max(x$time)/timestep))
  {
    a <- rbind(a,c(unlist(strsplit(as.character(format(as.POSIXct(date)+(i*timestep),"%Y,%m,%d,%H,%M")), ",")),
                   if(i*timestep<max(x$time)){
                     x$intens[Position(function(x) x>(i*timestep), x$time)-1]
                   }else{x$intens[x$time==max(x$time)]}
               ))
  }
  write.table(a,file = filename, row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
}
