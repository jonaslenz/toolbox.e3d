#' gets soil params example
#'
#' This function returns an example dataframe including all soil parameters of EROSION-3D.
#'
#' @importFrom utils read.csv
#' @export
#' @examples dummy_soilset(soil_params)
#'

dummy_soilset <- function(path = tempdir())
{
create_folders.E3D(path, overwrite = TRUE)
soils <- read.csv(file.path(path,"model/soil/soil_params.csv"))[1,]
return(soils)
}
