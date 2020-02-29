#' creates folder structure for E3D modelling
#'
#' 
#' @inheritParams str_detect
#' @return creates folders
#' @seealso
#' @export
#' @examples
#'

create_folders.E3D <- function(path)
{
  #create model folders
  if(!dir.exists(paste0(path,"model/"))){dir.create(paste0(path,"model/"))}
  if(!dir.exists(paste0(path,"model/soil/"))){dir.create(paste0(path,"model/soil/"))}
  if(!dir.exists(paste0(path,"model/relief/"))){dir.create(paste0(path,"model/relief/"))}
  if(!dir.exists(paste0(path,"model/result/"))){dir.create(paste0(path,"model/result/"))}
}
