#' check e3d setup
#'
#' checks, if e3d can be run from command line
#'
#' @return numeric, exit code of command line
check_setup.E3D <- function()
{
  system2("e3d", wait = FALSE, invisible = TRUE, stdout = NULL)
  code <- system2("Taskkill","/IM e3d.exe /F", stdout = NULL)
  if(code != 0){message("please add e3d instalation directory to %PATH% in global environment and restart R.")}
  return(code)
}
