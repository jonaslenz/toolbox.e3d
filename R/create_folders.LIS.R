#' creates folder structure including a basic openLISEM model
#'
#' @param path folder path in which file will be written
#' @param overwrite boolean, TRUE allows overwriting of files
#' @return file path to created folder structure
#' @importFrom utils unzip
#' @export
#' @examples
#' create_folders.LIS(path = "C:/LISmodel/", overwrite = TRUE)

create_folders.LIS <- function(path = tempdir(), overwrite = FALSE)
{
  unzip(zipfile= system.file("lisem.zip", package = "toolbox.e3d"),exdir = file.path(path,"lisem"), overwrite = overwrite);
  change_settings.LIS(path,setpath = TRUE)
  return(normalizePath(file.path(path,"lisem")));
}
