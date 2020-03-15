#' creates folder structure including a basic E3D modell
#'
#' @param path folder path in which file will be written
#' @param overwrite boolean, TRUE allows overwriting of files
#' @return file path to created folder structure
#' @export
#' @examples
#' create_folders.E3D(path = "C:/E3Dmodel/", overwrite = TRUE)

create_folders.E3D <- function(path = tmpDir(), overwrite = FALSE)
{
  unzip(zipfile= system.file("model.zip", package = "liberos"),exdir = paste0(path,"model"), overwrite = overwrite);
  return(paste0(path,"model/"));
}
