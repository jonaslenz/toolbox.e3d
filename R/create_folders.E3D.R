#' creates folder structure including a basic E3D modell
#'
#'
#' @inheritParams
#' @return creates folders
#' @seealso
#' @export
#' @examples
#'

create_folders.E3D <- function(path = tmpDir(), overwrite = FALSE)
{
  unzip(zipfile= system.file("model.zip", package = "liberos"),exdir = paste0(path,"model"), overwrite = overwrite);
  return(paste0(path,"model"));
}
