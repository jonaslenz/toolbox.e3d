#' creates folder structure including a basic E3D modell
#'
#'
#' @inheritParams
#' @return creates folders
#' @seealso
#' @export
#' @examples
#'

create_folders.E3D <- function(path, overwrite = FALSE)
{
if (!overwrite)
{
  if(dir.exists(paste0(path,"/model/"))){stop("Directory allready exists. Use overwrite = TRUE")}
}
  unzip(zipfile= system.file("model.zip", package = "liberos"),exdir = path, overwrite = overwrite)
}

