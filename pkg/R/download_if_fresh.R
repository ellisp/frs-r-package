#' Download a file if it doesn't already exist locally
#' 
#' 
#' @param fn URL of a file to download
#' @param destfile path and name of file to save the file as, if it isn't already there
#' @param mode mode of the file to be downloaded. See \code{\link[utils]{download.file}} for details.
#' @author Peter Ellis
#' @details
#' This is a simple convenience function that only downloads files if the \code{destfile} doesn;t already exist.
#' It should be used in situations where you want, for reproducibility, to ensure a file is downloaded but you 
#' don't want it freshly downloaded every time a project is run (typically, because it is large, and not 
#' updated frequently)
#' @seealso \code{\link[utils]{download.file}}
#' @importFrom utils download.file
#' @export
download_if_fresh <- function(fn, destfile, mode = "wb"){
  if(!destfile %in% list.files(recursive = TRUE)){
    utils::download.file(fn, destfile = destfile, mode = mode)
  }
}