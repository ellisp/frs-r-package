#' Download a file if it doesn't already exist locally
#' 
#' 
#' @param fn URL of a file to download
#' @param destfile path and name of file to save the file as, if it isn't already there
#' @param mode mode of the file to be downloaded. See \code{\link[utils]{download.file}} for details.
#' @param ... other arguments to be passed to \code{download.file()}
#' @author Peter Ellis
#' @details
#' This is a simple convenience function that only downloads files if the \code{destfile} doesn;t already exist.
#' It should be used in situations where you want, for reproducibility, to ensure a file is downloaded but you 
#' don't want it freshly downloaded every time a project is run (typically, because it is large, and not 
#' updated frequently)
#' @return An (invisible) integer code, 987 if no attempt was made to download the file (because \code{destfile}
#' already exists), 0 for success and non-zero for failure. Inherited from \code{download.file} if the file 
#' is downloaded.
#' @examples 
#' file <- "_dif_eg.html"
#' x <- download_if_fresh("http://freerangestats.info", destfile = file)
#' y <- download_if_fresh("http://freerangestats.info", destfile = file)
#' print(c(x, y))
#' unlink(file)
#' @seealso \code{\link[utils]{download.file}}
#' @importFrom utils download.file
#' @export
download_if_fresh <- function(fn, destfile, mode = "wb", ...){
  status <- 987
  if(!destfile %in% list.files(recursive = TRUE)){
    status <- utils::download.file(fn, destfile = destfile, mode = mode, ...)
    
  }
  invisible(status)
}


