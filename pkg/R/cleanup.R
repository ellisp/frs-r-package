
#' Function for removing all objects except those designated as 'keepers'
#' 
#' @param keepers a vector of the names of objects to keep
#' @param verbose verbosity of messages
#' @export
#' @author Peter Ellis
cleanup <- function(keepers = NULL, verbose = FALSE){
  objs <- ls(pos = 1)
  objs <- objs[!objs %in% keepers]
  if(verbose){
    message(paste("Removing:", paste(objs, collapse = ", ")))  
  }
  rm(list = objs, pos = 1)
}
