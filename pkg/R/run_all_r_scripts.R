
#' Run all R scripts in a folder system
#' 
#' @param path relative file path to the folder with R scripts to run
#' @param recursive whether or not to run all scripts in sub-folders as well as in the path
#' @param verbose whether or not to print to the screen the name of each script as run
#' @param push_through whether to push through to the next script if an error is found (otherwise will stop)
#' @param cleanup whether to remove all objects from the global environment other than those designated as keepers.
#' If there is an object called \code{keepers} in the global environment, then any objects named in \code{keepers}
#' will not be removed at the end of running each script.
#' @param suppress_messages whether messages should be suppressed or not. Sometimes messages can be overwhelming.
#' @author Peter Ellis
#' @export
run_all_r_scripts <- function(path, recursive = TRUE, verbose = TRUE, 
                              push_through = FALSE, cleanup = TRUE,
                              suppress_messages = TRUE){
  if(cleanup & !exists("keepers")){
    keepers <- NULL
  }
  
  fs <- list.files(path, pattern = "\\.[Rr]$", full.names = TRUE, recursive = recursive)
  errors <- 0
  for (f in fs){
    if(verbose){
      print(paste("Running", f))
    }
    if(push_through){
      res <- try(suppressMessages(source(f)))
      if(class(res)[1] == "try-error"){
        errors <- errors + 1
      }
      
    }
    else{
      (suppressMessages(source(f)))
    }
    if(cleanup){
      cleanup(keepers)
    }
    
  }
  if(verbose){
    print(paste("Ran", length(fs), "scripts with", errors, "errors."))
  }
}
