#' Helper function to convert the output of Sys.time() into a character string
#' without the time zone on it
#' 
#' @details Not exported.
#' @keywords internal
#' @param dt an object of class \code{POSIXCT}
#' @export
#' @examples
#' datetime_ch(Sys.time())
datetime_ch <- function(dt){
  dt <- gsub(" [A-Z]*$", "", as.character(dt))
  dt <- paste0("CAST ('", dt, "' AS DATETIME)")
  return(dt)
}



#' Execute SQL
#'
#' Execute T-SQL in a script, split into batches
#' 
#' @export
#' @importFrom DBI dbGetQuery
#' @importFrom stringr str_split str_length 
#' @details Reads a script of SQL, splits it into separate queries on the basis of any occurrences of \code{GO}
#' in the script, and passes it to the database server for execution.  While the initial use case was for SQL Server, there's no
#' reason why it wouldn't work with other ODBC connections.
#' 
#' The case of \code{GO} is ignored but it has to be the first non-space word on its line of code.
#' 
#' This won't work with SQL scripts that create of temporary tables eg \code{#my_temp_table}; I'm not sure why.
#' The workaround is to give such tables explicit persistent names eg \code{dbo.my_temp_table} and remember to
#' clean them up at the end.
#' 
#' If any batch at any point returns rows of data (eg via a \code{SELECT} statement that does not \code{INSERT} the
#' results into another table or variable on the database), the rest of that batch is not executed.  
#' If that batch was the last batch of SQL
#' in the original file, the results are returned as a data.frame, otherwise it is discarded.
#' 
#' Example SQL code for creating a log suitable for this function:
#' \preformatted{
#' CREATE TABLE some_database.dbo.sql_executed_by_r_log
#' (
#'   log_event_code INT NOT NULL IDENTITY PRIMARY KEY, 
#'   start_time     DATETIME, 
#'   end_time       DATETIME,
#'   sub_text       NVARCHAR(200),
#'   script_name    NVARCHAR(1000),
#'   batch_number   INT,
#'   result         NCHAR(30),
#'   err_mess       VARCHAR(8000),
#'   duration       NUMERIC(18, 2),
#'   sub_out        NVARCHAR(255),
#'   ruser          NVARCHAR(255),
#' );
#' }
#' 
#' @param channel connection handle as returned by RODBC::odbcConnect() of class RODBC
#' @param filename file name of an SQL script
#' @param sub_in character string that you want to be replaced with \code{sub_out}.  Useful if you want to do a bulk search
#' and replace.  This is useful if you have a bunch of scripts that you maybe want
#' to run on one schema sometimes, and on another schema other times - just automate the search and replace.  Use with caution.
#' @param sub_out character string that you want to replace \code{sub_in} with.
#' @param fixed logical.  If TRUE, \code{sub_in} is a string to be matched as is.  Otherwise it is treated as a regular expression 
#' (eg if fixed = FALSE, then . is a wild card)
#' @param error_action should you stop with an error if a batch gets an error message back from the database?  Any alternative
#' to "stop" means we just keep ploughing on, which may or may not be a bad idea.  Use "stop" unless you know that failure
#' in one part of a script isn't fatal.
#' @param log_table table in the database to record a log of what happened.  Set to NULL if no log table available.  The log_table
#' needs to have (at least) the following columns: event_time, sub_out, script_name, batch_number, result, err_mess, duration and ruser. 
#' See Details for example SQL to create such a log table.
#' @param verbose Logical, gives some control over messages
#' @param ... other arguments to be passed to \code{sqlQuery()}, such as \code{stringsAsFactors = FALSE}.
#' @examples
#' \dontrun{
#' ch <- dbConnect(odbc(), "some_dsn_name", database = "some_database")
#' execute_sql(ch, "some_file.sql")
#' }
#' @author Peter Ellis
execute_sql <- function(channel, filename, sub_in = NULL, sub_out = NULL, fixed = TRUE, 
                        error_action = "stop", log_table = 'dbo.sql_executed_by_r_log', 
                        verbose = FALSE, ...){
  
  check <- try( DBI::dbGetQuery(channel, "select 1"), silent = TRUE)
  if(class(check) == "try-error"){
    stop("Channel seems to be dead. Maybe re-create it with odbcConnect")
  }
  
  
  # we can't tell in advance what encoding the .sql files are in, so we read it in
  # in two ways (one of which is certain to return gibberish) and choose the version that is recognised as a proper string:
  
  # encoding method 1 (weird Windows encoding):
  file_con <- file(filename, encoding = "UCS-2LE")
  sql1 <- paste(readLines(file_con, warn = FALSE), collapse = "\n")
  close(file_con)
  
  # encoding method 2 (let R work it out - works in most cases):
  file_con <- file(filename)
  sql2 <- paste(readLines(file_con, warn = FALSE), collapse = "\n")
  close(file_con)
  
  # choose between the two encodings, based on which one has a legitimate string length:
  suppressWarnings({
    if(is.na(stringr::str_length(sql2))){
      sql <- sql1
    } else {
      sql <- sql2
    }
  })
  
  # Strip out comment blocks
  sql <- gsub("\\/\\*.*?\\*\\/", "", sql)
  
  # Strip out trailing GO
  sql <- gsub("[Gg][Oo] *?$", "", sql)
  
  # do the find and replace that are needed
  if(!is.null(sub_in)){
    sql <- gsub(sub_in, sub_out, sql, fixed = fixed)
  }
  
  # split the SQL into separate commands wherever there is a "GO" at the beginning of a line
  # ("GO" is not ANSI SQL, only works for SQL Server - it indicates the lines above are a batch)
  sql_split <- stringr::str_split(sql, "\\n *[Gg][Oo]", simplify = TRUE)
  
  base_log_entry <- data.frame(
    sub_out         = ifelse(is.null(sub_out), "none", sub_out),
    script_name      = filename,
    stringsAsFactors = FALSE
  )
  
  n_batches <- length(sql_split)
  
  # execute the various separate commands
  for(i in 1:n_batches){
    log_entry              <- base_log_entry
    log_entry$batch_number <- i
    log_entry$result       <- "no error"
    log_entry$err_mess     <- ""
    log_entry$start_time   <- datetime_ch(Sys.time())
    
    if(verbose){message(paste("Executing batch", i, "of", n_batches))}
    
    duration <- system.time({res <- DBI::dbGetQuery(channel, sql_split[[i]])})
    log_entry$duration <- duration[3]
    
    if(class(res) == "data.frame"){
      txt <- paste("Downloaded a data.frame with", nrow(res), "rows and",
                   ncol(res), "columns in batch", i, ". Any commands left in batch", i, "were not run.")
      if(verbose){message(txt)}
      log_entry$result <- "data.frame"
      
    } 
    if(class(res) == "character" & length(res) > 0){
      message("\n\nI got this error message:")
      cat(res)
      log_entry$result <- "error"
      log_entry$err_mess <- paste(gsub("'", "", res), collapse = "\n")
      message(paste0("\n\nSomething went wrong with the SQL execution of batch ", i, 
                     " in ", filename, ". \n\nError message from the database is shown above\n\n"))
    }
    
    log_entry$end_time <- datetime_ch(Sys.time())
    
    # Update the log in the database, if we have been given one:
    if(!is.null(log_table)){
      # couldn't get sqlSave to append to a table even when append = TRUE... 
      # see https://stackoverflow.com/questions/36913664/rodbc-error-sqlsave-unable-to-append-to-table
      # so am writing the SQL to update the log by hand:
      sql <- with(log_entry, paste0("INSERT INTO ", 
                                    log_table, 
                                    "(start_time, end_time, sub_out, script_name, batch_number, 
                                    result, err_mess, duration, ruser)",
                                    " VALUES (", start_time, ", ", end_time, ", '", 
                                    sub_out, "', '", script_name, "', ", batch_number, ", '", result, "', '",
                                    err_mess, "', ", duration, ", '", as.character(Sys.info()["user"]), "');"))
      
      log_res <- dbGetQuery(channel, sql)
      
      
    }
    if(error_action == "stop" && log_entry$result == "error"){
      stop(paste("Stopping due to an error in", filename))
    }
    if(class(res) == "data.frame"){
      if(i == n_batches){
        return(res)
      } else {
        if(verbose){
          warning("Downloaded a data frame from batch ", i, " of SQL, which wasn't the \nlast batch in the file.  This data frame is not kept.")
        }
      }
      
    }
  }
}
