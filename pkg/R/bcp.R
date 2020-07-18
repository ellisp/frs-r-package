
#' bulk upload of a pipe delimited text file to an existing table in a SQL Survey database
#' 
#' @param server address of a SQL Server server eg 'localhost' or a URL
#' @param database name of the database on the server to which to upload the data
#' @param schema name of the schema on the database to which to upload the data
#' @param table name of the (already existing) table on the database to upload the data
#' @param file name of a pipe-delimited text file to upload to the table, with no column headers
#' @param user user name if needed to write to the server
#' @param pwd password if needed to write to the server
#' @param verbose whether to print out the commands sent to the command line
#' @param extra_args extra arguments to pass through to the \code{bcp} utility
#' @param delim delimiter in the text file we are uploading
#' @author Peter Ellis
#' @export
#' @details This is a convenience wrapper for the \code{bcp} function for bulk upload to SQL Server.
#' 
#' Never type your password into a script; use \code{rstudioapi::askForPassword("Database password")} 
#' instead and pass it to the \code{pwd} argument.
#' 
#' I recommend you use something like
#'  \code{data.table:fwrite(your_data, "staging/your_data.txt", sep = "|", col.names = FALSE)}
#' as the fastest way to write your data to a pipe-delimited text file with no column names.
#' 
#' If the columns in the file you have saved don't match those in the table, this operation will fail,
#' possibly with an error, possibly silently. So take care.
bcp <- function(server, database, schema, table, file, user = NULL, pwd = NULL, verbose = FALSE,
                extra_args = " -b 1000000", delim = "|"){
  
  ff_file <- tempfile()
  extra_args <- paste0(" ", stringr::str_squish(extra_args))
  
  # Write a format file
  if(server == "localhost"){
    ff <- paste0('bcp [', 
                  database, '].[', schema, '].[', table, 
                  '] format nul -c -x -f ', 
                 ff_file, 
                 ' -t | -T', extra_args)
    cmd <- paste0('bcp [', 
                  database, '].[', schema, '].[', table, 
                  '] in ', 
                  file, ' -f ',
                  ff_file, 
                  ' -T', extra_args)
    
  } else {
    ff <- paste0('bcp [', 
                 database, '].[', schema, '].[', table, 
                 '] format nul -c -x -f ', 
                 ff_file, 
                 ' -t | -S ', server, ' -U ',
                 user, ' -P ', pwd, extra_args)
    cmd <- paste0('bcp [', 
                  database, '].[', schema, '].[', table, 
                  '] in ', 
                  file, ' -f ',
                  ff_file, 
                  '  -S ', server, ' -U ',
                  user, ' -P ', pwd, extra_args)
  }  
  
  ff <- gsub("|", delim, ff, fixed = TRUE)
  
  if(verbose){print(ff)}
  system(ff, show.output.on.console = verbose)
  
  
  if(verbose){print(cmd)}
  system(cmd, show.output.on.console = verbose)
  
  unlink(ff_file)
}
