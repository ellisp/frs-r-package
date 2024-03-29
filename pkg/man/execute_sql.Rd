% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/execute_sql.R
\name{execute_sql}
\alias{execute_sql}
\title{Execute SQL}
\usage{
execute_sql(
  channel,
  filename,
  sub_in = NULL,
  sub_out = NULL,
  fixed = TRUE,
  error_action = "stop",
  log_table = "dbo.sql_executed_by_r_log",
  verbose = FALSE,
  ...
)
}
\arguments{
\item{channel}{connection handle as returned by RODBC::odbcConnect() of class RODBC}

\item{filename}{file name of an SQL script}

\item{sub_in}{character string that you want to be replaced with \code{sub_out}.  Useful if you want to do a bulk search
and replace.  This is useful if you have a bunch of scripts that you maybe want
to run on one schema sometimes, and on another schema other times - just automate the search and replace.  Use with caution.}

\item{sub_out}{character string that you want to replace \code{sub_in} with.}

\item{fixed}{logical.  If TRUE, \code{sub_in} is a string to be matched as is.  Otherwise it is treated as a regular expression 
(eg if fixed = FALSE, then . is a wild card)}

\item{error_action}{should you stop with an error if a batch gets an error message back from the database?  Any alternative
to "stop" means we just keep ploughing on, which may or may not be a bad idea.  Use "stop" unless you know that failure
in one part of a script isn't fatal.}

\item{log_table}{table in the database to record a log of what happened.  Set to NULL if no log table available.  The log_table
needs to have (at least) the following columns: event_time, sub_out, script_name, batch_number, result, err_mess, duration and ruser. 
See Details for example SQL to create such a log table.}

\item{verbose}{Logical, gives some control over messages}

\item{...}{other arguments to be passed to \code{sqlQuery()}, such as \code{stringsAsFactors = FALSE}.}
}
\description{
Execute T-SQL in a script, split into batches
}
\details{
Reads a script of SQL, splits it into separate queries on the basis of any occurrences of \code{GO}
in the script, and passes it to the database server for execution.  While the initial use case was for SQL Server, there's no
reason why it wouldn't work with other ODBC connections.

The case of \code{GO} is ignored but it has to be the first non-space word on its line of code.

This won't work with SQL scripts that create of temporary tables eg \code{#my_temp_table}; I'm not sure why.
The workaround is to give such tables explicit persistent names eg \code{dbo.my_temp_table} and remember to
clean them up at the end.

If any batch at any point returns rows of data (eg via a \code{SELECT} statement that does not \code{INSERT} the
results into another table or variable on the database), the rest of that batch is not executed.  
If that batch was the last batch of SQL
in the original file, the results are returned as a data.frame, otherwise it is discarded.

Example SQL code for creating a log suitable for this function:
\preformatted{
CREATE TABLE some_database.dbo.sql_executed_by_r_log
(
  log_event_code INT NOT NULL IDENTITY PRIMARY KEY, 
  start_time     DATETIME, 
  end_time       DATETIME,
  sub_text       NVARCHAR(200),
  script_name    NVARCHAR(1000),
  batch_number   INT,
  result         NCHAR(30),
  err_mess       VARCHAR(8000),
  duration       NUMERIC(18, 2),
  sub_out        NVARCHAR(255),
  ruser          NVARCHAR(255),
);
}
}
\examples{
\dontrun{
ch <- dbConnect(odbc::odbc(), "some_dsn_name", database = "some_database")
execute_sql(ch, "some_file.sql")
}
}
\author{
Peter Ellis
}
