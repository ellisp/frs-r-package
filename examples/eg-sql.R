library(pssmisc)
?sql_execute

ch <- odbcConnect("laptop")
sql_execute(ch, "examples/eg-sql.sql", log_table = "pivot_experiments.dbo.sql_executed_by_r_log")
sql_execute(ch, "examples/eg-sql.sql", log_table = "pivot_experiments.dbo.sql_executed_by_r_log", error_action = "continue")
args(sql_execute)
