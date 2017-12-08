/*
eg-sql.sql

for testing the sql_execute R function

*/

-- this will return five rows but sql_execute discards them
SELECT top 5 * FROM pivot_experiments.dbo.fact_0_million_rows
GO

-- this will return an error
some non-legitimate SQL here that causes an error
go

-- next batch will only get as far as the first seven rows
SELECT top 7 * FROM pivot_experiments.dbo.fact_14_million_rows
SELECT top 10 * FROM pivot_experiments.dbo.fact_14_million_rows
GO

