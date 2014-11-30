\name{configPG}
\alias{configPG}
\docType{data}
\title{Postgres Configuration}
\description{List of Parameters Used by RPostgreSQL Package}
\usage{db.table <- fetchQuery(db.query, configPG, verbose=FALSE)}
\format{
  The format is:
List of 6
 $ user    : chr "postgres"
 $ password: chr "postgres"
 $ dbname  : chr "postgres"
 $ host    : chr "127.0.0.1"
 $ port    : num 5432
 $ drv     : chr "PostgreSQL"
}
\examples{
library(RPostgreSQL)
sec.code <- c("SiM4", "EuM4")
db.table <- "futures"
db.col <- paste("seccode", "expdate", "bid", "ask", "lastprice", sep = ",")
db.where <- paste("seccode = ", "'", sec.code, "'", sep = "", collapse = " OR ")
db.query <- paste("SELECT", db.col, "FROM", db.table, "WHERE", db.where)
db.table <- fetchQuery(db.query, configPG, verbose=FALSE)
}
\keyword{datasets}
