\name{configPG}
\alias{configPG}
\title{Postgres Configuration}
\description{List of Parameters Used by RPostgreSQL Package}
\usage{db.table <- fetchQuery(db.query, configPG, verbose=FALSE)}
\arguments{
  \item{user}{DB user name}
  \item{password}{DB password}
  \item{dbname}{DB name}
  \item{host}{DB host}
  \item{port}{DB port}
  \item{drv}{DB driver}
}
\value{list of parameters}
\author{Oleg Mubarakshin}
\examples{
sec.code <- c("SiM4", "EuM4")
db.table <- "futures"
db.col <- paste("seccode", "expdate", "bid", "ask", "lastprice", sep = ",")
db.where <- paste("seccode = ", "'", sec.code, "'", sep = "", collapse = " OR ")
db.query <- paste("SELECT", db.col, "FROM", db.table, "WHERE", db.where)
db.table <- fetchQuery(db.query, configPG, verbose=FALSE)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }
