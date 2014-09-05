\name{makeCxn}
\alias{makeCxn}
\title{Make a connection to a database}
\description{
This function abstracts the idea of a database connection, allowing variable
parameters depending on the type of database you're connecting to
}
\usage{cxn <- makeCxn(config)}
\arguments{
  \item{param}{
  config a named list of the configuration options for the database connection
  }
}
\value{a connection to the database defined in the config}
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
