\name{fetchQuery}
\alias{fetchQuery}
\title{Runing a query on a database}
\description{
This function runs a query on a database, fetching the result if desired
The purpose of this function is to remove connection management from the querying process
}
\usage{fut.table <- fetchQuery(db.query, configPG, verbose=FALSE)}
\arguments{
  \item{query}{
  the query you want to make to the SQL connection you've specified
  }
  \item{config}{
  a named list of the configuration options for the connection
  }
  \item{n}{
  the number of rows to return, or -1 for all rows
  }
  \item{verbose}{
  Should the queries be printed as they're made?
  }
  \item{split}{
  Should the queries be split on semicolons, or run as a block?
  }
}
\value{A list of results if multiple queries, or a single result if one query}
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
