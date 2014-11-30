\name{VannaVolgaVol}
\alias{VannaVolgaVol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The vanna-volga method to calculate implied volatility of a specified option
}
\description{
Calculates implied volatility of an option using the vanna-volga method
}
\usage{
VannaVolgaVol(
  method = c("continuous", "discrete"),
  S = 0, K = 0, T = 0,
  vol0 = 0,
  vola = data.frame(K = c(0, 0, 0), v = c(0, 0, 0)),
  r = 0, b = 0,
  tol = 1e-05
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{method}{
the method to calculate vega, vanna, and volga in vanna-volga method, character value "continuous" or "discrete" 
}
  \item{S}{
the asset price, a numeric value
}
  \item{K}{
the strike price, a numeric value
}
  \item{T}{
the time to maturity measured in years, a numeric value
}
  \item{vol0}{
the annualized constant BS implied volatility of the underlying security, a numeric value; e.g. 0.1 means 10\% volatility pa
}
  \item{vola}{
\code{data.frame} with the strike prices and corresponding volatility levels. Generally use: at the money, 25 delta call, and 25 delta put options
}
  \item{r}{
the annualized rate of interest, a numeric value; e.g. 0.25 means 25\% pa
}
  \item{b}{
the annualized cost-of-carry rate, a numeric value; e.g. 0.1 means 10\% pa
}
  \item{tol}{
the tolerance to compute the root of the GBS volatility equation
}
}
\details{}
\value{
The annualized vanna-volga volatility, a numeric value; e.g. 0.1 means 10\% pa.
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Oleg Mubarakshin
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
library(fOptions)
VannaVolgaVol(method = "continuous", S = 35000, K = 37000, T = 0.25, vol0 = 0.11, 
  vola = data.frame(K = c(34000, 35000, 36000), v = c(0.10, 0.11, 0.12)),
  r = 0, b = 0, tol = 1e-05)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
