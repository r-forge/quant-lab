\name{StrikeDeltaConv}
\alias{StrikeDeltaConv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculating the strike price for given delta value
}
\description{
Calculates the strike price of an option corresponding given value of the delta
}
\usage{
StrikeDeltaConv(delta = 0, S = 0, T = 0, r = 0, b = 0, vola = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{delta}{
the delta value, a numeric value
}
  \item{S}{
the asset price, a numeric value
}
  \item{T}{
the time to maturity measured in years, a numeric value
}
  \item{r}{
the annualized rate of interest, a numeric value; e.g. 0.25 means 25\% pa
}
  \item{b}{
the annualized cost-of-carry rate, a numeric value; e.g. 0.1 means 10\% pa
}
  \item{vola}{
the annualized volatility of the underlying security
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
Returns strike price, a numeric value
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

c25 <- StrikeDeltaConv(delta = 0.25, S = 35000, T = 0.25, vola = 0.15)
p25 <- StrikeDeltaConv(delta = -0.25, S = 35000, T = 0.25, vola = 0.15)

}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
