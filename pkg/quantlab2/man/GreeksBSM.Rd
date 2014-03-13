\name{GreeksBSM}
\alias{GreeksBSM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Black-Scholes Premium and Greeks Calculation
}
\description{
Calculates premium of an vanilla option and sensitivities: delta, vega, theta,
rho, gamma, vanna, and volga
}
\usage{
GreeksBSM(
  name = c("premium", "delta", "vega", "theta",
           "rho", "gamma", "vanna", "volga"),
  type = c("c", "p"),
  S = 0, K = 0, T = 0,
  r = 0, b = 0,
  vola = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
the option parameter to be calculated: premium, delta, vega, theta, rho, gamma, vanna, and volga
}
  \item{type}{
a character string either "c" for a call option or a "p" for a put option
}
  \item{S}{
the underlying asset price
}
  \item{K}{
the strike price of an option
}
  \item{T}{
the time to maturity measured in years
}
  \item{r}{
the annualized rate of interest
}
  \item{b}{
the annualized cost-of-carry rate
}
  \item{vola}{
the annualized volatility of the underlying security
}
}
\details{
Function calculates option premium and sensitivities, setting \code{b = r} we get Black and Scholes' stock option model, \code{b = r-q} we get Merton's stock option model with continuous dividend yield \code{q}, \code{b = 0} we get Black's futures option model, and \code{b = r-rf} we get Garman and Kohlhagen's currency option model with foreign interest rate \code{rf}
}
\value{
returns the option premium or sensitivity for the selected Greek
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
GreeksBSM(name = "premium", type = "p", S = 36000, K = 36000,
          T = 0.25, r = 0, b = 0, vola = 0.2)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
