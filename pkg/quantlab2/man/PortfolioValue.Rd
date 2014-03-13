\name{PortfolioValue}
\alias{PortfolioValue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimation of value of an option portfolio
}
\description{
Calculates value (premium) and Greeks of given option portfolio
}
\usage{
PortfolioValue(
  param = c("premium", "delta", "vega", "theta", "gamma"),
  S = 0,
  T = 0,
  r = 0, b = 0,
  tol = 1e-05,
  vola.vv = c(ATM = NULL, RR25 = NULL, BF25 = NULL),
  vola.scen = list(
    ATM = data.frame(x = NULL, d = NULL),
    RR25 = data.frame(x = NULL, d = NULL),
    BF25 = data.frame(x = NULL, d = NULL)
  ),
  dS = S/1000,
  port = data.frame(
    type = NULL,
    strike = NULL,
    vola = NULL,
    quant = NULL)
  )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{param}{
the portfolio parameter to calculation: premium, delta, vega, theta, 
or gamma, character value
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
  \item{tol}{
the tolerance to compute the root of the GBS volatility equation
}
  \item{vola.vv}{
the vanna-volga volatility curve parameters, if it is not NULL the sticky delta method is applied to return value calculation, named vector of numeric values
}
  \item{vola.scen}{
given scenarios of the volatility curve changing according to the underlying price movement, a list of three data frames of numeric values
}
  \item{dS}{
the underlying price augment, used to calculate the delta by the vanna-volga method, a numeric value
}
  \item{port}{
the option portfolio, data frame: \code{type} can be "c" for a call option, "p" for a put option, "u" for underlying asset, \code{strike} is a numeric value, \code{vola} is the implied volatility of an option (it is not used if \code{vola.vv} is not NULL), \code{quant} is quantity, a numeric value
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
The list of a portfolio parameter value (numeric or NA) and an error status (0 if 
no errors).
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
# the sticky strike method 

k <- seq(32500, 36500, 250) 

port <- data.frame(
  type=c("c", "p", "u"),
  strike=c(35500, 34000, 0),
  vola=c(0.1, 0.08, 0),
  quant=c(1000, -1000, -650)
) 

pv <- pn.month <- pv.exp <- NULL 

for(S in k) { 
  
  pv <- c(pv,
    PortfolioValue(
      param="premium",
      S=S, T=0.25, r=0, b=0, tol=1e-5, 
      vola.vv=NULL, vola.scen=NULL,
      dS=10, port=port)$value
  ) 
  
  pn.month <- c(pn.month,
    PortfolioValue(
      param="premium",
      S=S, T=(0.25 - 1/12), r=0, b=0, tol=1e-5,
      vola.vv=NULL, vola.scen=NULL,
      dS=10, port=port)$value
  ) 
 
  pv.exp <- c(pv.exp,
    PortfolioValue(
      param="premium",
      S=S, T=1/365/24/60, r=0, b=0, tol=1e-5,
      vola.vv=NULL, vola.scen=NULL,
      dS=10, port=port)$value
  ) 

} 

S0 <- 34500

pv <- pv - PortfolioValue(param="premium", S=S0, T=0.25, r=0,
  b=0, tol=1e-5, vola.vv=NULL, vola.scen=NULL, dS=10, port=port)$value
 
pn.month <- pn.month - PortfolioValue(param="premium", S=S0, T=0.25, r=0, 
  b=0, tol=1e-5, vola.vv=NULL, vola.scen=NULL, dS=10, port=port)$value 
  
pv.exp <- pv.exp - PortfolioValue(param="premium", S=S0, T=0.25, r=0,
  b=0, tol=1e-5, vola.vv=NULL, vola.scen=NULL, dS=10, port=port)$value 

plot(k, pv, ylim=range(pv, pn.month, pv.exp), type="l", col="red") 
lines(k, pn.month, type="l", col="blue") 
lines(k, pv.exp, type="l", col="black") 
abline(v=S0,h=0, col="grey") 
grid()

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
