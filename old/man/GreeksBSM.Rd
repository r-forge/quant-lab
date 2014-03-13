\name{GreeksBSM}
\alias{GreeksBSM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GreeksBSM(name = c("premium", "delta", "vega", "theta", "rho", "gamma", "vanna", "volga"), type = c("c", "p"), S = 0, K = 0, T = 0, r = 0, b = 0, vola = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
%%     ~~Describe \code{name} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{S}{
%%     ~~Describe \code{S} here~~
}
  \item{K}{
%%     ~~Describe \code{K} here~~
}
  \item{T}{
%%     ~~Describe \code{T} here~~
}
  \item{r}{
%%     ~~Describe \code{r} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
}
  \item{vola}{
%%     ~~Describe \code{vola} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (name = c("premium", "delta", "vega", "theta", "rho", 
    "gamma", "vanna", "volga"), type = c("c", "p"), S = 0, K = 0, 
    T = 0, r = 0, b = 0, vola = 0) 
{
    c.type <- c("premium", "delta", "vega", "theta", "rho", "gamma", 
        "vanna", "volga")
    {
        if (type == "c" | type == "C" | type == "call" | type == 
            "Call") {
            i <- 1
        }
        else {
            if (type == "p" | type == "P" | type == "put" | type == 
                "Put") {
                i <- -1
            }
            else {
                err <- "Type out of c(c, p, call, put, Call, Put)"
                return(list(err = err, value = NA))
            }
        }
        if (!(name \%in\% c.type)) {
            err <- "Name out of c(premium, delta, vega, theta, rho, gamma, vanna, volga)"
            return(list(err = err, value = NA))
        }
        if (S <= 0) {
            err <- "S must have a positive value"
            return(list(err = err, value = NA))
        }
        if (K <= 0) {
            err <- "K must have a positive value"
            return(list(err = err, value = NA))
        }
        if (T <= 0) {
            err <- "T must have a positive value"
            return(list(err = err, value = NA))
        }
        if (vola <= 0) {
            err <- "vola must have a positive value"
            return(list(err = err, value = NA))
        }
    }
    q <- r - b
    d1 <- (log(S/K) + (r - q + vola * vola/2) * T)/(vola * sqrt(T))
    d2 <- d1 - vola * sqrt(T)
    if (name == "premium") {
        out <- i * (S * exp(-q * T) * pnorm(i * d1) - K * exp(-r * 
            T) * pnorm(i * d2))
        return(list(err = 0, value = out))
    }
    if (name == "delta") {
        out <- i * exp(-q * T) * pnorm(i * d1)
        return(list(err = 0, value = out))
    }
    if (name == "vega") {
        out <- S * exp(-q * T) * dnorm(d1) * sqrt(T)
        return(list(err = 0, value = out))
    }
    if (name == "theta") {
        out <- -exp(-q * T) * S * dnorm(d1) * vola/(2 * sqrt(T)) - 
            i * r * K * exp(-r * T) * pnorm(i * d2) + i * q * 
            S * exp(-q * T) * pnorm(i * d1)
        return(list(err = 0, value = out))
    }
    if (name == "rho") {
        out <- i * K * T * exp(-r * T) * pnorm(i * d2)
        return(list(err = 0, value = out))
    }
    if (name == "gamma") {
        out <- exp(-q * T) * dnorm(d1)/(S * vola * sqrt(T))
        return(list(err = 0, value = out))
    }
    if (name == "vanna") {
        out <- -exp(-q * T) * dnorm(d1) * d2/vola
        return(list(err = 0, value = out))
    }
    if (name == "volga") {
        out <- S * exp(-q * T) * dnorm(d1) * sqrt(T) * (d1 * 
            d2)/vola
        return(list(err = 0, value = out))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
