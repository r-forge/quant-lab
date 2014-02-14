\name{StrikeDeltaConv}
\alias{StrikeDeltaConv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StrikeDeltaConv(delta = 0, S = 0, T = 0, r = 0, b = 0, vola = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{delta}{
%%     ~~Describe \code{delta} here~~
}
  \item{S}{
%%     ~~Describe \code{S} here~~
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
function (delta = 0, S = 0, T = 0, r = 0, b = 0, vola = 0) 
{
    q <- r - b
    if (delta > 0) {
        d1 <- qnorm(delta * exp(q * T))
    }
    else {
        d1 <- -qnorm(-delta * exp(q * T))
    }
    return(S * exp(-d1 * vola * sqrt(T) + (r - q + vola * vola/2) * 
        T))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
