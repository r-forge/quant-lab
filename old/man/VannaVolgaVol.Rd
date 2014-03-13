\name{VannaVolgaVol}
\alias{VannaVolgaVol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
VannaVolgaVol(method = c("continuous", "discrete"), S = 0, K = 0, T = 0, vol0 = 0, vola = data.frame(K = c(0, 0, 0), v = c(0, 0, 0)), r = 0, b = 0, tol = 1e-05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{method}{
%%     ~~Describe \code{method} here~~
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
  \item{vol0}{
%%     ~~Describe \code{vol0} here~~
}
  \item{vola}{
%%     ~~Describe \code{vola} here~~
}
  \item{r}{
%%     ~~Describe \code{r} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
}
  \item{tol}{
%%     ~~Describe \code{tol} here~~
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
function (method = c("continuous", "discrete"), S = 0, K = 0, 
    T = 0, vol0 = 0, vola = data.frame(K = c(0, 0, 0), v = c(0, 
        0, 0)), r = 0, b = 0, tol = 1e-05) 
{
    if (method == "continuous") {
        C <- c(function(vol = vol0, spot = S) GreeksBSM(name = "premium", 
            type = "c", S = spot, K = vola$K[1], T = T, r = r, 
            b = b, vola = vol)$value, function(vol = vol0, spot = S) GreeksBSM(name = "premium", 
            type = "c", S = spot, K = vola$K[2], T = T, r = r, 
            b = b, vola = vol)$value, function(vol = vol0, spot = S) GreeksBSM(name = "premium", 
            type = "c", S = spot, K = vola$K[3], T = T, r = r, 
            b = b, vola = vol)$value)
        TV.BS <- GreeksBSM(name = "premium", type = "c", S = S, 
            K = K, T = T, r = r, b = b, vola = vol0)$value
        B.vega <- sapply(1:3, function(i) GreeksBSM(name = "vega", 
            type = "c", S = S, K = vola$K[i], T = T, r = r, b = b, 
            vola = vol0)$value)
        B.vanna <- sapply(1:3, function(i) GreeksBSM(name = "vanna", 
            type = "c", S = S, K = vola$K[i], T = T, r = r, b = b, 
            vola = vol0)$value)
        B.volga <- sapply(1:3, function(i) GreeksBSM(name = "volga", 
            type = "c", S = S, K = vola$K[i], T = T, r = r, b = b, 
            vola = vol0)$value)
        O.vega <- GreeksBSM(name = "vega", type = "c", S = S, 
            K = K, T = T, r = r, b = b, vola = vol0)$value
        O.vanna <- GreeksBSM(name = "vanna", type = "c", S = S, 
            K = K, T = T, r = r, b = b, vola = vol0)$value
        O.volga <- GreeksBSM(name = "volga", type = "c", S = S, 
            K = K, T = T, r = r, b = b, vola = vol0)$value
        B.cost <- sapply(1:3, function(i) C[[i]](vol = vola$v[i]) - 
            C[[i]](vol = vol0))
        A <- t(matrix(c(B.vega, B.vanna, B.volga), nrow = 3))
        x <- matrix(c(O.vega, O.vanna, O.volga), nrow = 3)
        w <- solve(A, x)
        CF <- t(w) \%*\% matrix(B.cost, nrow = 3)
        vola <- GBSVolatility(price = TV.BS + CF, TypeFlag = "c", 
            S = S, X = K, Time = T, r = r, b = b, tol = tol)
        return(vola)
    }
    if (method == "discrete") {
        dS <- S * 0.001
        dvol <- vol0 * 0.01
        C <- c(function(vol = vol0, spot = S) GBSOption(TypeFlag = "c", 
            S = spot, X = vola$K[1], Time = T, r = r, b = b, 
            sigma = vol)@price, function(vol = vol0, spot = S) GBSOption(TypeFlag = "c", 
            S = spot, X = vola$K[2], Time = T, r = r, b = b, 
            sigma = vol)@price, function(vol = vol0, spot = S) GBSOption(TypeFlag = "c", 
            S = spot, X = vola$K[3], Time = T, r = r, b = b, 
            sigma = vol)@price)
        Vega <- function(f, vol, spot = S) (f(vol + dvol, spot) - 
            f(vol - dvol, spot))/(2 * dvol)
        Vanna <- function(f, vol, spot = S) (Vega(f, vol, spot + 
            dS) - Vega(f, vol, spot - dS))/(2 * dS)
        Volga <- function(f, vol) (Vega(f, vol + dvol) - Vega(f, 
            vol - dvol))/(2 * dvol)
        O <- function(vol = vol0, spot = S) GBSOption(TypeFlag = "c", 
            S = spot, X = K, Time = T, r = r, b = b, sigma = vol)@price
        TV.BS <- O()
        B.vega <- sapply(1:3, function(i) Vega(C[[i]], vol0))
        B.vanna <- sapply(1:3, function(i) Vanna(C[[i]], vol0))
        B.volga <- sapply(1:3, function(i) Volga(C[[i]], vol0))
        O.vega <- Vega(O, vol0)
        O.vanna <- Vanna(O, vol0)
        O.volga <- Volga(O, vol0)
        B.cost <- sapply(1:3, function(i) C[[i]](vola$v[i]) - 
            C[[i]](vol0))
        A <- t(matrix(c(B.vega, B.vanna, B.volga), nrow = 3))
        x <- matrix(c(O.vega, O.vanna, O.volga), nrow = 3)
        w <- solve(A, x)
        CF <- t(w) \%*\% matrix(B.cost, nrow = 3)
        vola <- GBSVolatility(price = TV.BS + CF, TypeFlag = "c", 
            S = S, X = K, Time = T, r = r, b = b, tol = tol)
        return(vola)
    }
    return(NA)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
