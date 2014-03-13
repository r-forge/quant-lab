\name{PortfolioValue}
\alias{PortfolioValue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PortfolioValue(param = c("premium", "delta", "vega", "theta", "gamma"), S = 0, T = 0, r = 0, b = 0, tol = 1e-05, vola.vv = c(ATM = NULL, RR25 = NULL, BF25 = NULL), vola.scen = list(ATM = data.frame(x = NULL, d = NULL), RR25 = data.frame(x = NULL, d = NULL), BF25 = data.frame(x = NULL, d = NULL)), dS = S/1000, port = data.frame(type = NULL, strike = NULL, vola = NULL, quant = NULL))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{param}{
%%     ~~Describe \code{param} here~~
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
  \item{tol}{
%%     ~~Describe \code{tol} here~~
}
  \item{vola.vv}{
%%     ~~Describe \code{vola.vv} here~~
}
  \item{vola.scen}{
%%     ~~Describe \code{vola.scen} here~~
}
  \item{dS}{
%%     ~~Describe \code{dS} here~~
}
  \item{port}{
%%     ~~Describe \code{port} here~~
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
function (param = c("premium", "delta", "vega", "theta", "gamma"), 
    S = 0, T = 0, r = 0, b = 0, tol = 1e-05, vola.vv = c(ATM = NULL, 
        RR25 = NULL, BF25 = NULL), vola.scen = list(ATM = data.frame(x = NULL, 
        d = NULL), RR25 = data.frame(x = NULL, d = NULL), BF25 = data.frame(x = NULL, 
        d = NULL)), dS = S/1000, port = data.frame(type = NULL, 
        strike = NULL, vola = NULL, quant = NULL)) 
{
    {
        if (is.null(port$vola) & is.null(vola.vv)) {
            err <- "Please define port$vola or vola.vv"
            return(list(err = err, value = NA))
        }
        if (nrow(port) < 1) {
            err <- "Portfolio is empty"
            return(list(err = err, value = 0))
        }
        if (!is.null(vola.scen$ATM) && nrow(vola.scen$ATM) == 
            0) 
            vola.scen$ATM <- NULL
        if (!is.null(vola.scen$RR25) && nrow(vola.scen$RR25) == 
            0) 
            vola.scen$RR25 <- NULL
        if (!is.null(vola.scen$BF25) && nrow(vola.scen$BF25) == 
            0) 
            vola.scen$BF25 <- NULL
        if (is.null(vola.scen$ATM) & is.null(vola.scen$RR25) & 
            is.null(vola.scen$BF25)) 
            vola.scen <- NULL
    }
    if (param == "premium") {
        value <- 0
        for (i in 1:nrow(port)) {
            if (port$type[i] == "u") {
                value <- value + port$quant[i] * S
            }
            else {
                if (is.null(vola.vv)) {
                  if (is.null(vola.scen$ATM)) 
                    ATM.d <- 0
                  else ATM.d <- CutSplineFun(vola.scen$ATM, S)
                  vola <- port$vola[i] + ATM.d
                }
                else {
                  if (is.null(vola.scen$ATM)) 
                    ATM.d <- 0
                  else ATM.d <- CutSplineFun(vola.scen$ATM, S)
                  if (is.null(vola.scen$RR25)) 
                    RR25.d <- 0
                  else RR25.d <- CutSplineFun(vola.scen$RR25, 
                    S)
                  if (is.null(vola.scen$BF25)) 
                    BF25.d <- 0
                  else BF25.d <- CutSplineFun(vola.scen$BF25, 
                    S)
                  ATM.vol <- vola.vv[["ATM"]] + ATM.d
                  C25.vol <- (vola.vv[["RR25"]] + RR25.d)/2 + 
                    (vola.vv[["BF25"]] + BF25.d) + ATM.vol
                  P25.vol <- C25.vol - (vola.vv[["RR25"]] + RR25.d)
                  ATM.K <- S
                  C25.K <- StrikeDeltaConv(delta = 0.25, S = S, 
                    T = T, r = r, b = b, vola = C25.vol)
                  P25.K <- StrikeDeltaConv(delta = -0.25, S = S, 
                    T = T, r = r, b = b, vola = P25.vol)
                  vola <- VannaVolgaVol.cmp(method = "continuous", 
                    S = S, K = port$strike[i], T = T, vol0 = ATM.vol, 
                    vola = data.frame(K = c(P25.K, ATM.K, C25.K), 
                      v = c(P25.vol, ATM.vol, C25.vol)), r = r, 
                    b = b, tol = tol)
                }
                p <- GreeksBSM(name = "premium", type = port$type[i], 
                  S = S, K = port$strike[i], T = T, r = r, b = b, 
                  vola = vola)
                if (p$err == 0) 
                  p <- p$value
                else return(list(err = p$err, value = NA))
                value <- value + p * port$quant[i]
            }
        }
        return(list(err = 0, value = value))
    }
    if (param == "delta") {
        value <- 0
        for (i in 1:nrow(port)) {
            if (port$type[i] == "u") {
                value <- value + port$quant[i]
            }
            else {
                if (is.null(vola.vv)) {
                  if (is.null(vola.scen$ATM)) 
                    ATM.d <- 0
                  else ATM.d <- CutSplineFun(vola.scen$ATM, S)
                  vola <- port$vola[i] + ATM.d
                  p <- GreeksBSM(name = "delta", type = port$type[i], 
                    S = S, K = port$strike[i], T = T, r = r, 
                    b = b, vola = vola)
                  if (p$err == 0) 
                    p <- p$value
                  else return(list(err = p$err, value = NA))
                }
                else {
                  {
                    if (is.null(vola.scen$ATM)) 
                      ATM.d <- 0
                    else ATM.d <- CutSplineFun(vola.scen$ATM, 
                      S - dS)
                    if (is.null(vola.scen$RR25)) 
                      RR25.d <- 0
                    else RR25.d <- CutSplineFun(vola.scen$RR25, 
                      S - dS)
                    if (is.null(vola.scen$BF25)) 
                      BF25.d <- 0
                    else BF25.d <- CutSplineFun(vola.scen$BF25, 
                      S - dS)
                    ATM.vol <- vola.vv[["ATM"]] + ATM.d
                    C25.vol <- (vola.vv[["RR25"]] + RR25.d)/2 + 
                      (vola.vv[["BF25"]] + BF25.d) + ATM.vol
                    P25.vol <- C25.vol - (vola.vv[["RR25"]] + 
                      RR25.d)
                    ATM.K <- S - dS
                    C25.K <- StrikeDeltaConv(delta = 0.25, S = S - 
                      dS, T = T, r = r, b = b, vola = C25.vol)
                    P25.K <- StrikeDeltaConv(delta = -0.25, S = S - 
                      dS, T = T, r = r, b = b, vola = P25.vol)
                    vola <- VannaVolgaVol.cmp(method = "continuous", 
                      S = S - dS, K = port$strike[i], T = T, 
                      vol0 = ATM.vol, vola = data.frame(K = c(P25.K, 
                        ATM.K, C25.K), v = c(P25.vol, ATM.vol, 
                        C25.vol)), r = r, b = b, tol = tol)
                    p1 <- GreeksBSM(name = "premium", type = port$type[i], 
                      S = S - dS, K = port$strike[i], T = T, 
                      r = r, b = b, vola = vola)
                    if (p1$err == 0) 
                      p1 <- p1$value
                    else return(list(err = p1$err, value = NA))
                  }
                  {
                    if (is.null(vola.scen$ATM)) 
                      ATM.d <- 0
                    else ATM.d <- CutSplineFun(vola.scen$ATM, 
                      S + dS)
                    if (is.null(vola.scen$RR25)) 
                      RR25.d <- 0
                    else RR25.d <- CutSplineFun(vola.scen$RR25, 
                      S + dS)
                    if (is.null(vola.scen$BF25)) 
                      BF25.d <- 0
                    else BF25.d <- CutSplineFun(vola.scen$BF25, 
                      S + dS)
                    ATM.vol <- vola.vv[["ATM"]] + ATM.d
                    C25.vol <- (vola.vv[["RR25"]] + RR25.d)/2 + 
                      (vola.vv[["BF25"]] + BF25.d) + ATM.vol
                    P25.vol <- C25.vol - (vola.vv[["RR25"]] + 
                      RR25.d)
                    ATM.K <- S + dS
                    C25.K <- StrikeDeltaConv(delta = 0.25, S = S + 
                      dS, T = T, r = r, b = b, vola = C25.vol)
                    P25.K <- StrikeDeltaConv(delta = -0.25, S = S + 
                      dS, T = T, r = r, b = b, vola = P25.vol)
                    vola <- VannaVolgaVol.cmp(method = "continuous", 
                      S = S + dS, K = port$strike[i], T = T, 
                      vol0 = ATM.vol, vola = data.frame(K = c(P25.K, 
                        ATM.K, C25.K), v = c(P25.vol, ATM.vol, 
                        C25.vol)), r = r, b = b, tol = tol)
                    p2 <- GreeksBSM(name = "premium", type = port$type[i], 
                      S = S + dS, K = port$strike[i], T = T, 
                      r = r, b = b, vola = vola)
                    if (p2$err == 0) 
                      p2 <- p2$value
                    else return(list(err = p2$err, value = NA))
                  }
                  p <- (p2 - p1)/(2 * dS)
                }
                value <- value + p * port$quant[i]
            }
        }
        return(list(err = 0, value = value))
    }
    if (param == "vega") {
        value <- 0
        for (i in 1:nrow(port)) {
            if (port$type[i] != "u") {
                if (is.null(vola.vv)) {
                  if (is.null(vola.scen$ATM)) 
                    ATM.d <- 0
                  else ATM.d <- CutSplineFun(vola.scen$ATM, S)
                  vola <- port$vola[i] + ATM.d
                  p <- GreeksBSM(name = "vega", type = port$type[i], 
                    S = S, K = port$strike[i], T = T, r = r, 
                    b = b, vola = vola)
                  if (p$err == 0) 
                    p <- p$value
                  else return(list(err = p$err, value = NA))
                }
                else {
                  if (is.null(vola.scen$ATM)) 
                    ATM.d <- 0
                  else ATM.d <- CutSplineFun(vola.scen$ATM, S)
                  if (is.null(vola.scen$RR25)) 
                    RR25.d <- 0
                  else RR25.d <- CutSplineFun(vola.scen$RR25, 
                    S)
                  if (is.null(vola.scen$BF25)) 
                    BF25.d <- 0
                  else BF25.d <- CutSplineFun(vola.scen$BF25, 
                    S)
                  ATM.vol <- vola.vv[["ATM"]] + ATM.d
                  C25.vol <- (vola.vv[["RR25"]] + RR25.d)/2 + 
                    (vola.vv[["BF25"]] + BF25.d) + ATM.vol
                  P25.vol <- C25.vol - (vola.vv[["RR25"]] + RR25.d)
                  ATM.K <- S
                  C25.K <- StrikeDeltaConv(delta = 0.25, S = S, 
                    T = T, r = r, b = b, vola = C25.vol)
                  P25.K <- StrikeDeltaConv(delta = -0.25, S = S, 
                    T = T, r = r, b = b, vola = P25.vol)
                  vola <- VannaVolgaVol.cmp(method = "continuous", 
                    S = S, K = port$strike[i], T = T, vol0 = ATM.vol, 
                    vola = data.frame(K = c(P25.K, ATM.K, C25.K), 
                      v = c(P25.vol, ATM.vol, C25.vol)), r = r, 
                    b = b, tol = tol)
                  p <- GreeksBSM(name = "vega", type = port$type[i], 
                    S = S, K = port$strike[i], T = T, r = r, 
                    b = b, vola = vola)
                  if (p$err == 0) 
                    p <- p$value
                  else return(list(err = p$err, value = NA))
                }
                value <- value + p * port$quant[i]
            }
        }
        return(list(err = 0, value = value))
    }
    if (param == "theta") {
        value <- 0
        for (i in 1:nrow(port)) {
            if (port$type[i] != "u") {
                if (is.null(vola.vv)) {
                  if (is.null(vola.scen$ATM)) 
                    ATM.d <- 0
                  else ATM.d <- CutSplineFun(vola.scen$ATM, S)
                  vola <- port$vola[i] + ATM.d
                  p <- GreeksBSM(name = "theta", type = port$type[i], 
                    S = S, K = port$strike[i], T = T, r = r, 
                    b = b, vola = vola)
                  if (p$err == 0) 
                    p <- p$value
                  else return(list(err = p$err, value = NA))
                }
                else {
                  if (is.null(vola.scen$ATM)) 
                    ATM.d <- 0
                  else ATM.d <- CutSplineFun(vola.scen$ATM, S)
                  if (is.null(vola.scen$RR25)) 
                    RR25.d <- 0
                  else RR25.d <- CutSplineFun(vola.scen$RR25, 
                    S)
                  if (is.null(vola.scen$BF25)) 
                    BF25.d <- 0
                  else BF25.d <- CutSplineFun(vola.scen$BF25, 
                    S)
                  ATM.vol <- vola.vv[["ATM"]] + ATM.d
                  C25.vol <- (vola.vv[["RR25"]] + RR25.d)/2 + 
                    (vola.vv[["BF25"]] + BF25.d) + ATM.vol
                  P25.vol <- C25.vol - (vola.vv[["RR25"]] + RR25.d)
                  ATM.K <- S
                  C25.K <- StrikeDeltaConv(delta = 0.25, S = S, 
                    T = T, r = r, b = b, vola = C25.vol)
                  P25.K <- StrikeDeltaConv(delta = -0.25, S = S, 
                    T = T, r = r, b = b, vola = P25.vol)
                  vola <- VannaVolgaVol.cmp(method = "continuous", 
                    S = S, K = port$strike[i], T = T, vol0 = ATM.vol, 
                    vola = data.frame(K = c(P25.K, ATM.K, C25.K), 
                      v = c(P25.vol, ATM.vol, C25.vol)), r = r, 
                    b = b, tol = tol)
                  p <- GreeksBSM(name = "theta", type = port$type[i], 
                    S = S, K = port$strike[i], T = T, r = r, 
                    b = b, vola = vola)
                  if (p$err == 0) 
                    p <- p$value
                  else return(list(err = p$err, value = NA))
                }
                value <- value + p * port$quant[i]
            }
        }
        return(list(err = 0, value = value))
    }
    if (param == "gamma") {
        value <- 0
        for (i in 1:nrow(port)) {
            if (port$type[i] != "u") {
                if (is.null(vola.vv)) {
                  if (is.null(vola.scen$ATM)) 
                    ATM.d <- 0
                  else ATM.d <- CutSplineFun(vola.scen$ATM, S)
                  vola <- port$vola[i] + ATM.d
                  p <- GreeksBSM(name = "gamma", type = port$type[i], 
                    S = S, K = port$strike[i], T = T, r = r, 
                    b = b, vola = vola)
                  if (p$err == 0) 
                    p <- p$value
                  else return(list(err = p$err, value = NA))
                }
                else {
                  if (is.null(vola.scen$ATM)) 
                    ATM.d <- 0
                  else ATM.d <- CutSplineFun(vola.scen$ATM, S)
                  if (is.null(vola.scen$RR25)) 
                    RR25.d <- 0
                  else RR25.d <- CutSplineFun(vola.scen$RR25, 
                    S)
                  if (is.null(vola.scen$BF25)) 
                    BF25.d <- 0
                  else BF25.d <- CutSplineFun(vola.scen$BF25, 
                    S)
                  ATM.vol <- vola.vv[["ATM"]] + ATM.d
                  C25.vol <- (vola.vv[["RR25"]] + RR25.d)/2 + 
                    (vola.vv[["BF25"]] + BF25.d) + ATM.vol
                  P25.vol <- C25.vol - (vola.vv[["RR25"]] + RR25.d)
                  ATM.K <- S
                  C25.K <- StrikeDeltaConv(delta = 0.25, S = S, 
                    T = T, r = r, b = b, vola = C25.vol)
                  P25.K <- StrikeDeltaConv(delta = -0.25, S = S, 
                    T = T, r = r, b = b, vola = P25.vol)
                  vola <- VannaVolgaVol.cmp(method = "continuous", 
                    S = S, K = port$strike[i], T = T, vol0 = ATM.vol, 
                    vola = data.frame(K = c(P25.K, ATM.K, C25.K), 
                      v = c(P25.vol, ATM.vol, C25.vol)), r = r, 
                    b = b, tol = tol)
                  p <- GreeksBSM(name = "gamma", type = port$type[i], 
                    S = S, K = port$strike[i], T = T, r = r, 
                    b = b, vola = vola)
                  if (p$err == 0) 
                    p <- p$value
                  else return(list(err = p$err, value = NA))
                }
                value <- value + p * port$quant[i]
            }
        }
        return(list(err = 0, value = value))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
