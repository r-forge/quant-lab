# Quant-lab functions

##### libraries #####

list.of.packages <- c("fOptions", "RPostgreSQL")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
sapply(list.of.packages, require, character.only=TRUE)
rm(list.of.packages, new.packages)



##### options #####

#options(stringsAsFactors = FALSE)



##### GreeksBSM #####

# Function calculates option premium and sensitivities,
# setting b = r we get Black and Scholes' stock option model,
# b = r-q we get Merton's stock option model with continuous dividend yield q,
# b = 0 we get Black's futures option model,
# and b = r-rf we get Garman and Kohlhagen's currency option model with 
# foreign interest rate rf

GreeksBSM <- function(
  name  = c("premium", "delta", "vega", "theta", "rho", "gamma", "vanna", "volga"),
  type = c("c", "p"),
  S = 0,
  K = 0,
  T = 0,
  r = 0,
  b = 0,
  vola = 0
)
{

  c.type <- c("premium", "delta", "vega", "theta", "rho", "gamma", "vanna", "volga")
  
  {# check input params
  
  if(type == "c" | type == "C" | type == "call" | type == "Call") {
    i <- 1
  } else {
    if(type == "p" | type == "P" | type == "put" | type == "Put") {
      i <- -1
    } else {
      err <- "Type out of c(c, p, call, put, Call, Put)"
      return(list(err=err, value=NA))
    }
  }
  
  if(!(name %in% c.type)) {
    err <- "Name out of c(premium, delta, vega, theta, rho, gamma, vanna, volga)"
    return(list(err=err, value=NA))
  }
  
  if(S <= 0) {
    err <- "S must have a positive value"
    return(list(err=err, value=NA))
  }
  
  if(K <= 0) {
    err <- "K must have a positive value"
    return(list(err=err, value=NA))
  }
  
  if(T <= 0) {
    out <- 0
    if(name == "premium") out <- i * (S - K)
    if(name == "delta") out <- i
    return(list(err=0, value=out))
    #err <- "T must have a positive value"
    #return(list(err=err, value=NA))
  }
  
  if(vola <= 0) {
    err <- "vola must have a positive value"
    return(list(err=err, value=NA))
  }
  
  }
  
  q <- r - b
  d1 <- (log(S/K) + (r - q + vola*vola/2) * T)/(vola * sqrt(T))
  #d2 <- d1 - vola * sqrt(T)
  d2 <- (log(S/K) + (r - q - vola*vola/2) * T)/(vola * sqrt(T))
  
  if(name == "premium") {
    out <- i * (S * exp(-q*T) * pnorm(i*d1) - K * exp(-r*T) * pnorm(i*d2))
    return(list(err=0, value=out))
  }
  
  if(name == "delta") {
    out <- i*exp(-q*T) * pnorm(i*d1)
    return(list(err=0, value=out))
  }
  
  if(name == "vega") {
    out <- S * exp(-q*T) * dnorm(d1) * sqrt(T)
    return(list(err=0, value=out))
  }
  
  if(name == "theta") {
    out <- -exp(-q*T) * S*dnorm(d1)*vola/(2*sqrt(T)) - i*r*K*exp(-r*T)*pnorm(i*d2) +
      i*q*S*exp(-q*T)*pnorm(i*d1)
    return(list(err=0, value=out))
  }
  
  if(name == "rho") {
    out <- i*K * T * exp(-r*T) * pnorm(i*d2)
    return(list(err=0, value=out))
  }
  
  if(name == "gamma") {
    out <- exp(-q*T) * dnorm(d1)/(S*vola*sqrt(T))
    return(list(err=0, value=out))
  }
  
  if(name == "vanna") {
    out <- -exp(-q*T) * dnorm(d1) * d2/vola
    return(list(err=0, value=out))
  }
  
  if(name == "volga") {
    out <- S * exp(-q*T) * dnorm(d1) * sqrt(T) * (d1*d2)/vola
    return(list(err=0, value=out))
  }

}



##### StrikeDeltaConv #####

# Calculates the strike price of an option with given values of the delta,
# underlying price, implied volatility,...

StrikeDeltaConv <- function(
  delta = 0,
  S = 0,
  T = 0,
  r = 0,
  b = 0,
  vola = 0
)
{

  q <- r - b
  
  if(delta > 0) {
    d1 <- qnorm(delta * exp(q * T))
  } else {
    d1 <- -qnorm(-delta * exp(q * T))
  }
  
  return(S * exp(-d1 * vola * sqrt(T) + (r - q + vola*vola/2) * T))

}



##### VannaVolgaVol #####

# The vanna-volga method to calculate implied volatility of an option

VannaVolgaVol <- function(
  method = c("continuous", "discrete"),
  S = 0,
  K = 0,
  T = 0,
  vol0 = 0,
  vola = data.frame(
    K = c(0, 0, 0),
    v = c(0, 0, 0)),
  r = 0,
  b = 0,
  tol = 1e-5
)
{
  
  if(method == "continuous") {
  
    C <- c(function(vol=vol0, spot=S) GreeksBSM(name="premium", type="c", S=spot, K=vola$K[1], T=T, 
      r=r, b=b, vola=vol)$value, function(vol=vol0, spot=S) GreeksBSM(name="premium", type="c", S=spot, 
      K=vola$K[2], T=T, r=r, b=b, vola=vol)$value, function(vol=vol0, spot=S) GreeksBSM(name="premium",
      type="c", S=spot, K=vola$K[3], T=T, r=r, b=b, vola=vol)$value)
    
    TV.BS <- GreeksBSM(name="premium", type="c", S=S, K=K, T=T, r=r, b=b, vola=vol0)$value
    
    B.vega <- sapply(1:3, function(i) GreeksBSM(name="vega", type="c", S=S, K=vola$K[i], T=T,
      r=r, b=b, vola=vol0)$value)
    B.vanna <- sapply(1:3, function(i) GreeksBSM(name="vanna", type="c", S=S, K=vola$K[i], T=T,
      r=r, b=b, vola=vol0)$value)
    B.volga <- sapply(1:3, function(i) GreeksBSM(name="volga", type="c", S=S, K=vola$K[i], T=T,
      r=r, b=b, vola=vol0)$value)
    
    O.vega <- GreeksBSM(name="vega", type="c", S=S, K=K, T=T, r=r, b=b, vola=vol0)$value
    O.vanna <- GreeksBSM(name="vanna", type="c", S=S, K=K, T=T, r=r, b=b, vola=vol0)$value
    O.volga <- GreeksBSM(name="volga", type="c", S=S, K=K, T=T, r=r, b=b, vola=vol0)$value

    B.cost <- sapply(1:3, function(i) C[[i]](vol=vola$v[i]) - C[[i]](vol=vol0))
    
    A <- t(matrix(c(B.vega, B.vanna, B.volga), nrow = 3))
    x <- matrix(c(O.vega, O.vanna, O.volga), nrow = 3)
    w <- solve(A, x)
    CF <- t(w) %*% matrix(B.cost, nrow = 3)
    
    vola <- GBSVolatility(price=TV.BS+CF, TypeFlag="c", S=S, X=K, Time=T, r=r, b=b, tol=tol)
    
    return(vola)
  
  }
  
  if(method == "discrete") {
  
    dS <- S * 0.001
    dvol <- vol0 * 0.01
    
    C <- c(function(vol=vol0, spot=S) GBSOption(TypeFlag="c", S=spot, X=vola$K[1], Time=T, r=r, 
      b=b, sigma=vol)@price, function(vol=vol0, spot=S) GBSOption(TypeFlag="c", S=spot, X=vola$K[2],
      Time=T, r=r, b=b, sigma=vol)@price, function(vol=vol0, spot=S) GBSOption(TypeFlag="c", S=spot,
      X=vola$K[3], Time=T, r=r, b=b, sigma=vol)@price)
    
    Vega <- function(f, vol, spot=S) (f(vol+dvol, spot) - f(vol-dvol, spot))/(2*dvol)
    Vanna <- function(f, vol, spot=S) (Vega(f, vol, spot+dS) - Vega(f, vol, spot-dS))/(2*dS)
    Volga <- function(f, vol) (Vega(f, vol+dvol) - Vega(f, vol-dvol))/(2*dvol)

    O <- function(vol=vol0, spot=S) GBSOption(TypeFlag="c",  S=spot, X=K, Time=T, r=r, b=b, 
      sigma=vol)@price
    
    TV.BS <- O()
    
    B.vega <- sapply(1:3, function(i) Vega(C[[i]], vol0))
    B.vanna <- sapply(1:3, function(i) Vanna(C[[i]], vol0))
    B.volga <- sapply(1:3, function(i) Volga(C[[i]], vol0))
    
    O.vega <- Vega(O, vol0)
    O.vanna <- Vanna(O, vol0)
    O.volga <- Volga(O, vol0)
    
    B.cost <- sapply(1:3, function(i) C[[i]](vola$v[i]) - C[[i]](vol0))
    
    A <- t(matrix(c(B.vega, B.vanna, B.volga), nrow = 3))
    x <- matrix(c(O.vega, O.vanna, O.volga), nrow = 3)
    w <- solve(A, x)
    CF <- t(w) %*% matrix(B.cost, nrow = 3)
    
    vola <- GBSVolatility(price=TV.BS+CF, TypeFlag="c", S=S, X=K, Time=T, r=r, b=b, tol=tol)
    
    return(vola)
  
  }
  
  return(NA)
  
}



##### CutSplineFun #####

# Spline function

CutSplineFun <- function(
  xy = data.frame(x = NULL, y = NULL),
  x = NULL
)
{
  
  if(nrow(xy) == 0)
    return(0)
  
  if(x < min(xy[, 1]))
    return(xy[xy[, 1] == min(xy[, 1]), 2])
  
  if(x > max(xy[, 1]))
    return(xy[xy[, 1] == max(xy[, 1]), 2])
  
  return(splinefun(xy[, 1], xy[, 2])(x))
  
}



##### PortfolioValue #####

# The Profit & Loss and Greeks calculation of an option portfolio using
# the vanna-volga method and given volatility scenario

PortfolioValue <- function(
  param = c("premium", "delta", "vega", "theta", "gamma"),
  S = 0,
  T = 0,
  r = 0,
  b = 0,
  tol = 1e-5,
  vola.vv = c(ATM = NULL, RR25 = NULL, BF25 = NULL),
  vola.scen = list(ATM=data.frame(x = NULL, d = NULL),
  RR25 = data.frame(x = NULL, d = NULL),
  BF25 = data.frame(x = NULL, d = NULL)),
  dS = S/1000,
  port = data.frame(type = NULL, strike = NULL, vola = NULL, quant = NULL)
)
{

  {# check input params
  
  if(is.null(port$vola) & is.null(vola.vv)) {
    err <- "Please define port$vola or vola.vv"
    return(list(err=err, value=NA))
  }
  
  if(nrow(port) < 1) {
    err <- "Portfolio is empty"
    return(list(err=err, value=0))
  }
  
  if(!is.null(vola.scen$ATM) && nrow(vola.scen$ATM) == 0) vola.scen$ATM <- NULL
  if(!is.null(vola.scen$RR25) && nrow(vola.scen$RR25) == 0) vola.scen$RR25 <- NULL
  if(!is.null(vola.scen$BF25) && nrow(vola.scen$BF25) == 0) vola.scen$BF25 <- NULL
  if(is.null(vola.scen$ATM) & is.null(vola.scen$RR25) & is.null(vola.scen$BF25)) vola.scen <- NULL
  
  }
  
  if(param == "premium") {
    
    value <- 0
    
    for(i in 1:nrow(port)) {
    
      if(port$type[i] == "u") {
      
        value <- value + port$quant[i] * S
        
      } else {
      
        if(is.null(vola.vv)) {
        
          if(is.null(vola.scen$ATM)) ATM.d <- 0 else ATM.d <- CutSplineFun(vola.scen$ATM, S)
          vola <- port$vola[i] + ATM.d
          
        } else {
        
          if(is.null(vola.scen$ATM)) ATM.d <- 0 else ATM.d <- CutSplineFun(vola.scen$ATM, S)
          if(is.null(vola.scen$RR25)) RR25.d <- 0 else RR25.d <- CutSplineFun(vola.scen$RR25, S)
          if(is.null(vola.scen$BF25)) BF25.d <- 0 else BF25.d <- CutSplineFun(vola.scen$BF25, S)
          
          ATM.vol <- vola.vv[["ATM"]] + ATM.d
          C25.vol <- (vola.vv[["RR25"]] + RR25.d)/2 + (vola.vv[["BF25"]] + BF25.d) + ATM.vol
          P25.vol <- C25.vol - (vola.vv[["RR25"]] + RR25.d)
          ATM.K <- S
          C25.K <- StrikeDeltaConv(delta=0.25, S=S, T=T, r=r, b=b, vola=C25.vol)
          P25.K <- StrikeDeltaConv(delta=-0.25, S=S, T=T, r=r, b=b, vola=P25.vol)
          
          vola <- VannaVolgaVol(method="continuous", S=S, K=port$strike[i], T=T, vol0=ATM.vol,
            vola=data.frame(K=c(P25.K, ATM.K, C25.K), v=c(P25.vol, ATM.vol, C25.vol)),
            r=r, b=b, tol=tol)
        
        }
        
        p <- GreeksBSM(name="premium", type=port$type[i], S=S, K=port$strike[i], T=T, r=r, b=b, vola=vola)
        
        if(p$err == 0) p <- p$value else return(list(err=p$err, value=NA))
        
        value <- value + p * port$quant[i]
        
      }
    
    }
    
    return(list(err=0, value=value))
    
  }

  if(param == "delta") {
  
    value <- 0
    
    for(i in 1:nrow(port)) {
    
      if(port$type[i] == "u") {
      
        value <- value + port$quant[i]
        
      } else {
      
        if(is.null(vola.vv)) {
        
          if(is.null(vola.scen$ATM)) ATM.d <- 0 else ATM.d <- CutSplineFun(vola.scen$ATM, S)
          vola <- port$vola[i] + ATM.d
          
          p <- GreeksBSM(name="delta", type=port$type[i], S=S, K=port$strike[i], T=T, r=r, b=b, vola=vola)
          
          if(p$err == 0) p <- p$value else return(list(err=p$err, value=NA))
          
        } else {
        
          {# V(S-dS)
          
          if(is.null(vola.scen$ATM)) ATM.d <- 0 else ATM.d <- CutSplineFun(vola.scen$ATM, S-dS)
          if(is.null(vola.scen$RR25)) RR25.d <- 0 else RR25.d <- CutSplineFun(vola.scen$RR25, S-dS)
          if(is.null(vola.scen$BF25)) BF25.d <- 0 else BF25.d <- CutSplineFun(vola.scen$BF25, S-dS)
          
          ATM.vol <- vola.vv[["ATM"]] + ATM.d
          C25.vol <- (vola.vv[["RR25"]] + RR25.d)/2 + (vola.vv[["BF25"]] + BF25.d) + ATM.vol
          P25.vol <- C25.vol - (vola.vv[["RR25"]] + RR25.d)
          ATM.K <- S-dS
          C25.K <- StrikeDeltaConv(delta=0.25, S=S-dS, T=T, r=r, b=b, vola=C25.vol)
          P25.K <- StrikeDeltaConv(delta=-0.25, S=S-dS, T=T, r=r, b=b, vola=P25.vol)
          
          vola <- VannaVolgaVol(method="continuous", S=S-dS, K=port$strike[i], T=T, vol0=ATM.vol,
            vola=data.frame(K=c(P25.K, ATM.K, C25.K), v=c(P25.vol, ATM.vol, C25.vol)),
            r=r, b=b, tol=tol)

          p1 <- GreeksBSM(name="premium", type=port$type[i], S=S-dS, K=port$strike[i], T=T, r=r, b=b, vola=vola)
          
          if(p1$err == 0) p1 <- p1$value else return(list(err=p1$err, value=NA))
          
          }
          
          {# V(S+dS)
          
          if(is.null(vola.scen$ATM)) ATM.d <- 0 else ATM.d <- CutSplineFun(vola.scen$ATM, S+dS)
          if(is.null(vola.scen$RR25)) RR25.d <- 0 else RR25.d <- CutSplineFun(vola.scen$RR25, S+dS)
          if(is.null(vola.scen$BF25)) BF25.d <- 0 else BF25.d <- CutSplineFun(vola.scen$BF25, S+dS)
          
          ATM.vol <- vola.vv[["ATM"]] + ATM.d
          C25.vol <- (vola.vv[["RR25"]] + RR25.d)/2 + (vola.vv[["BF25"]] + BF25.d) + ATM.vol
          P25.vol <- C25.vol - (vola.vv[["RR25"]] + RR25.d)
          ATM.K <- S+dS
          C25.K <- StrikeDeltaConv(delta=0.25, S=S+dS, T=T, r=r, b=b, vola=C25.vol)
          P25.K <- StrikeDeltaConv(delta=-0.25, S=S+dS, T=T, r=r, b=b, vola=P25.vol)
          
          vola <- VannaVolgaVol(method="continuous", S=S+dS, K=port$strike[i], T=T, vol0=ATM.vol,
            vola=data.frame(K=c(P25.K, ATM.K, C25.K), v=c(P25.vol, ATM.vol, C25.vol)),
            r=r, b=b, tol=tol)

          p2 <- GreeksBSM(name="premium", type=port$type[i], S=S+dS, K=port$strike[i], T=T, r=r, b=b, vola=vola)
          
          if(p2$err == 0) p2 <- p2$value else return(list(err=p2$err, value=NA))
          
          }
          
          p <- (p2 - p1)/(2 * dS)
          
        }
        
        value <- value + p * port$quant[i]
      
      }
    
    }
    
    return(list(err=0, value=value))
  
  }

  if(param == "vega") {
  
    value <- 0
    
    for(i in 1:nrow(port)) {
    
      if(port$type[i] != "u") {
      
        if(is.null(vola.vv)) {
        
          if(is.null(vola.scen$ATM)) ATM.d <- 0 else ATM.d <- CutSplineFun(vola.scen$ATM, S)
          vola <- port$vola[i] + ATM.d
          
          p <- GreeksBSM(name="vega", type=port$type[i], S=S, K=port$strike[i], T=T, r=r, b=b, vola=vola)
          
          if(p$err == 0) p <- p$value else return(list(err=p$err, value=NA))
          
        } else {
        
          if(is.null(vola.scen$ATM)) ATM.d <- 0 else ATM.d <- CutSplineFun(vola.scen$ATM, S)
          if(is.null(vola.scen$RR25)) RR25.d <- 0 else RR25.d <- CutSplineFun(vola.scen$RR25, S)
          if(is.null(vola.scen$BF25)) BF25.d <- 0 else BF25.d <- CutSplineFun(vola.scen$BF25, S)
          
          ATM.vol <- vola.vv[["ATM"]] + ATM.d
          C25.vol <- (vola.vv[["RR25"]] + RR25.d)/2 + (vola.vv[["BF25"]] + BF25.d) + ATM.vol
          P25.vol <- C25.vol - (vola.vv[["RR25"]] + RR25.d)
          ATM.K <- S
          C25.K <- StrikeDeltaConv(delta=0.25, S=S, T=T, r=r, b=b, vola=C25.vol)
          P25.K <- StrikeDeltaConv(delta=-0.25, S=S, T=T, r=r, b=b, vola=P25.vol)
          
          vola <- VannaVolgaVol(method="continuous", S=S, K=port$strike[i], T=T, vol0=ATM.vol,
            vola=data.frame(K=c(P25.K, ATM.K, C25.K), v=c(P25.vol, ATM.vol, C25.vol)),
            r=r, b=b, tol=tol)

          p <- GreeksBSM(name="vega", type=port$type[i], S=S, K=port$strike[i], T=T, r=r, b=b, vola=vola)
          
          if(p$err == 0) p <- p$value else return(list(err=p$err, value=NA))
          
        }
        
        value <- value + p * port$quant[i]
      
      }
    
    }
    
    return(list(err=0, value=value))
  
  }

  if(param == "theta") {
  
    value <- 0
    
    for(i in 1:nrow(port)) {
    
      if(port$type[i] != "u") {
      
        if(is.null(vola.vv)) {
        
          if(is.null(vola.scen$ATM)) ATM.d <- 0 else ATM.d <- CutSplineFun(vola.scen$ATM, S)
          vola <- port$vola[i] + ATM.d
        
          p <- GreeksBSM(name="theta", type=port$type[i], S=S, K=port$strike[i], T=T, r=r, b=b, vola=vola)
          
          if(p$err == 0) p <- p$value else return(list(err=p$err, value=NA))
          
        } else {
        
          if(is.null(vola.scen$ATM)) ATM.d <- 0 else ATM.d <- CutSplineFun(vola.scen$ATM, S)
          if(is.null(vola.scen$RR25)) RR25.d <- 0 else RR25.d <- CutSplineFun(vola.scen$RR25, S)
          if(is.null(vola.scen$BF25)) BF25.d <- 0 else BF25.d <- CutSplineFun(vola.scen$BF25, S)
          
          ATM.vol <- vola.vv[["ATM"]] + ATM.d
          C25.vol <- (vola.vv[["RR25"]] + RR25.d)/2 + (vola.vv[["BF25"]] + BF25.d) + ATM.vol
          P25.vol <- C25.vol - (vola.vv[["RR25"]] + RR25.d)
          ATM.K <- S
          C25.K <- StrikeDeltaConv(delta=0.25, S=S, T=T, r=r, b=b, vola=C25.vol)
          P25.K <- StrikeDeltaConv(delta=-0.25, S=S, T=T, r=r, b=b, vola=P25.vol)
          
          vola <- VannaVolgaVol(method="continuous", S=S, K=port$strike[i], T=T, vol0=ATM.vol,
            vola=data.frame(K=c(P25.K, ATM.K, C25.K), v=c(P25.vol, ATM.vol, C25.vol)),
            r=r, b=b, tol=tol)

          p <- GreeksBSM(name="theta", type=port$type[i], S=S, K=port$strike[i], T=T, r=r, b=b, vola=vola)
          
          if(p$err == 0) p <- p$value else return(list(err=p$err, value=NA))
          
        }
        
        value <- value + p * port$quant[i]
      
      }
    
    }
    
    return(list(err=0, value=value))
  
  }

  if(param == "gamma") {
  
    value <- 0
    
    for(i in 1:nrow(port)) {
    
      if(port$type[i] != "u") {
      
        if(is.null(vola.vv)) {
        
          if(is.null(vola.scen$ATM)) ATM.d <- 0 else ATM.d <- CutSplineFun(vola.scen$ATM, S)
          vola <- port$vola[i] + ATM.d
        
          p <- GreeksBSM(name="gamma", type=port$type[i], S=S, K=port$strike[i], T=T, r=r, b=b, vola=vola)
          
          if(p$err == 0) p <- p$value else return(list(err=p$err, value=NA))
          
        } else {
        
          if(is.null(vola.scen$ATM)) ATM.d <- 0 else ATM.d <- CutSplineFun(vola.scen$ATM, S)
          if(is.null(vola.scen$RR25)) RR25.d <- 0 else RR25.d <- CutSplineFun(vola.scen$RR25, S)
          if(is.null(vola.scen$BF25)) BF25.d <- 0 else BF25.d <- CutSplineFun(vola.scen$BF25, S)
          
          ATM.vol <- vola.vv[["ATM"]] + ATM.d
          C25.vol <- (vola.vv[["RR25"]] + RR25.d)/2 + (vola.vv[["BF25"]] + BF25.d) + ATM.vol
          P25.vol <- C25.vol - (vola.vv[["RR25"]] + RR25.d)
          ATM.K <- S
          C25.K <- StrikeDeltaConv(delta=0.25, S=S, T=T, r=r, b=b, vola=C25.vol)
          P25.K <- StrikeDeltaConv(delta=-0.25, S=S, T=T, r=r, b=b, vola=P25.vol)
          
          vola <- VannaVolgaVol(method="continuous", S=S, K=port$strike[i], T=T, vol0=ATM.vol,
            vola=data.frame(K=c(P25.K, ATM.K, C25.K), v=c(P25.vol, ATM.vol, C25.vol)),
            r=r, b=b, tol=tol)

          p <- GreeksBSM(name="gamma", type=port$type[i], S=S, K=port$strike[i], T=T, r=r, b=b, vola=vola)
          
          if(p$err == 0) p <- p$value else return(list(err=p$err, value=NA))
          
        }
        
        value <- value + p * port$quant[i]
      
      }
    
    }
    
    return(list(err=0, value=value))
  
  }

}



##### configPG #####

configPG <- list(
  user = "postgres",
  password = "postgres",
  dbname = "postgres",
  host = "127.0.0.1",
  port = 5432,
  drv = "PostgreSQL"
)

# configPGAdmin <- list(
#   user = "username_of_admin",
#   password = "password_of_admin",
#   dbname = "MY_DBNAME",
#   host = "url_of_host",
#   port = port_of_host,
#   drv = "PostgreSQL"
# )

# configMySQL <- list(
#   user = "username",
#   password = "password",
#   dbname = "MY_DBNAME",
#   host = "MY_HOST_IP_ADDRESS",
#   drv = "MySQL"
# )

# configWhse <- list(
#   drv = JDBC("oracle.jdbc.OracleDriver", "/usr/lib/oracle/instantclient_11_2/ojdbc5.jar"),
#   user = "username",
#   password = "password",
#   url =  "url"
# )



##### makeCxn #####

# Make a connection to a database
# This function abstracts the idea of a database connection, allowing variable
# parameters depending on the type of database you're connecting to
# @param config a named list of the configuration options for the database connection
# @return a connection to the database defined in the config

makeCxn <- function(config)
{
  
  if(class(config[['drv']]) == "character")
    config[['drv']] <- dbDriver(config[['drv']])
  
  do.call(dbConnect, config)
  
}



##### fetchQuery #####

# This function runs a query on a database, fetching the result if desired
# The purpose of this function is to remove connection management from the querying process
# @param query the query you want to make to the SQL connection you've specified
# @param config a named list of the configuration options for the connection
# @param n the number of rows to return, or -1 for all rows
# @param verbose Should the queries be printed as they're made?
# @param split Should the queries be split on semicolons, or run as a block?
# @return A list of results if multiple queries, or a single result if one query.

fetchQuery <- function(
  query,
  config = configPG,
  split = FALSE,
  verbose = TRUE,
  n = -1
)
{
  
  res <- list()
  cxn <- makeCxn(config)
  t1 <- Sys.time()
  queries <- query
  
  if(split == TRUE)
    queries <- strsplit(query, ";", fixed = TRUE)[[1]] # Split the query into components
  
  for(item in queries) {
    
    if(verbose) {
      cat(paste(item, '\n'))
    }
    
    tmp <- try(dbSendQuery(cxn, item)) # send the query
    
    if ('try-error' %in% class(tmp)) {
      res[[item]] <- dbGetException(cxn)
      next
    }
    
    type <- tolower(substring(gsub(" ", "", item), 0, 6)) # identify if select, insert, delete
    
    if (type == "select" | grepl("with..", type) | grepl('EXPLAI|explai', type) | !split) {
      res[[item]] <- try(fetch(tmp, n))
    } else {
      res[[item]] <- dbGetRowsAffected(tmp)
      cat(res[[item]])
    }
    
    if (verbose) {
      print(Sys.time() - t1)
      if (!is.null(dim(res))) {
        print(dim(res))
      }
    }
    
    dbClearResult(tmp)
    
  }
  
  dbDisconnect(cxn)
  
  if (length(res) == 1) {
    res <- res[[1]]
  }
  
  return(res)
  
}









