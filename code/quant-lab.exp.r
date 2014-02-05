# Function
# VannaVolgaVol

# Description
# The vanna-volga method to calculate implied volatility of a specified option

# Usage
# VannaVolgaVol(method = c("continuous", "discrete"), S = 32440, K = 34000, 
#   T = 0.25, vol0 = 0.07527, vola = data.frame(K = c(32000, 32500, 33000),
#   v = c(0.0725, 0.0775, 0.083)), r = 0, b = 0, tol = 1e-5)

# Arguments
# method	the method to calculate vega, vanna, and volga in vanna-volga method,
#         character value “continuous” or “discrete”
# S	      the asset price, a numeric value
# K	      the strike price, a numeric value
# T	      the time to maturity measured in years, a numeric value
# vol0	  the annualized constant BS implied volatility of the underlying security,
#         a numeric value; e.g. 0.1 means 10% volatility pa
# vola	  data.frame with the strike prices and corresponding volatility levels.
#         Generally use: at the money, 25 delta call, and 25 delta put options
# r	      the annualized rate of interest, a numeric value; e.g. 0.25 means 25% pa
# b	      the annualized cost-of-carry rate, a numeric value; e.g. 0.1 means 10% pa
# tol   	the tolerance to compute the root of the GBS volatility equation

# Value
# The annualized vanna-volga volatility, a numeric value; e.g. 0.1 means 10% pa.

# Examples

# the vanna-volga volatility
VannaVolgaVol(method = "continuous", S = 34570, K = 36500, T = 32/365, 
  vol0 = 0.095, vola = data.frame(K = c(33500, 34500, 35500), 
  v = c(0.085, 0.095, 0.11)), r = 0, b = 0, tol = 1e-5)

# the vanna-volga smile
k <- seq(32500, 36500, 250)
vvv <- NULL
for(i in k)
  vvv <- c(vvv, VannaVolgaVol(method = "continuous", S = 34500, K = i, 
    T = 60/365, vol0 = 0.09, vola = data.frame(K = c(33500, 34500, 35500), 
    v = c(0.085, 0.09, 0.12)), r = 0, b = 0, tol = 1e-5))
plot(k, vvv, type="l")




# Function
# PortfolioValue

# Description
# The profit & loss and Greeks of an option portfolio calculation
# using the vanna-volga method and given volatility scenarios.

# Usage
# PortfolioValue(param = c("premium", "delta", "vega", "theta", "gamma"),
#   S = 33500, T = 0.25, r = 0, b = 0, tol = 1e-5, vola.vv = c(ATM = NULL, RR25 = NULL, BF25 = NULL),
#   vola.scen = list(ATM = data.frame(x = NULL, d = NULL), RR25 = data.frame(x = NULL, d = NULL),
#   BF25 = data.frame(x = NULL, d = NULL)), dS = S/1000,
#   port = data.frame(type = NULL, strike = NULL, vola = NULL, quant = NULL))

# Arguments
# param	    the portfolio parameter to calculation: premium, delta, vega, theta, or gamma, character value
# S	        the asset price, a numeric value
# T	        the time to maturity measured in years, a numeric value
# r	        the annualized rate of interest, a numeric value; e.g. 0.25 means 25% pa
# b	        the annualized cost-of-carry rate, a numeric value; e.g. 0.1 means 10% pa
# tol	      the tolerance to compute the root of the GBS volatility equation
# vola.vv	  the vanna-volga volatility curve parameters, if it is not NULL the sticky delta method 
#           is applied to return value calculation, named vector of numeric values
# vola.scen	given scenarios of the volatility curve changing according to the underlying price movement, 
#           a list of three data frames of numeric values
# dS	      the underlying price augment, used to calculate the delta by the vanna-volga method, a numeric value
# port	    the option portfolio, data frame: type can be "c" for a call option, "p" for a put option,
#           "u" for underlying asset, strike is a numeric value, vola is the implied volatility of an option (it is not used if vola.vv is not NULL), quant is quantity, a numeric value.

# Value
# The list of a portfolio parameter value (numeric or NA) and an error status (0 if no errors).

# Examples
# the sticky strike method
k <- seq(32500, 36500, 250)
port <- data.frame(type=c("c", "p", "u"), strike=c(35500, 34000, 0), vola=c(0.1, 0.08, 0), 
  quant=c(1000, -1000, -650))
pv <- pn.month <- pv.exp <- NULL

for(S in k) {
  pv <- c(pv, PortfolioValue(param="premium", S=S, T=0.25, r=0, b=0, tol=1e-5, vola.vv=NULL,
    vola.scen=NULL, dS=10, port=port)$value)
  pn.month <- c(pn.month, PortfolioValue(param="premium", S=S, T=(0.25 - 1/12), r=0, b=0, 
    tol=1e-5, vola.vv=NULL, vola.scen=NULL, dS=10, port=port)$value)
  pv.exp <- c(pv.exp, PortfolioValue(param="premium", S=S, T=1/365/24/60, r=0, b=0, tol=1e-5, 
    vola.vv=NULL, vola.scen=NULL, dS=10, port=port)$value)
}

S0 <- 34500
pv <- pv - PortfolioValue(param="premium", S=S0, T=0.25, r=0, b=0, tol=1e-5, vola.vv=NULL,
  vola.scen=NULL, dS=10, port=port)$value
pn.month <- pn.month - PortfolioValue(param="premium", S=S0, T=0.25, r=0, b=0, tol=1e-5,
  vola.vv=NULL, vola.scen=NULL, dS=10, port=port)$value
pv.exp <- pv.exp - PortfolioValue(param="premium", S=S0, T=0.25, r=0, b=0, tol=1e-5,
  vola.vv=NULL, vola.scen=NULL, dS=10, port=port)$value

plot(k, pv, ylim=range(pv, pn.month, pv.exp), type="l", col="red")
lines(k, pn.month, type="l", col="blue")
lines(k, pv.exp, type="l", col="black")
abline(v=S0,h=0, col="grey")
grid()



# This example shows applying of the PortfolioValue function with given scenario of 
# volatility curve dynamics. In this case the profit & loss value and the Greeks values of
# an option portfolio are estimated by combined method of the vanna-volga model and the sticky strike rule.

# the vanna-volga parameters
vola.vv <- c(ATM = 0.11, RR25 = 0.02, BF25 = 0.004)

# calculate corresponding implied volatility values
S <- 35037
T <- 49.15/365
ATM.v <- vola.vv[["ATM"]]
C25.v <- vola.vv[["RR25"]]/2 + vola.vv[["BF25"]] + ATM.v
P25.v <- C25.v - vola.vv[["RR25"]]
ATM.K <- S
C25.K <- StrikeDeltaConv(delta = 0.25, S = S, T = T,  r = 0, b = 0, v = C25.v)
P25.K <- StrikeDeltaConv(delta = -0.25, S = S, T = T, r = 0, b = 0, v = P25.v)
k <- seq(33500, 37000, 250)
vola.fix <- NULL
for(i in k)
  vola.fix <- c(vola.fix, VannaVolgaVol(method = "continuous", S = S, K = i, T = T, 
    vol0 = ATM.v, vola = data.frame(K = c(P25.K, ATM.K, C25.K), v = c(P25.v, ATM.v, C25.v)),
    r = 0, b = 0, tol = 1e-5))

# define the volatility scenario:
x <- seq(33500, 37000, 250)
ATM.sc <- data.frame(x = x, d = c(-1.5, -1.2, -0.9, -0.65, -0.4, -0.2, 0, 0.25,
  0.5, 0.75, 1.05, 1.35, 1.65, 2.05, 2.45))
RR25.sc <- data.frame(x = x, d = c(-0.42, -0.33, -0.25, -0.18, -0.11, -0.05, 0, 
  0.07, 0.14, 0.21, 0.3, 0.38, 0.46, 0.57, 0.69))
BF25.sc <- NULL
vola.scen <- list(ATM = ATM.sc, RR25 = RR25.sc, BF25 = BF25.sc)

# define a portfolio:
port <- data.frame(type=c("p", "c", "u"), strike=c(34750, 36000, 0), quant=c(-1200, 1400, -883))
port$vola <- 0
for(i in 1:nrow(port))
  if(port$type[i] != "u")
    port$vola[i] <- vola.fix[port$strike[i] == k]
    
# calculate and plot P&L, delta, and vega profiles: 
# the vanna-volga method with volatility scenario – red line,
# sticky strike method with fixed volatility values – blue line
S <- 35037
T <- 49.15/365
# the vanna-volga parameters
vola.vv <- c(ATM = 0.11, RR25 = 0.02, BF25 = 0.004)

# fixed implied volatility for sticky strike calculations
ATM.v <- vola.vv[["ATM"]]
C25.v <- vola.vv[["RR25"]]/2 + vola.vv[["BF25"]] + ATM.v
P25.v <- C25.v - vola.vv[["RR25"]]
ATM.K <- S
C25.K <- StrikeDeltaConv(delta = 0.25, S = S, T = T,  r = 0, b = 0, v = C25.v)
P25.K <- StrikeDeltaConv(delta = -0.25, S = S, T = T, r = 0, b = 0, v = P25.v)
k <- seq(33500, 37000, 250)
vola.fix <- NULL
for(i in k)
  vola.fix <- c(vola.fix, VannaVolgaVol(method = "continuous", S = S, K = i, T = T,
    vol0 = ATM.v, vola = data.frame(K = c(P25.K, ATM.K, C25.K), v = c(P25.v, ATM.v, C25.v)), r = 0, b = 0, tol = 1e-5))

# define volatility scenario
x <- seq(33500, 37000, 250)
ATM.sc <- data.frame(x = x, d = c(-1.5, -1.2, -0.9, -0.65, -0.4, -0.2, 0, 0.25, 0.5,
  0.75, 1.05, 1.35, 1.65, 2.05, 2.45)/100)
RR25.sc <- data.frame(x = x, d = c(-0.42, -0.33, -0.25, -0.18, -0.11, -0.05, 0, 0.07,
  0.14, 0.21, 0.3, 0.38, 0.46, 0.57, 0.69)/100)
vola.scen <- list(ATM = ATM.sc/2, RR25 = RR25.sc, BF25 = BF25.sc)

# define portfolio
port <- data.frame(type=c("p", "c", "u"), strike=c(34750, 36000, 0), quant=c(-1200, 1400, -883))
port$vola <- 0
for(i in 1:nrow(port))
  if(port$type[i] != "u")
    port$vola[i] <- vola.fix[port$strike[i] == k]

# calculate P&L (sticky delta, sticky strike, to expiry)
pnl.sd <- pnl.ss <- pnl.exp <- NULL
for(i in x) {
  pnl.sd <- c(pnl.sd, PortfolioValue(param="premium", S=i, T=T, r=0, b=0, tol=1e-5,
    vola.vv = vola.vv, vola.scen = vola.scen, dS = 10, port = port)$value)
  pnl.ss <- c(pnl.ss, PortfolioValue(param="premium", S=i, T=T, r=0, b=0, tol=1e-5, 
    vola.vv = NULL, vola.scen = NULL, dS = 10, port = port)$value)
  pnl.exp <- c(pnl.exp, PortfolioValue(param="premium", S=i, T=1e-6, r=0, b=0, tol=1e-5,  
    vola.vv = NULL, vola.scen = NULL, dS = 10, port = port)$value)}

pnl.sd <- pnl.sd - PortfolioValue(param = "premium", S = S, T = T, r = 0, b = 0, tol = 1e-5,
  vola.vv = vola.vv, vola.scen = vola.scen, dS = 10, port = port)$value
pnl.ss <- pnl.ss - PortfolioValue(param = "premium", S = S, T = T, r = 0, b = 0, tol = 1e-5,
  vola.vv = NULL, vola.scen = NULL, dS = 10, port = port)$value
pnl.exp <- pnl.exp - PortfolioValue(param = "premium", S = S, T = 1e-6, r = 0, b = 0, 
  tol = 1e-5, vola.vv = NULL, vola.scen = NULL, dS = 10, port = port)$value

plot(x, pnl.sd, type="l", col="red", main="The P&L values", xlab="Underlying price",
  ylab="Level, RUB", ylim=range(pnl.sd, pnl.ss, pnl.exp))
abline(v=S, h=0, col="grey")
grid()
lines(x, pnl.ss, type="l", col="blue")
lines(x, pnl.exp, type="l", col="grey")

# calculate delta
delta.sd <- delta.ss <- delta.exp <- NULL
for(i in x) {
  delta.sd <- c(delta.sd, PortfolioValue(param="delta", S=i, T=T, r=0, b=0, tol=1e-5,
    vola.vv = vola.vv, vola.scen = vola.scen, dS = 10, port = port)$value)
  delta.ss <- c(delta.ss, PortfolioValue(param="delta", S=i, T=T, r=0, b=0, tol=1e-5,
    vola.vv = NULL, vola.scen = NULL, dS = 10, port = port)$value)
  delta.exp <- c(delta.exp, PortfolioValue(param="delta", S=i, T=1/365, r=0, b=0, tol=1e-5,
    vola.vv = NULL, vola.scen = NULL, dS = 10, port = port)$value)}

plot(x, delta.sd, type="l", col="red", main="The P&L values", xlab="Underlying price", 
  ylab="Level, RUB", ylim=range(delta.sd, delta.ss, delta.exp))
abline(v=S, h=0, col="grey")
grid()
lines(x, delta.ss, type="l", col="blue")
lines(x, delta.exp, type="l", col="grey")

# calculate vega
vega.sd <- vega.ss <- vega.exp <- NULL
for(i in x) {
  vega.sd <- c(vega.sd, PortfolioValue(param="vega", S=i, T=T, r=0, b=0, tol=1e-5, 
    vola.vv = vola.vv, vola.scen = vola.scen, dS = 10, port = port)$value)
  vega.ss <- c(vega.ss, PortfolioValue(param="vega", S=i, T=T, r=0, b=0, tol=1e-5, 
    vola.vv = NULL, vola.scen = NULL, dS = 10, port = port)$value)
  vega.exp <- c(vega.exp, PortfolioValue(param="vega", S=i, T=7/365, r=0, b=0, tol=1e-5,
    vola.vv = NULL, vola.scen = NULL, dS = 10, port = port)$value)}

plot(x, vega.sd, type="l", col="red", main="The P&L values", xlab="Underlying price",
  ylab="Level, RUB", ylim=range(vega.sd, vega.ss, vega.exp))
abline(v=S, h=0, col="grey")
grid()
lines(x, vega.ss, type="l", col="blue")
lines(x, vega.exp, type="l", col="grey")


