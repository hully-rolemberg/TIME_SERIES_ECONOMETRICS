rm(list=ls())
gc()


# Load package using a function load_package-----------------------------------------------------------------
load_package<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

load_package('tseries')
load_package("AER")
load_package("vars")
load_package("xts")
load_package("tsDyn")
load_package("lmtest")
par(mfrow=c(1,1))

data("PepperPrice")
black <- xts(PepperPrice[,1], order.by = seq(as.Date("1973-10-01"), as.Date("1996-04-01"), by = "month"))
white <- xts(PepperPrice[,2], order.by = seq(as.Date("1973-10-01"), as.Date("1996-04-01"), by = "month"))

# (a) Plot both series in one chart
plot(cbind(black,white))
adf.test(black)
adf.test(white)

# (b) Estimate a VAR and choose the optimal lag
aux <- cbind(black, white)
VARselect(aux)
var <- VAR(aux,2)
summary(var)

# (c) Do a Johansen cointegration test. Do they cointegrate? If yes, estimate a cointegrated VAR.
jotest <- ca.jo(data.frame(aux), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

s <- 1.0000000*black - 0.7419607*white   # estimated cointegration vector
plot(s)
adf.test(s)

vec <- VECM(aux, lag = 2, r=1)
summary(vec)

# (d) Do a Granger causality test 
grangertest(aux)
