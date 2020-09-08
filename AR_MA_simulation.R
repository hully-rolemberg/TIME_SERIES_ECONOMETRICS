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
load_package('openxlsx')

par(mfrow=c(2,2))

# SIMULATING TIME SERIES PROCESS (without packages) AND EVALUTING THEIR MAIN FEATURES

# QUESTION 1 ----

# (a) Simulating Gaussian White Noise  ---
n <- 1000

set.seed(123456)
gwn <- rnorm(n)


analysis <- function(series){
  series <- ts(series)
  plot(series, type = "l")
  hist(series)
  jarque.bera.test(series)
  acf(series)
  pacf(series)
}

analysis(gwn)

# (b) Simulating AR processes --- 

u=gwn

# (i) AR(1) phi=0.1
y=u[1]
for(i in 2:1000){
  y[i] <- 0.1*y[i-1]+u[i] 
}

analysis(y)

# (ii) AR(1) phi=0.95
y=u[1]
for(i in 2:1000){
  y[i] <- 0.95*y[i-1]+u[i] 
}

analysis(y)

# (iii) AR(1) phi=0.1 drift=5
y=5/(1-0.1)
for(i in 2:1000){
  y[i] <- 5 + 0.1*y[i-1]+u[i] 
}

analysis(y)

# (iv) AR(1) phi=0.95 drift=5
y=5/(1-0.95)
for(i in 2:1000){
  y[i] <- 5 + 0.95*y[i-1]+u[i] 
}

analysis(y)

# (v) AR(2) phi_1=0.5 phi_2=0.4
y=u[1:2]
for(i in 3:1000){
  y[i] <- 0.5*y[i-1]+0.4*y[i-2]+u[i] 
}

analysis(y)

# (vi) AR(2) phi_1=0.5 phi_2=-0.4
y=u[1:2]
for(i in 3:1000){
  y[i] <- 0.5*y[i-1]-0.4*y[i-2]+u[i] 
}

analysis(y)

# (vii) random walk
y=u[1]
for(i in 2:1000){
  y[i] <- y[i-1]+u[i] 
}

analysis(y)



# QUESTION 2 ----

# Simulating MA processes 

# (i) MA(1) theta=-0.1
y=u[1]
for(i in 2:1000){
  y[i] <- u[i]-0.1*u[i-1] 
}

analysis(y)

# (ii) MA(1) theta=-0.95
y=u[1]
for(i in 2:1000){
  y[i] <- u[i]-0.95*u[i-1] 
}

analysis(y)

# (iii) MA(1) theta=-0.1 drift=5
y=5
for(i in 2:1000){
  y[i] <- 5+u[i]-0.1*u[i-1] 
}

analysis(y)

# (iv) MA(1) theta=-0.95 drift=5
y=5
for(i in 2:1000){
  y[i] <- 5+u[i]-0.95*u[i-1] 
}

analysis(y)

# (v) MA(3) theta1=0.5 theta=0.3 theta=0.1
y=u[1:3]
for(i in 4:1000){
  y[i] <- 0.5*u[i-1]+0.3*u[i-2]+0.1*u[i-3]+u[i]
}

analysis(y)

# (vi)  MA(3) theta1=0.5 theta=-0.3 theta=-0.1
y=u[1:3]
for(i in 4:1000){
  y[i] <- 0.5*u[i-1]-0.3*u[i-2]-0.1*u[i-3]+u[i]
}

analysis(y)

# (vii) MA(1) theta=1
y=u[1]
for(i in 2:1000){
  y[i] <- u[i]+u[i-1]
}

analysis(y)


# QUESTION 3 ----  
# Non-Stationarity: Deterministic x Stochastic

n <- 1000
par(mfrow=c(1,2))

set.seed(123456)
u <- rnorm(n)
u <- ts(u)

rw<-u[1] # simulating random walk
for(i in 2:1000){
  rw[i] <- rw[i-1]+u[i] 
}
rw <- ts(rw)
plot(rw, main = "Random Walk")

y<-u[1] # simulating trended process
trend <- seq(1:1000)
for(i in 2:1000){
  y[i] <- trend[i]+0.5*(y[i-1]-trend[i-1])+u[i] 
}
y<-ts(y)  
plot(y, main = "Trended Model")

par(mfrow=c(2,2))
acf(rw);pacf(rw)
acf(y);pacf(y)

adf.test(rw)
adf.test(y)