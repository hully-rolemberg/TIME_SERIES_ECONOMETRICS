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


# Spurious Regression

n <- 1025
set.seed(123456)  
u <- rnorm(n)
v <- rnorm(n)

x<-u[1]
for(i in 2:n){
  x[i] <- x[i-1]+u[i] 
}

y<-v[1]
for(i in 2:n){
  y[i] <- y[i-1]+v[i]  
}

# removing the first 1000 observations so that the effects of the start-up condition are washed out 
x <- x[1001:n]
y <- y[1001:n]

eq <- lm(y~x)
summary(eq)

beta <- c()
t_stat <- c()
r_2 <- c()
f_stat <- c()

rep <- 2000

# Monte Carlo
for(i in 1:rep){
  u <- rnorm(n)
  v <- rnorm(n)
  
  x<-u[1]
  for(i in 2:n){
    x[i] <- x[i-1]+u[i] 
  }
  y<-v[1]
  for(i in 2:n){
    y[i] <- y[i-1]+v[i]  
  }
  
  x <- x[-1000]
  y <- y[-1000]
  
  x <- x[1001:n]
  y <- y[1001:n]
  
  eq <- lm(y~x)
  
  # storing coefficients, t-stat, r-squared and f-stat
  beta <- rbind(beta, summary(eq)$coefficients[2])
  t_stat <- rbind(t_stat, summary(eq)$coefficients[4])
  r_2 <- rbind(r_2, summary(eq)$r.squared)
  f_stat <- rbind(f_stat, summary(eq)$fstatistic)
}

par(mfrow=c(2,2))
hist(beta, 50)
hist(r_2, 50)
hist(t_stat, 50)
hist(f_stat, 50)