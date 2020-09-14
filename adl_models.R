# clear workspace
rm(list=ls())

# Load package using a function load_package-----------------------------------------------------------------
load_package<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

load_package("tseries")
load_package("zoo")
load_package("openxlsx")
load_package("car")
load_package("forecast")
load_package("sandwich")
load_package("strucchange")


datagive <- read.xlsx("datagive.xlsx")


# Testing weak exogeneity for INC in the CONS equation -----

# estimate the conditional model
eq1.ls <- lm(cons ~ inc, datagive)
summary(eq1.ls)
res_eq1 <- eq1.ls$residuals

# estimate the marginal model for inc
eq2.ls <- lm(inc ~ cons1 + inc1, datagive)
summary(eq2.ls)
res_eq2 <- c(NA, eq2.ls$residuals)

# regress res_eq1 on inc and res_eq2
eq3.ls <- lm(res_eq1 ~ inc + res_eq2, datagive)

# compute the LM statistics
(LM <- (summary(eq3.ls)$r.squared)*length(na.omit(datagive$cons1))) 
(p_value = 1 - pchisq(LM, df=1)) 

# second form is to regress cons on inc and res_eq2
eq4.ls <- lm(cons ~ inc + res_eq2, datagive)

# compute the Wald test for inc
linearHypothesis(eq4.ls, hypothesis.matrix=c(0,0,1), rhs = 0)


# Durbin-Wu-HAusman test

# using cons[-1] as instrument 
eq4.tsls1 <- lm(inc ~ cons1, datagive)
datagive$inc.hat <- c(NA,fitted(eq4.tsls1))

eq4.tsls2 <- lm(cons ~ inc.hat, datagive)
summary(eq4.tsls2)

# test statistics
(q <- ((eq4.tsls2$coefficients[2] - eq1.ls$coefficients[2])^2)/((summary(eq4.tsls2)$coefficients[4] - summary(eq1.ls)$coefficients[4])^2))
(p_valuewh = 1 - pchisq(q, df=1)) 


#-----------
# Static Regression -----

datagive1 <- datagive[-1,] # lagged data

eq1.ls <- lm(cons ~ inc, datagive1)  
summary(eq1.ls)

res_eq1 <- eq1.ls$residuals
c_hat <- fitted(eq1.ls)
c_se <- summary(eq1.ls)$sigma

c_up <- c_hat + 2*c_se  # limit superior
c_low <- c_hat - 2*c_se  # limit inferior


# define a function to plot the fitted series and and its confidence interval
plot_1 <- function(series, series_hat, up, low){
  par(mfrow=c(1,1))
  plot.zoo(cbind(series, series_hat, up, low), 
           plot.type = "single", col = c(4,2,3,1), 
           ylab = "")
  legend("topright", c("Actual", "Fitted", "LS", "LI"), 
         lty = 1, bty = "n", inset=c(0,0),
         col=c(4,2,3,1),  cex = .8)
}


# define a function to plot the residuals 
plot_2 <- function(series, series_hat, res){
  par(mfrow=c(2,1))
  plot.zoo(cbind(series, series_hat), 
           plot.type = "single", col = c(2,3), 
           ylab = "")
  legend("topright", c("Actual", "Fitted"), 
       lty = 1, bty = "n", inset=c(0,0),
       col=c(2,3),  cex = .8)
  plot.zoo(res, col = 4)
  legend("topright", c("RES"), 
       lty = 1, bty = "n", inset=c(0,0),
       col=4,  cex = .7)
}


plot_1(datagive1$cons, c_hat, c_up, c_low)
plot_2(datagive1$cons, c_hat, res_eq1)

# Estimate an AR(1) for CONS using OLS ----

eq3.ls <- lm(cons ~ cons1, datagive)
summary(eq3.ls)

res_eq3 <- eq3.ls$residuals
car_hat <- fitted(eq3.ls)
car_se <- summary(eq3.ls)$sigma

car_up <- car_hat + 2*car_se
car_low <- car_hat - 2*car_se

plot_1(datagive1$cons, car_hat, car_up, car_low)
plot_2(datagive1$cons, car_hat, res_eq3)


# Estimate a leading indicator model for CONS using INC as leading indicator ----

eq4.ls <- lm(cons ~ inc1, datagive)
summary(eq4.ls)

res_eq4 <- eq4.ls$residuals
cleadi_hat <- fitted(eq4.ls)
cleadi_se <- summary(eq4.ls)$sigma

cleadi_up <- cleadi_hat + 2*cleadi_se
cleadi_low <- cleadi_hat - 2*cleadi_se


plot_1(datagive1$cons, cleadi_hat, cleadi_up, cleadi_low)
plot_2(datagive1$cons, cleadi_hat, res_eq4)


# Estimate a rate of change model for CONS and INC ----

eq5.ls <- lm(diff(cons) ~ diff(inc), datagive)
summary(eq5.ls)

res_eq5 <- eq5.ls$residuals
dcratec_hat <- fitted(eq5.ls)
dcratec_se <- summary(eq5.ls)$sigma

dcratec_up <- dcratec_hat + 2*dcratec_se
dcratec_low <- dcratec_hat - 2*dcratec_se


plot_1(diff(datagive$cons), dcratec_hat, dcratec_up, dcratec_low)
plot_2(diff(datagive$cons), dcratec_hat, res_eq5)


# Modifying the program to have the same dependent variable 
dinc <- diff(datagive$inc)
eq6.ls <- lm(cons ~ dinc, datagive1, offset=cons1)
summary(eq6.ls)

res_eq6 <- eq6.ls$residuals
cratec_hat <- fitted(eq6.ls)
cratec_se <- summary(eq6.ls)$sigma

cratec_up <- cratec_hat + 2*cratec_se
cratec_low <- cratec_hat - 2*cratec_se


plot_1(datagive1$cons, cratec_hat, cratec_up, cratec_low)
plot_2(datagive1$cons, cratec_hat, res_eq6)


# Estimate distributed lags model for CONS and INC ----
eq7.ls <- lm(cons ~ inc + inc1, datagive)
summary(eq7.ls)

res_eq7 <- eq7.ls$residuals
cdl_hat <- fitted(eq7.ls)
cdl_se <- summary(eq7.ls)$sigma

cdl_up <- cdl_hat + 2*cdl_se
cdl_low <- cdl_hat - 2*cdl_se


plot_1(datagive1$cons, cdl_hat, cdl_up, cdl_low)
plot_2(datagive1$cons, cdl_hat, res_eq7)


# Estimate the partial adjustment for CONS and INC ----
eq8.ls <- lm(cons ~ inc + cons1, datagive)
summary(eq8.ls)

res_eq8 <- eq8.ls$residuals
cpa_hat <- fitted(eq8.ls)
cpa_se <- summary(eq8.ls)$sigma

cpa_up <- cpa_hat + 2*cpa_se
cpa_low <- cpa_hat - 2*cpa_se


plot_1(datagive1$cons, cpa_hat, cpa_up, cpa_low)
plot_2(datagive1$cons, cpa_hat, res_eq8)


# Estimate the static regression with AR(1) errors -----
eq9.ls <- arima(datagive$cons, xreg = datagive$inc, order = c(1,0,0))
eq9.ls

res_eq9 <- eq9.ls$residuals
csrar_hat <- fitted.values(eq9.ls)
csrar_se <- sqrt(eq9.ls$sigma2)

csrar_up <- csrar_hat + 2*csrar_se
csrar_low <- csrar_hat - 2*csrar_se


plot_1(datagive$cons, csrar_hat, csrar_up, csrar_low)
plot_2(datagive$cons, csrar_hat, res_eq9)


# NeweyWest correction:
m <- floor(0.75 * length(datagive$cons)^(1/3))
(eq9.lsnw <- NeweyWest(lm(cons ~ inc, datagive1), 
                     lag = m - 1, prewhite = F, 
                     adjust = T))


# Estimate the reduced form for CONS and INC ----
eq11.ls <- lm(cons ~ inc1 + cons1, datagive)
summary(eq11.ls)

res_eq11 <- eq11.ls$residuals
crf_hat <- fitted(eq11.ls)
crf_se <- summary(eq11.ls)$sigma

crf_up <- crf_hat + 2*crf_se
crf_low <- crf_hat - 2*crf_se


plot_1(datagive1$cons, crf_hat, crf_up, crf_low)
plot_2(datagive1$cons, crf_hat, res_eq8)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#-----------
# Estimate an ADL(1,1) for CONS and INC ----
eq12.ls <- lm(cons ~ cons1 + inc + inc1, datagive)
summary(eq12.ls)

res_eq12 <- eq12.ls$residuals
cadl_hat <- fitted(eq12.ls)
cadl_se <- summary(eq12.ls)$sigma

cadl_up <- cadl_hat + 2*cadl_se
cadl_low <- cadl_hat - 2*cadl_se


plot_1(datagive1$cons, cadl_hat, cadl_up, cadl_low)
plot_2(datagive1$cons, cadl_hat, res_eq12)


# wald test - multiple constraints on coefficients to test whether the long run elasticity is well defined
linearHypothesis(eq12.ls, hypothesis.matrix= matrix(c(0,0,1,0,0,1,0,1), ncol = 4), rhs = c(1,0))


# test omitted variables INFLAT and INFLAT(-1)
summary(eq12.ls)
eq12.testadd <- lm(cons ~ cons1 + inc + inc1 + inflat + inflat1, datagive)
summary(eq12.testadd)

# wald test on inflation coefficients
linearHypothesis(eq12.testadd, hypothesis.matrix=matrix(c(0,0,0,0,0,0,0,0,1,0,0,1), ncol = 6), rhs = c(0,0)) 

# Estimate ADL CONS c INC INFLAT ----
eq13.ls <- lm(cons ~ cons1 + inc + inc1 + inflat + inflat1, datagive)
summary(eq13.ls)

res_eq13 <- eq13.ls$residuals
cadl111_hat <- fitted(eq13.ls)
cadl111_se <- summary(eq13.ls)$sigma

cadl111_up <- cadl111_hat + 2*cadl111_se
cadl111_low <- cadl111_hat - 2*cadl111_se
  

plot_1(datagive1$cons, cadl111_hat, cadl111_up, cadl111_low)
plot_2(datagive1$cons, cadl111_hat, res_eq13)

# testing if long run elasticity of INC is well defined
linearHypothesis(eq13.ls, hypothesis.matrix= matrix(c(0,0,1,0,0,1,0,1,0,0,0,0), ncol = 6), rhs = c(1,0))

# testing if long run elasticity of INFLAT is well defined
linearHypothesis(eq13.ls, hypothesis.matrix= matrix(c(0,0,1,0,0,0,0,0,0,1,0,1), ncol = 6), rhs = c(1,0))

# construct the error correction term
c1 <- eq13.ls$coefficients[1]
c2 <- eq13.ls$coefficients[2]
c3 <- eq13.ls$coefficients[3]
c4 <- eq13.ls$coefficients[4]
c5 <- eq13.ls$coefficients[5]
c6 <- eq13.ls$coefficients[6]

ce <- datagive$cons - (c1/(1-c2)) - ((c3+c4)/(1-c2))*datagive$inc - ((c5+c6)/(1-c2))*datagive$inflat
par(mfrow=c(1,1))
plot(ce, type="l")

# testing unit root 
adf.test(ce)

# Estimate ECM and testing  ----
ce1 <- ce[1:(length(ce)-1)]
eq14.ls <- lm(diff(cons) ~ diff(inc) + diff(inflat) + ce1, datagive)
summary(eq14.ls)

res_eq14 <- eq14.ls$residuals
cecm_hat <- fitted(eq14.ls)

plot_2(diff(datagive$cons), cecm_hat, res_eq14)

# histogram of the residuals
par(mfrow=c(1,1))
hist(res_eq14, 30)

# testing serial correlation for the residuals
res_eq14_1 <- c(NA, res_eq14[1:(length(res_eq14)-1)]) 
res_eq14_2 <- c(NA, NA, res_eq14[1:(length(res_eq14)-2)])
eq14.auto <- lm(res_eq14 ~ diff(inc) + diff(inflat) + ce1 + res_eq14_1 + res_eq14_2, datagive)
summary(eq14.auto)

# testing arch for the residuals
res_eq14_sq <- res_eq14^2
res_eq14_1_sq <- res_eq14^2
eq14.archtest <- lm(res_eq14_sq ~ res_eq14_1_sq)
summary(eq14.archtest)

# testing heteroskedasticity
dinflat <- diff(datagive$inflat)
dinflat_sq <- dinflat^2
dinc_dinflat <- dinc*dinflat
ce1_sq <- ce1^2
dinc_ce1 <- dinc*ce1
dinc_sq <- dinc^2
dinflat_ce1 <- dinflat*ce1

eq14.white <- lm(res_eq14^2 ~ dinc_sq + dinc_dinflat + dinc_ce1 + dinc 
                 + dinflat_sq + dinflat_ce1 + dinflat + ce1_sq + ce1)
summary(eq14.white)


# CUSUM
residrls <- recresid(eq14.ls)
dcons <- diff(datagive$cons)
cusum <- efp(dcons ~ dinc + dinflat + ce1, type = "Rec-CUSUM")
plot(cusum)

# CUSUMQ
residrls2 <- residrls^2 
cusumq <- cumsum(c(0, residrls2))/sum(residrls2)

up <- 2*sd(cusumq)/sqrt(length(cusumq)) + (1/length(cusumq))*seq(0:(length(cusumq)-1))
low <- -2*sd(cusumq)/sqrt(length(cusumq)) + (1/length(cusumq))*seq(0:(length(cusumq)-1))
plot.zoo(cbind(cusumq, low, up), plot.type="single", main = "Recursive CUSUMQ test", col = c(1,2,2))


