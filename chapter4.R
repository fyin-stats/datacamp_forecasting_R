#######################
#######################
### Chapter 4: Transformations for variance stabilization
#######################
#######################
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}
packages <- c("dplyr", "forecast",
              "boot", "tidyr","ggplot2","astsa",
              "xts","lubridate","readxl","fpp2")
ipak(packages)
#########################
#########################


### In ETS model, we use multiplicative error and multiplicative seasonality
### to handle time series with increasing variance 
### an alternative approach: transform the time series
### if the data show increasing variation as the level of the series
### increases, then a transformation can be useful

### following methods are of increasing strength
### square root
### cube root
### logarithm
### inverse (-1/x)

### usmelec
###
autoplot(usmelec) + xlab("Year") + ylab("") + ggtitle("")

###
autoplot(log(usmelec)) + xlab("Year") + ylab("") + ggtitle("")
### the fluctuations at the top end are still larger than the fluctuations
### at the bottom end

### Box-Cox transformations
### lambda = 1: no substantive transformation
### 
BoxCox.lambda(usmelec)
###
usmelec %>% ets(lambda = -0.57) %>% forecast(h=60) %>% autoplot()

### it is not very common to use a Box-cox transformation with an ETS model like this
### as ETS models are capable of handling the increasing variance directly 
### by using multiplicative error and seasonal components in the model
### 

# Plot the series
autoplot(a10)

# Try four values of lambda in Box-Cox transformations
a10 %>% BoxCox(lambda = 0.0) %>% autoplot()
a10 %>% BoxCox(lambda = 0.1) %>% autoplot()
a10 %>% BoxCox(lambda = 0.2) %>% autoplot()
a10 %>% BoxCox(lambda = 0.3) %>% autoplot()


# Compare with BoxCox.lambda()
BoxCox.lambda(a10)


# Plot the US female murder rate
autoplot(wmurders)

# Plot the differenced murder rate
autoplot(diff(wmurders))

# Plot the ACF of the differenced murder rate
ggAcf(diff(wmurders))


#############################
# Plot the data
autoplot(h02)

# Take logs and seasonal differences of h02
difflogh02 <- diff(log(h02), lag = 12)

# Plot difflogh02
autoplot(difflogh02)

# Take another difference and plot
ddifflogh02 <- diff(difflogh02)
autoplot(ddifflogh02)

# Plot ACF of ddifflogh02
ggAcf(ddifflogh02)













##########################
### ARIMA models
##########################
### autoregressive models, multiple regression with lagged observations as predictors
### Moving average models, multiple regression with lagged errors as predictors
### et ~ white noise

### ARMA models
### can only work with stationary data
### 
### combine ARMA model with d - lots of differencing
### If our time series needs to be differenced d times to make it
### stationary, then the resulting model is called an ARIMA(p,d,q) model
### Fortunately there is an automatic procedure for this


### 
fit <- auto.arima(usnetelec)

### 
summary(fit)

### can only compare AICc for models of the same class
fit %>% forecast() %>% autoplot()

### 
### How does auto.arima() work?

### Hyndman-Khandakar algorithm
### select number of differences d via unit root tests
### select p and q by minimizing AICc

### model space is very large, use stepwise search to traverse model space, to save time
### 

# Fit an automatic ARIMA model to the austa series
fit <- auto.arima(austa)

# Check that the residuals look like white noise
checkresiduals(fit)


# Fit an automatic ARIMA model to the austa series
fit <- auto.arima(austa)

# Check that the residuals look like white noise
checkresiduals(fit)
residualsok <- TRUE

# Summarize the model
summary(fit)

# Find the AICc value and the number of differences used
AICc <- -14.46
d <- 1

# Plot forecasts of fit
fit %>% forecast(h = 10) %>% autoplot()



#
# Plot forecasts from an ARIMA(0,1,1) model with no drift
austa %>% Arima(order = c(0, 1, 1), include.constant = FALSE) %>% forecast() %>% autoplot()

# Plot forecasts from an ARIMA(2,1,3) model with drift
austa %>% Arima(order = c(2,1,3), include.constant = TRUE) %>% forecast() %>% autoplot()

# Plot forecasts from an ARIMA(0,0,1) model with a constant
austa %>% Arima(order = c(0,0,1), include.constant = TRUE) %>% forecast() %>% autoplot()

# Plot forecasts from an ARIMA(0,2,1) model with no constant
austa %>% Arima(order = c(0,2,1), include.constant = FALSE) %>% forecast() %>% autoplot()


# 

# Set up forecast functions for ETS and ARIMA models
fets <- function(x, h) {
    forecast(ets(x), h = h)
}
farima <- function(x, h) {
    forecast(auto.arima(x), h = h)
}

# Compute CV errors for ETS on austa as e1
e1 <- tsCV(austa, fets, h = 1)

# Compute CV errors for ARIMA on austa as e2
e2 <- tsCV(austa, farima, h = 1)

# Find MSE of each model class
mean(e1^2, na.rm = TRUE)
mean(e2^2, na.rm = TRUE)

# Plot 10-year forecasts using the best model class
austa %>% farima(h = 10) %>% autoplot()



#######################
### Seasonal time series
#######################

# ARIMA models (p,d,q)
# (P,D,Q)
# D = Number of seasonal differences
# P = Number of seasonal AR differences
# Q = Number of seasonal MA lags
# m = number of observations per year

# 
autoplot(debitcards) + xlab("Year") + ylab("million ISK") + 
    ggtitle("Retail debit card usage in Iceland")

# 
fit <- auto.arima(debitcards, lambda = 0)

# fit

# arima can be hard to interpret 
# but are very powerful models which can handle a very wide range of time series

# 
fit %>% forecast(h = 36) %>% autoplot() + xlab("Year")

# A nice feature of seasonal ARIMA models is that they allow the seasonality to change over time
# The forecasted seasonal pattern will be most affected by the shape of the seasonality near
# the end of the series, and not so much on the seasonal patterns at the start of the series
# you might wonder where the trend comes from in this model, as there is no drift term here
# It turns out that whether you do two lots of differencing of any kind
# the forecasts will have a trend without needing to include the contant
# Here we have done both ordinary and seasonal differencing, so there is
# a trend in the forecasts
# 


# Box.test
# If the p value is greater than 0.05 then the residuals are 
# independent which we want for the model to be correct. 
# If you simulate a white noise time series using the code below 
# and use the same test for it then the p value will be greater than 0.05.

# 

# Check that the logged h02 data have stable variance
h02 %>% log() %>% autoplot()

# Fit a seasonal ARIMA model to h02 with lambda = 0
fit <- auto.arima(h02, lambda = 0)

# Summarize the fitted model
summary(fit)

# Record the amount of lag-1 differencing and seasonal differencing used
d <- 1
D <- 1

# Plot 2-year forecasts
fit %>% forecast(h = 24) %>% autoplot()


# Find an ARIMA model for euretail
fit1 <- auto.arima(euretail)

# Don't use a stepwise search
fit2 <- auto.arima(euretail, stepwise=FALSE)

# AICc of better model
AICc <- 68.39

# Compute 2-year forecasts from better model
fit2 %>% forecast(h = 8) %>% autoplot()

# Use 20 years of the qcement data beginning in 1988
train <- window(qcement, start = 1988, end = c(2007, 4))

# Fit an ARIMA and an ETS model to the training data
fit1 <- auto.arima(train)
fit2 <- ets(train)

# Check that both models have white noise residuals
checkresiduals(fit1)
checkresiduals(fit2)

# Produce forecasts for each model
fc1 <- forecast(fit1, h = 25)
fc2 <- forecast(fit2, h = 25)

# Use accuracy() to find best model based on RMSE
accuracy(fc1, qcement)
accuracy(fc2, qcement)
bettermodel <- fit2