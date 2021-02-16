#################
#################
### chapter 5
#################
#################

### Advanced method

### Dynamic regression
### so far we have only covered the models that only use information from the history of time 
### series, but do not use any other information
### but often, there is other information
### for example, if you are forecasting monthly sales, then we could use the advertising
### expenditure for the month to improve our forecasts
### or perhaps, information about competitor activities


### dynamic regression is one way of combining this external information with the
### history of the time series in a single model
### Dynamic regression
### yt = \beta_0 + beta_1 x1t + ... + et

### in dynamic regression, we allow et to be an ARIMA process

### US personal comsumption and income
### forecast consumption using income data

### Dynamic regression model
### 
fit <- auto.arima(uschange[,"Consumption"],
                  xreg = uschange["Income"])
#
fit


# As usual, the ARIMA coefficients are not particularly interpretable, but 
# the regression coefficient is interpretable
# in dynamic regression, the regression part takes care of the predictor variable
# while the ARIMA part takes care of the short-term time series
# dynamics

# Residuals from dynamic regression model
# checkresiduals(fit)

# p-value > 0.05, which means these residuals do look like white noise

# Forecasts from dynamic regression model
# fcast <- forecast(fit, xreg = rep(0.8, 8))
# autoplot(fcast) + xlab("Year") + ylab("Percentage change")
# 

# Time plot of both variables
autoplot(advert, facets = TRUE)

# Fit ARIMA model
fit <- auto.arima(advert[, "sales"], xreg = advert[, "advert"], stationary = TRUE)

# Check model. Increase in sales for each unit increase in advertising
salesincrease <- coefficients(fit)[3]

# Forecast fit as fc
fc <- forecast(fit, xreg = rep(10, 6))

# Plot fc with x and y labels
autoplot(fc) + xlab("Month") + ylab("Sales")


# Time plots of demand and temperatures
autoplot(elec[, c("Demand", "Temperature")], facets = TRUE)

# Matrix of regressors
xreg <- cbind(MaxTemp = elec[, "Temperature"], 
              MaxTempSq = elec[, "Temperature"]^2, 
              Workday = elec[, "Workday"])

# Fit model
fit <- auto.arima(elec[, "Demand"], xreg = xreg)

# Forecast fit one day ahead
forecast(fit, xreg = cbind(20, 20^2, 1))

##################################
##################################
#### dynamic harmonic regression
##################################
##################################
# Periodic seasonality can be handled using pairs
# of Fourier terms
# m = seasonal period
# Every periodic function can be approximated by sums of sin and cos terms for large enough k
# regression coefficients alpha_k, beta_k
# et can be modeled as a non-seasonal ARIMA process
# assumes seasonal pattern us unchanging

# Australian cafe expenditure
# 
fit <- auto.arima(cafe, xreg = fourier(cafe, K = 1),
                  seasonal = FALSE, lambda = 0) # set seasonal = FALSE, meaning that the ARIMA error in
# the model should be non-seasonal
fit %>% forecast(xreg = fourier(cafe, K =1, h = 24)) %>% autoplot() + ylin(1.6, 5.1)

# also used a Box-Cox transformation by setting lambda = 0
# because the variance increases with the level fo the series
# We can use the fourier function to generate all the Fourier terms to include in our model
# just have select the value of K which determines how complicatd the seasonal pattern will be
# When making predictions, you also need the fourier function to generate the fourier series
# It mush have the same value of K as was used when fitting the model
# By adding the h argument, the fourier function knows we want future values not past values
# 

# can increase the value of K to capture more complex seasonal patterns
# most parts are automtic only need manually try K
# 


# Set up harmonic regressors of order 13
harmonics <- fourier(gasoline, K = 13)

# Fit regression model with ARIMA errors
fit <- auto.arima(gasoline, xreg = harmonics, seasonal = FALSE)

# Forecasts next 3 years
newharmonics <- fourier(gasoline, K = 13, h = 3*52)
fc <- forecast(fit, xreg = newharmonics)

# Plot forecasts fc
autoplot(fc)



# Harmonic regression for multiple seasonality
# 

# Fit a harmonic regression using order 10 for each type of seasonality
fit <- tslm(taylor ~ fourier(taylor, K = c(10, 10)))

# Forecast 20 working days ahead
fc <- forecast(fit, newdata = data.frame(fourier(taylor, K = c(10, 10), h = 20 * 48)))

# Plot the forecasts
autoplot(fc)

# Check the residuals of fit
checkresiduals(fit)




#################################################
#########

# Plot the calls data
autoplot(calls)

# Set up the xreg matrix
xreg <- fourier(calls, K = c(10, 0))

# Fit a dynamic regression model
fit <- auto.arima(calls, xreg = xreg, seasonal = FALSE, stationary = TRUE)

# Check the residuals
checkresiduals(fit)

# Plot forecast for 10 working days ahead
fc <- forecast(fit, xreg = fourier(calls, c(10, 0), h = 1690))
autoplot(fc)



####################################
### TBATS model
####################################
# Trigonometric terms for seasonality
# Box-cox transformations for heterogeneity
# ARMA errors for short-term dynamics
# Trend (possibly damped)
# Seasonal (including multiple and non-integer periods)

# US Gasoline data
gasoline %>% tbats() %>% forecast() %>% autoplot() + xlab("Year") + ylab("thousand barrels per day")

# last part corresponds to the fourier series

# call center data
calls %>% window(start = 20) %>% tbats() %>% forecast() %>% autoplot() + xlab("Weeks") + ylab("Calls")

# very general for handling non-integer seasonality, multiple seasonal periods
# entirely automated

# predictions are often too wide
# point forecast look good

# very slow on long series

# 

# Plot the gas data
autoplot(gas)

# Fit a TBATS model to the gas data
fit <- tbats(gas)

# Forecast the series for the next 5 years
fc <- forecast(fit, h = 60)

# Plot the forecasts
autoplot(fc)

# Plot the gas data
autoplot(gas)

# Fit a TBATS model to the gas data
fit <- tbats(gas)

# Forecast the series for the next 5 years
fc <- forecast(fit, h = 12 * 5)

# Plot the forecasts
autoplot(fc)

# Record the Box-Cox parameter and the order of the Fourier terms
lambda <- 0.082
K <- 5


###############################
###############################
###############################
# your future in forecasting
###############################
###############################
###############################
# online text book
# practice forecasting lots of different times series, build your intuition
# 
