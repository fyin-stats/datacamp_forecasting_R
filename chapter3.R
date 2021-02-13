###########################
###########################
##### chapter 3
###########################
###########################
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
# 
# https://www.stat.berkeley.edu/~arturof/Teaching/STAT248/lab10_part1.html
###########################
###########################
###########################
## Exponentially weighted forecasts
###########################
###########################
###########################

# two simple methods: naive, mean method
# something between these two would be useful
# forecast based on all observations but where the most recent observations
# are more heavily weighted
# simple exponential smoothing

# yhat: point forecast, subscript what period you are forecasing
# | means conditional on
# 

# yhat t+h | t = alpha y_t + alpha(1-alpha) y_t-1 + alpha(1-alpha)^2 y_t-2
# where alpha between 0 and 1
# 0 <= alpha <= 1

# how much the weight is placed on the most recent observation
# and how quickly the weight decays
# large alpha: larger weight for most recent observations and the weight decays very quickly
# small alpha: less weight for most recent observations and the weight decays slowly

# simple exponential smoothing
# lt is the level of the series at time t
# two parameters to estimate
# we choose alpha and l0 by minimizing SSE
# ses function can help you handle this


# 
oildata <- window(oil, start = 1996)
fc <- ses(oildata, h=5) # forecast for new 5 years
summary(fc)

# simple exponential smoothing is the starting point
# 

# Use ses() to forecast the next 10 years of winning times
fc <- ses(marathon, h = 10)

# Use summary() to see the model parameters
summary(fc)

# Use autoplot() to plot the forecasts
autoplot(fc)

# Add the one-step forecasts for the training data to the plot
autoplot(fc) + autolayer(fitted(fc))


# Create a training set using subset()
train <- subset(marathon, end = length(marathon) - 20)

# Compute SES and naive forecasts, save to fcses and fcnaive
fcses <- ses(train, h = 20)
fcnaive <- naive(train, h = 20)

# Calculate forecast accuracy measures
accuracy(fcses, marathon)
accuracy(fcnaive, marathon)

# Save the best forecasts as fcbest
fcbest <- fcnaive
# 
# Compute cross-validated errors for up to 8 steps ahead
e <- tsCV(goog, forecastfunction = naive, h = 8)

# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = TRUE)

# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
    ggplot(aes(x = h, y = MSE)) + geom_point()

## 







#######################################
####### Exponential smoothing with trend
######################################
#######
# Holt's linear trend
# add a trend component
# forecast
# level
# trend
# two smoothing parameters alpha and beta*
# a large beta* value means the slope changes rapidly, allowing for highly nonlinear trend
# use beta* here
# choose alpha, beta*, l0, b0 to minimize SSE

# Holt's method in R

AirPassengers %>% holt(h = 5) %>% autoplot()


# holt method will produce forecasts where the trend continues at the same slope indefinitely into
# the future
# a variation: allow the trend to dampen over time, so that it levels off to a constant value
# damped trend method, dampling parameter phi, 
# large phi, least dampling (0 < phi < 1)
# phi = 1, it is equivalent to holt's linear trend method
# short-run forecasts trended, long-run forecasts constant

# Air passengers
# damped trend levels off
# linear trend continues

# 
# Produce 10 year forecasts of austa using holt()
fcholt <- holt(austa, h = 10)

# Look at fitted model using summary()
summary(fcholt)

# Plot the forecasts
autoplot(fcholt)

# Check that the residuals look like white noise
checkresiduals(fcholt)


########################################
########################################
###### Exponential smoothing methods with trend
###### and seasonality
######


###### Holt-Winter's additive method


######
# one more smoothing parameter
# and more state parameters to estimate to account for the initial seasonal pattern
# seasonal component averages to zero


######
# Holt-winter's multiplicative method
# smoothing parameters
# 

hw(aust, seasonal = "additive")
hw(aust, seasonal = "multiplicative")

# hw function provides forecast with seasonal and trend
# 

# Plot the data
autoplot(a10)

# Produce 3 year forecasts
fc <- hw(a10, seasonal = "multiplicative", h = 3*12)

# Check if residuals look like white noise
checkresiduals(fc)
whitenoise <- FALSE

# Plot forecasts
autoplot(fc)

# 


# Create training data with subset()
train <- subset(hyndsight, end = length(hyndsight) - 28)

# Holt-Winters additive forecasts as fchw
fchw <- hw(train, seasonal = "additive", h = 28)

# Seasonal naive forecasts as fcsn
fcsn <- snaive(train, h = 28)

# Find better forecasts with accuracy()
accuracy(fchw, hyndsight)
accuracy(fcsn, hyndsight)

# Plot the better forecasts
autoplot(fchw)




###########################################
###########################################
### State space models
###########################################
###########################################
### Trend (N, A, Ad)
### Seasonal (N, A, M)
### Error (A, M)
### a total of 18 combinations

### ETS model, advantages
### can use MLE to estimate parameters
### have a way to generate prediction intervals
### select best model for a particular time series instead of looking at figures
### can use ets function to do this for us

### AIC, AICc, BIC

### ets does not do the forecast for you
### ausair %>% ets() %>% forecast() %>% autoplot()

### Monthly cortecosteroid drug sales
### h02 %>% ets() %>% forecast() %>% autoplot()

### completely automatic methods

### 


# Fit ETS model to austa in fitaus
fitaus <- ets(austa)

# Check residuals
checkresiduals(fitaus)

# Plot forecasts
autoplot(forecast(fitaus))

# Repeat for hyndsight data in fiths
fiths <- ets(hyndsight)
checkresiduals(fiths)
autoplot(forecast(fiths))

# Which model(s) fails test? (TRUE or FALSE)
fitausfail <- FALSE
fithsfail <- TRUE



#
# Function to return ETS forecasts
fets <- function(y, h) {
    forecast(ets(y), h = h)
}

# Apply tsCV() for both methods
e1 <- tsCV(cement, fets, h = 4)
e2 <- tsCV(cement, snaive, h = 4)

# Compute MSE of resulting errors (watch out for missing values)
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)

# Copy the best forecast MSE
bestmse <- mean(e2^2, na.rm=TRUE)


# 

# When does ETS fail?
#     
#     Computing the ETS does not work well for all series.
# 
# Here, you will observe why it does not work well for the annual Canadian 
# lynx population available in your workspace as lynx.

# It's important to realize that ETS doesn't work for all cases. 

# Plot the lynx series
autoplot(lynx)

# Use ets() to model the lynx series
fit <- ets(lynx)

# Use summary() to look at model and parameters
summary(fit)

# Plot 20-year forecasts of the lynx series
fit %>% forecast(h=20) %>% autoplot()