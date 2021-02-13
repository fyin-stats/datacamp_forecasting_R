##################
##################
#### Forecasting in R
##################
##################
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}
packages <- c("dplyr", "forecast",
              "boot", "tidyr","ggplot2","astsa","xts","lubridate","readxl","fpp2")
ipak(packages)

## what you will learn
##
## exploring and visualizing time series
## simple benchmark methods for forecasting
## exponential smoothing and ARIMA models
## advanced forecasting methods
## measuring forecast accuracy

## Course textbook
## Forecasting" principles and practice
## 


## Prediction is a big problem
## we focus on time series only in this course
## time series: series of data observed over time
## e.g., daily IBM stocks, monthly rainfall in London
## Forecasting is estimating how the sequence of observations will continue in the future
## there is considerable uncertainty associated with such estimates, so we
## normally also provide an estimate of that uncertainty in the form of prediction intervals
## E.g., forecasts of monthly Australian expenditure on eating out

## What forecasting methods are available that take account of trend, seasonality, and other
## features of the data
## How to measure the accuracy of your forecasts?
## How to choose a good forecasting model?

## 
# Read the data from Excel into R
mydata <- read_excel("exercise1.xlsx")

# Look at the first few lines of mydata
head(mydata)

# Create a ts object called myts
myts <- ts(mydata[,2:4], start = c(1981, 1), frequency = 4)

## time series plot


# Plot the data with facetting
autoplot(myts, facets = TRUE)

# Plot the data without facetting
autoplot(myts, facets = FALSE)

# Plot the three series
autoplot(gold)
autoplot(woolyrnq)
autoplot(gas)

# Find the outlier in the gold series
goldoutlier <- which.max(gold)

# Look at the seasonal frequencies of the three series
frequency(gold)
frequency(woolyrnq)
frequency(gas)


# 
# Load the fpp2 package
library(fpp2)

# Create plots of the a10 data
autoplot(a10)
ggseasonplot(a10)

# Produce a polar coordinate season plot for the a10 data
ggseasonplot(a10, polar = TRUE)

# Restrict the ausbeer data to start in 1992
beer <- window(ausbeer, start = 1992)

# Make plots of the beer data
autoplot(beer)
ggsubseriesplot(beer)







#########################################################
# time series patterns
# Trends, seasonality, and cyclicity
# Trend: a pattern exists involving a long-term increase OR decrease in the data
# Seasonal: A periodic pattern exists due to the calendar 
# Cyclic: A pattern exists where the data exhibits rises and falls
# that are not of fixed period

# Australian electricity production
# US treasury bill contracts (be aware of the time granularity)
# Annual Canadian lynx trappings
# 


# Seasonal or cyclic?
# Differences betweens seasonal and cyclic patterns
# Seasonal pattern constant length vs cyclic pattern variable length
# Average length of cycle longer than length of seasonal pattern
# Magnitude of cycle more variable than magnitude of seasonal pattern
# The timing of peaks and troughs is predictable with seasonal
# data, but unpredictable in the long terms with cyclic data
# 

# Create an autoplot of the oil data
autoplot(oil)

# Create a lag plot of the oil data
gglagplot(oil)

# Create an ACF plot of the oil data
ggAcf(oil)


##########################

# Plot the annual sunspot numbers
autoplot(sunspot.year)
ggAcf(sunspot.year)

# Save the lag corresponding to maximum autocorrelation
maxlag_sunspot <- 1

# Plot the traffic on the Hyndsight blog
autoplot(hyndsight)
ggAcf(hyndsight)

# Save the lag corresponding to maximum autocorrelation
maxlag_hyndsight <- 7


#

# Use the process of elimination - pair the easy ones first, then see what is left.
# Trends induce positive correlations in the early lags.
# Seasonality will induce peaks at the seasonal lags.
# Cyclicity induces peaks at the average cycle length.
# 1-B, 2-A, 3-D, 4-C

# white noise
# white noise is just a time series of iid data
set.seed(3)
wn <- ts(rnorm(36))
autoplot(wn)


# 
ggAcf(wn) + ggtitle("Sample ACF for white noise")

# expectation: each autocorrelation is close to zero
# 95% of all autocorrelations for white noise should lie within the blue lines
# if not: series is probably not white noise
# 
autoplot(pigs/1000) + xlab("Year") 

ggAcf(pigs/1000) + ggtitle("ACF of monthly pigs slaughtered in Victoria")


# significant autocorrelation at lags 1, 2, 3
# you can be confident that this is not white noise series
# there is some information in the data that can be useful for forecasting
# Testing all autocorrelations together
# Ljung-Box test considers the first h autocorrelation values together
# A significant test indicates the data are probably not white noise

# 
Box.test(pigs, lag = 24, fitdf = 0, type = "Lj")

# White noise summary
# white noise is a time series that is purely random
# we can test for white noise by looking at an ACF plot or by doing a Ljung-Box test
# 

# 
# Plot the original series
autoplot(goog)

# Plot the differenced series
autoplot(diff(goog))

# ACF of the differenced series
ggAcf(diff(goog))

# Ljung-Box test of the differenced series
Box.test(diff(goog), lag = 10, type = "Ljung")


