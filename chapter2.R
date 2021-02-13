#################
#################
### Chapter 2
#################
#################


# sample futures
# Forecasts and potential futures
# simulated future
# each time you simulate from the model, you get a possible future path
# repeat this for several times, you have distribution of future paths
# simulated futures

# forecast intervals

# average of what our model says possible
# 80% forecast intervals
# 95% forecast intervals should contain 95% of future observations

# 
# Use naive() to forecast the goog series
fcgoog <- naive(goog, h=20)

# Plot and summarize the forecasts
autoplot(fcgoog)
summary(fcgoog)

# Use snaive() to forecast the ausbeer series
fcbeer <- snaive(ausbeer, h=16)

# Plot and summarize the forecasts
autoplot(fcbeer)
summary(fcbeer)


# Fitted values and residuals
# try to forecast what you have already seen
# a fitted value is the forecast of an observation using all previous observations
# that is, they are one-step forecasts
# often not true forecasts since parameters are estimated on all data
# a residual is the difference between an observations and its fitted value
# if the forecasting model 


# naive method: use the most recent observation
# autoplot(residuals(fc))
# Residuals should look like white noise

# essential assumptions
# they should be uncorrelated
# they should have zero mean

# useful properties (for computing prediction intervals)
# they should have constant variance
# they should be normally distributed

# we can test these assumptions using the check residuals
# checkresiduals(fc)
# 

# Training and test sets
# training data, test data (in time)
# the test set must not be used for any aspect of calculating forecasts
# build forecasts using training test
# split data into training and testing using window
training <- window(oil, end = 2003)
test <- window(oil, start = 2004)
fc <- naive(training, h =10)
autoplot(fc) + autolayer(test, series = "Test data")

# forecast error != residuals
# 1, which are errors on the training test (vs test)
# 2, which are based on one-step forecasts (vs multi step)
# Measures of forecast accuracy
# mean absolute error
# mean squared error
# mean absolute percentage error
# mean absolute scaled error (Q is a scaling constant)

# in all scenarios, small values indicate better results
# accuracy(fc, test)

# 

# Create the training data as train
train <- subset(gold, end = 1000)

# Compute naive forecasts and save to naive_fc
naive_fc <- naive(train, h = 108)

# Compute mean forecasts and save to mean_fc
mean_fc <- meanf(train, h = 108)

# Use accuracy() to compute RMSE statistics
accuracy(naive_fc, gold)
accuracy(mean_fc, gold)

# Assign one of the two forecasts as bestforecasts
bestforecasts <- naive_fc




# 
# Create three training series omitting the last 1, 2, and 3 years
train1 <- window(vn[, "Melbourne"], end = c(2014, 4))
train2 <- window(vn[,"Melbourne"], end = c(2013, 4))
train3 <- window(vn[,"Melbourne"], end = c(2012,4))

# Produce forecasts using snaive()
fc1 <- snaive(train1, h = 4)
fc2 <- snaive(train2, h = 4)
fc3 <- snaive(train3, h = 4)

# Use accuracy() to compare the MAPE of each series
accuracy(fc1, vn[, "Melbourne"])["Test set", "MAPE"]
accuracy(fc2, vn[, "Melbourne"])["Test set", "MAPE"]
accuracy(fc3, vn[, "Melbourne"])["Test set", "MAPE"]

# Well done! A good model forecasts well (so has low RMSE on the test set) 
# and uses all available information in the training data (so has white noise residuals).


#################################################
# Traditional train / test splict, might be wasteful
# we have a relatively small test set, and it is possible that we could draw
# conclusions that work for that test set but which are not reliable for future times
# 
# Time series cross-validation is a solution to this problem
# a series of training and test sets
# forecast evaluation on a rolling origin
# forecast origin (analogous to CV for non-time series data)
# tsCV function in R

# e <- tsCV(oil, forecastfunction = naive, h = 1)
# mean(e^2, na.rm = TRUE)

# 
sq <- function(u) {u^2}

for(h in 1:10){
    oil %>% tsCV(forecastfunction = naive, h = h) %>% sq() %>% mean(na.rm = TRUE) %>% print()
    
}


# The MSE increases with the forecast horizong
# choose the model with the smallest MSE computed using time series cross validation
# compute it at the forecast horizon of most interest to you
