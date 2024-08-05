#This code performs linear and exponential regression on a population dataset,
#evaluates the accuracy of the models, and generates a 1-step-ahead forecast 
#using the exponential model.

myData <- population_lowinc
library(forecast)

# Create a time series object from the 'Population' column of 'myData'
# The time series starts at 1960 and ends at 2017, with annual frequency
newData <- ts(myData$Population, start = c(1960), end = c(2017),
frequency=1)

# Fit a linear regression model to the time series data with 'trend' as the predictor
Linear_Model <- tslm(newData ~ trend)
summary(Linear_Model)

# Fit an exponential regression model to the log-transformed time series data with 
#'trend' as the predictor
Exponential_Model <- tslm(log(newData) ~ trend)
summary(Exponential_Model)

# Calculate and display the accuracy metrics for the linear model
accuracy(fitted(Linear_Model), myData$Population)
se <- sigma(Exponential_Model)
accuracy(exp(fitted(Exponential_Model) + se^2/2), myData$Population)

# Generate a 3-step-ahead forecast using the exponential model
flg <- forecast(Exponential_Model, h = 3)
exp(flg$mean + se^2/2)