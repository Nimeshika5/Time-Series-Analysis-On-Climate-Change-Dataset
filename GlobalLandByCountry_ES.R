#Simple Exponential Smoothening
#The simplest of the exponentially smoothing methods is naturally called simple exponential smoothing (SES). 
#This method is suitable for forecasting data with no clear trend or seasonal pattern.


library(tidyverse)
library(fpp2)
library(dplyr)


df1 = read.csv("GlobalLandTemperaturesByCountry.csv")
df2=data.frame(df1)

df2 = df2 %>%
  mutate(AverageTemperature = replace(AverageTemperature,is.na(AverageTemperature), mean(AverageTemperature,na.rm = T) ))

t <- ts(df2$AverageTemperature,start=1960,end=1998,frequency = 1)
str(t)
df <- window(t, start=1960, end=1998)

landAvgTemps <- df2[121:3192,]$AverageTemperature
Y = ts(landAvgTemps,start=c(1743,1),frequency = 12)
print(paste("Start time:",start(Y)[1],start(Y)[2]))
print(paste("End time:",end(Y)[1],end(Y)[2]))


autoplot(window(Y,1960,1998),main = "Time Series Plot [1960-1998]") + ylab("Temperature (C)")

#Global Temperature from 1960 to 1998
autoplot(Y) + ylab("Land Average Temperature") + xlab("Year")

df <- window(Y, start=1960, end=1980)
# Estimate parameters
fc <- ses(df, h=5)


# Accuracy of one-step-ahead training errors and its plot
round(accuracy(fc),2)

train <- window(t, start = 1960, end = 1998)
#test <- window(Y, start = 1999)



#---------Holt-Winters Method----------

#Before we predict values, we need to create a fit to the data. 
#We use the Holt-Winters function to tune the fit manually by setting the tuning variables such as alpha, beta and gamma
HW1 <- HoltWinters(Y)
HW2 <- HoltWinters(Y, alpha=0.2, beta=0.1, gamma=0.1)

plot(Y, ylab="Temperature", xlab="Years", xlim=c(1960,1998))
lines(HW1$fitted[,1], lty=2, col="blue")
lines(HW2$fitted[,1], lty=2, col="red")



#Using the predict function we'll need to specify how many data points we'll predict into the future(1998 - 2001). 
#Here, we'll use a value of 24 to project 2 years into the future
#We set the level of confidence interval = 0.95

HW1.pred <- predict(HW1, 24, prediction.interval = TRUE, level=0.95)
#Visually evaluate the prediction
plot(Y, ylab="Temperature Forecasting", xlim=c(1998,2001))
lines(HW1$fitted[,1], lty=2, col="blue")
lines(HW1.pred[,1], col="red")
lines(HW1.pred[,2], lty=2, col="orange")
lines(HW1.pred[,3], lty=2, col="orange")


#Using our HW1 Holt-Winters fit from before, we can use forecast to make new predictions(1998 - 2001) and include both 80% and 95% confidence intervals.
library(forecast)
HW1_for <- forecast(HW1, h=24, level=c(80,95))
#visualize our predictions:
plot(HW1_for, xlim=c(1998, 2001))
lines(HW1_for$fitted, lty=2, col="purple")

