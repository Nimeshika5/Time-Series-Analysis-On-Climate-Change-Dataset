#Simple Exponential Smoothening
#The simplest of the exponentially smoothing methods is naturally called simple exponential smoothing (SES). 
#This method is suitable for forecasting data with no clear trend or seasonal pattern.


library(tidyverse)
library(fpp2)
library(dplyr)

#Simple Exponential Smoothening
#The simplest of the exponentially smoothing methods is naturally called simple exponential smoothing (SES). 
#This method is suitable for forecasting data with no clear trend or seasonal pattern.


library(tidyverse)
library(fpp2)
library(dplyr)

df1 = read.csv("GlobalTemperatures.csv")
df2=data.frame(df1)

df2 = df2 %>%
  mutate(LandAverageTemperature = replace(LandAverageTemperature,is.na(LandAverageTemperature), mean(df2$LandAverageTemperature,na.rm = T) ))

df1 = read.csv("GlobalTemperatures.csv")
df2=data.frame(df1)

df2 = df2 %>%
  mutate(LandAverageTemperature = replace(LandAverageTemperature,is.na(LandAverageTemperature), mean(df2$LandAverageTemperature,na.rm = T) ))

t <- ts(df2$LandAverageTemperature,start=1750,end=2015,frequency = 1)
str(t)
df <- window(t, start=1750, end=2015)



landAvgTemps <- df2[121:3192,]$LandAverageTemperature
Y = ts(landAvgTemps,start=c(1760,1),frequency = 12)
print(paste("Start time:",start(Y)[1],start(Y)[2]))
print(paste("End time:",end(Y)[1],end(Y)[2]))


autoplot(window(Y,1965,2015),main = "Time Series Plot [1965-2015]") + ylab("Temperature (C)")

#Global Temperature from 1750 to 2015
autoplot(t) + ylab("Land Average Temperature") + xlab("Year")

df <- window(t, start=1750)
# Estimate parameters
fc <- ses(df, h=5)


# Accuracy of one-step-ahead training errors and its plot
round(accuracy(fc),2)
autoplot(fc) +
  autolayer(fitted(fc), series="Fitted") +
  ylab("Land Average Temperature") + xlab("Year")



train <- window(Y, start = 1965, end = 2015)
#test <- window(Y, start = 2016)

# removing the trend
Y.dif <- diff(train)

#Plot after removing the trend 
autoplot(Y.dif)

# reapplying SES on the filtered data
ses.Y.dif <- ses(Y.dif,
                 alpha = .2,
                 h = 100)
autoplot(ses.Y.dif)
#The forecasts for the period 1750 - 2015 are plotted. 
#Also plotted are one-step-ahead fitted values alongside the data over the same period
#The large value of alpha reflects in larger adjustment that takes place in the estimated level
#A smaller value of alpha would lead to smaller changes over time, and so the series of fitted values would be smoother.
#In our case, the alpha value is smaller

#---------Holt-Winters Method----------

#Before we predict values, we need to create a fit to the data. 
#We use the Holt-Winters function to tune the fit manually by setting the tuning variables such as alpha, beta and gamma
HW1 <- HoltWinters(Y)
HW2 <- HoltWinters(Y, alpha=0.2, beta=0.1, gamma=0.1)

plot(Y, ylab="Temperature", xlab="Years", xlim=c(2000,2015))
lines(HW1$fitted[,1], lty=2, col="blue")
lines(HW2$fitted[,1], lty=2, col="red")



#Using the predict function we'll need to specify how many data points we'll predict into the future. 
#Here, we'll use a value of 24 to project 2 years into the future
#We set the level of confidence interval = 0.95

HW1.pred <- predict(HW1, 24, prediction.interval = TRUE, level=0.95)
#Visually evaluate the prediction
plot(Y, ylab="Temperature Forecasting", xlim=c(2015,2018))
lines(HW1$fitted[,1], lty=2, col="blue")
lines(HW1.pred[,1], col="red")
lines(HW1.pred[,2], lty=2, col="orange")
lines(HW1.pred[,3], lty=2, col="orange")


#Using our HW1 Holt-Winters fit from before, we can use forecast to make new predictions and include both 80% and 95% confidence intervals.
library(forecast)
HW1_for <- forecast(HW1, h=24, level=c(80,95))
#visualize our predictions:
plot(HW1_for, xlim=c(2015, 2018))
lines(HW1_for$fitted, lty=2, col="purple")


