#Loading necessary libraries
library(forecast)
library(zoo)
#Setting working directory
setwd("~/Documents/Mac Projects")
#Reading the Data
makeup.data <- read.csv("Amtrak_Ridershipdata.csv")
#Look at the data to understand frequency, gaps, errors (if any)
View(makeup.data)
#Calculate the summary statistics for the dataset.
summary(makeup.data)
#Reading it as a time series
makeup.data.ts <- ts(makeup.data$Demand,start=c(2000,1), end=c(2022,3), frequency=12)
#To see its effectiveness
print(makeup.data.ts)
#For overview and to cross verify with before and after
summary(makeup.data.ts)

#Create a scatter diagram, a boxplot, and an additional graph that provide insights regarding the dataset.

##ScatterPlot
plot(makeup.data$Demand, xlab ="Month", ylab="Demand")
#This helps in clearing the plot settings, preventing mini graphs
dev.off()

##BoxPlot
boxplot(makeup.data$Demand,xlab = "Month", ylab ="Demand", main="Demand over Time")
#For Better understanding of the Data distribution
boxplot.stats(makeup.data$Demand)
dev.off()
##Lineplot(using timeseries object we created earlier)
#end was chosen as 2022.25 because data ends 2022 March, 
#ylim chosen based on boxplot limits,
#0.25 gap between each marking on x axis to ensure we capture last quarter of the dataset
plot(makeup.data.ts, ylim = c(100, 600),  ylab = "Demand", xlab = "Time", bty = "l", xaxt = "n", 
     xlim = c(2000,2022.25), main = "Demand over Time")
axis(1, at = seq(2000,2022.25 , 1), labels = format(seq(2000, 2022.25, 1)))

#Conduct two forecasts (of your choosing) to predict the sales value of a property 
#by partitioning the data into training and validation sets.
nValid <- 36
nTrain <- length(makeup.data.ts)-nValid
train.ts <- window(makeup.data.ts, start=c(2000,1),end=c(2000,nTrain))
valid.ts <- window(makeup.data.ts, start=c(2000,nTrain+1),end=c(2000,length(makeup.data.ts)))

##Linear model
makeup.data.lm <- tslm(train.ts ~ trend)
makeup.data.lm.pred <- forecast(makeup.data.lm, h = nValid, level = 0)

##Quadratic model
makeup.data.qt <- tslm(train.ts ~ trend+I(trend^2))
makeup.data.lm2.pred <-forecast(makeup.data.qt, h = nValid, level = 0)

#Holt winters model(with multiplicative seasonality)
makeup.data.holt <- ets(train.ts, model="AAM",alpha = 0.2,beta = 0.15,gamma = 0.05, restrict = FALSE)
makeup.data.holt.pred <- forecast(makeup.data.holt, h=nValid, level=0)

#Plot a histogram of the forecast errors. Print the forecast errors and related statistics. 
#Compare the performance of the two forecasts. Comment on the results.
##Accuracy stats and Residual plots for each model
#Linear Model
accuracy(makeup.data.lm.pred,valid.ts)
hist(makeup.data.lm.pred$residuals,ylab="Frequency",xlab="Forecast Errors",bty="l",main="Linear Regression Forecast errors")
#Quadratic model
accuracy(makeup.data.lm2.pred,valid.ts)
hist(makeup.data.lm2.pred$residuals,ylab="Frequency",xlab="Forecast Errors",bty="l",main="Quadratic Regression Forecast errors")
#Holt winters model
accuracy(makeup.data.holt.pred)
hist(makeup.data.holt.pred$residuals,ylab="Frequency",xlab="Forecast Errors",bty="l",main="Holt winter's Forecast errors")

#Extra
##Plot the linear regression
plot(makeup.data.lm.pred, ylim = c(100, 600), ylab = "Demand", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2000,2025.25), main = "Linear Model Prediction of Demand", flty = 2) 
axis(1, at = seq(2000, 2025.25, 1), labels = format(seq(2000, 2025, 1)))
lines(makeup.data.lm.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)
