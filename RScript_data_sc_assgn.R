#========================================================================================
#PART 1: EXPLORING AND PREPARING DATA FOR ANALYSIS
#========================================================================================


#READING THE DATA:
#=================

#The data is first converted into the csv format and then read in R

data <- read.csv("C:/Users/Messi/Desktop/Data_scientist_assignment.csv", header=T)

head(data)		#Gives the first 6 obs. of the data
tail(data)		#Gives the last 6 obs. of the data



#CONVERTING THE DATA INTO data.frame:
#====================================

#The data is being converted into a data frame such that each rows of the data frame
#represents the unique dates and the columns represents the hours of a day, which
#gives a better visual representation of the entire data.

data.df <- as.data.frame(matrix(data$vals, 136, 24))
rownames(data.df) <- seq(as.Date("2014/5/1"), as.Date("2014/9/13"), "day")
colnames(data.df) <- seq(00,23)

data.df[1:10,]	#Checking out the first 10 obs.



#CONVERTING THE DATA AS "ts" CLASS:
#==================================

#In order to use various Time Series packages this data.df needs to be expressed
#as "ts" class. So, now we will convert the data.frame into ts class.

data.ts <- ts(as.vector(as.matrix(data.df)), start=c(as.numeric(as.Date("2014/5/1")),1), 
			end=c(as.numeric(as.Date("2014/9/13")),24), frequency=24) 

class(data.ts)

#The start and end of the ts() fnuction represents the starting time point and the ending
#time point of the time series data. An imortant thing over here is that the argument start
#and end can only take numerical values. That is why the dates has been converted into 
#numeric values using the as.numeric() function. In R the dates are stored as an integer
#such that the integer 0 indicates the date 1970-01-01


#SUMMARIZING THE DATA:
#====================

summary(data.ts)



#PLOTTING THE DATA:
#==================

#At the time of plotting it should be kept in mind that the time in data.ts is expressed 
#as integers so when we plot the data the x-axis will display the integers (eg. the date
#2014/5/1 will de displayed as an integer as 16191, which will be a little difficult to
#interpret. This we would like to modify x-axis such a way so that it displays dates instead
#of integers.

plot(data.ts,axes=F,main="Plotting the Time Series Data")		
				                                              #Plots data.ts without any axis 

box()				                      	                      #Creats a box around the plot

library(zoo)		              	#Used over here to convert the numeric figures back into dates

axis(1, seq(16200,16320,20), labels=as.Date(seq(16200,16320,20)))              #labelling x-axis, 
											                                            #i.e. axis(1)

axis(2)				                  #keeping the y-axis, i.e. axis(2) as it is


#Plotting the data with a regression line fitted to it

plot(data.ts,axes=F)
box()
axis(1, seq(16200,16320,20), labels=as.Date(seq(16200,16320,20)))
axis(2)
abline(reg=lm(data.ts~time(data.ts)),col="blue")           #Fitting a regression line to the data




#TIME SERIES DECOMPOSITION:
#==========================

#In order to decompose the time series data in to Trend, Seasonal components and Random
#errors we use the function decompose(). To use decompose() function the data should be
#a "ts" class.

d <- decompose(data.ts)
plot(d)


#========================================================================================
#PART 2: BUILDING MODELS:
#========================================================================================
# HOLT-WINTER'S MODEL:
#=====================

#Fitting Holt-Winter's Model to the Series and plotting the fitted data:

data.ts.hw <- HoltWinters(data.ts)

plot(data.ts.hw,axes=F)
plot(data.ts,axes=F)
box()
axis(1, seq(16200,16320,20), labels=as.Date(seq(16200,16320,20)))
axis(2)


#Making n-step ahead prediction using the fitted Holt Winters Model:
#n.ahead=10*24 implies making 240 hours ahead of prediction (ie, 20 days & 24 hours)  
#The code below predicts all the values from 1st hour ahead till the 20*24 hours ahead

data.predict <- predict(data.ts.hw, n.ahead = 20*24)

#Plotting the predictive values along with the series:
#The dotted line represents the forecasting values.

ts.plot(data.ts, data.predict, lty=1:2)


#some predictions

data.predict[1*1]
data.predict[1*3]
data.predict[2*5]
data.predict[5*7]
data.predict[8*15]
data.predict[1*12]
data.predict[10*12]
data.predict[10*24]
data.predict[15*20]
data.predict[15*24]
data.predict[16*24]

#We can clearly see that the Holt Winters model is not giving a good prediction.
#Let us now check for stationary or non-stationary models. This may be due to the unusual
#increase in spikes at the end of the series.

#Let us make some variable transformation and check if the Holt-Winters Model gives a 
#better forecast

#Taking Log Transformation of the data:

data.ts.log <- log(data.ts)
plot(data.ts.log)

#Fitting Holt-Winters to the Transformed data

data.ts.log.hw <- HoltWinters(data.ts.log)
plot(data.ts.log.hw)
data.predict2 <- predict(data.ts.log.hw, n.ahead = 20*24)
ts.plot(data.ts.log, data.predict2, lty=1:2)

#Some Predictions

exp(data.predict2[1*1])
exp(data.predict2[1*3])
exp(data.predict2[2*5])
exp(data.predict2[5*7])
exp(data.predict2[8*15])
exp(data.predict2[1*12])
exp(data.predict2[10*12])
exp(data.predict2[10*24])
exp(data.predict2[15*20])
exp(data.predict2[15*24])
exp(data.predict2[16*24])

#This forecasting seems to be more reliable compared to the previous ones.


#=======================================================================================
# STOCHASTIC MODELS
#=======================================================================================
#CHECKING FOR STATIONARITY
#=========================

#Checking for auto correation function & partial auto correlation function and displaying
#it on the same page.

par(mfrow=c(2,1))
acf(data.ts)
pacf(data.ts)


#TRANSFORMATION OF DATA - DIFFERENCING:
#=====================================

#From the library forecast using the ndiffs() finction we can find how many differences
#are needed to make the model stationary.

library(forecast)

ndiffs(data.ts)


#ndiffs() suggests that one difference is needed to make the model stationary.
#We take the difference as follows

data.diff <- diff(data.ts)

#Plotting the transformed data:

plot(data.diff, axes=F)
box()	
axis(1, seq(16200,16320,20), labels=as.Date(seq(16200,16320,20))) 
axis(2)				

#acf and pacf of the transformed data:

par(mfrow(2,1))
acf(data.diff)
pacf(data.diff)


#FITTING ARIMA(1,1,0) MODEL:
#===========================

data.fit1 <- arima(data.ts, order = c(1,1,0), seasonal=list(order=c(1,1,0),period=24,
            include.mean=FALSE))

data.fit1

data.pred1 <- predict(data.fit1, n.ahead=100)    #making 100 hrs ahead prediction

#plotting the time series along with the predicted values
#The predicted values are shown by the blue lines.
plot(data.ts, axes=F, main = "ARIMA(1,1,0) Model")                              
lines(data.pred1$pred,col="blue")
box()
axis(1, seq(16200,16320,20), labels=as.Date(seq(16200,16320,20)))
axis(2)



#=========================================================================================
#PART 4: MODEL SELECTION
#=========================================================================================

#Model selection procedure is based on AIC. The lower the AIC the better is the model.


#Finding the AIC of different ARIMA models:

#ARIMA(1,1,0)
AIC(data.fit1)

#ARIMA(2,1,0)
AIC(arima(data.ts, order = c(2,1,0), seasonal=list(order=c(2,1,0),period=24,include.mean=FALSE)))

#ARIMA(0,1,1)
AIC(arima(data.ts, order = c(0,1,1), seasonal=list(order=c(0,1,1),period=24,include.mean=FALSE)))

#ARIMA(0,1,2)
AIC(arima(data.ts, order = c(0,1,2), seasonal=list(order=c(0,1,2),period=24,include.mean=FALSE)))

#ARIMA(1,1,1)
AIC(arima(data.ts, order = c(1,1,1), seasonal=list(order=c(1,1,1),period=24,include.mean=FALSE)))

#ARIMA(2,1,1)
AIC(arima(data.ts, order = c(2,1,1), seasonal=list(order=c(2,1,1),period=24,include.mean=FALSE)))

#ARIMA(2,1,2)
AIC(arima(data.ts, order = c(2,1,2), seasonal=list(order=c(2,1,2),period=24,include.mean=FALSE)))



#Comparing the AIC of various ARIMA models that we have used so far the best ARIMA model turned out to be the ARIMA(2,1,2) model.

#Thus, it is a good idea to select the ARIMA(2,1,2) model. This is the final model we select.


#Fitting the ARIMA(2,1,2) model

data.fit <- arima(data.ts, order = c(2,1,2), seasonal=list(order=c(2,1,2),period=24 ,include.mean=FALSE))

data.pred <- predict(data.fit, n.ahead=100)   #making 100 hrs ahead prediction

#plotting the time series along with the predicted values
#The predicted values are shown by the blue lines.
plot(data.ts, axes=F, main = "Predicting using ARIMA(2,1,2) Model")                              
lines(data.pred$pred,col="blue")
box()
axis(1, seq(16200,16320,20), labels=as.Date(seq(16200,16320,20)))
axis(2)


#==========================================================================================
#PART 5: BUILDING CONFIDENCE INTERVAL
#==========================================================================================

#plotting the predicted data along with the confidence intervals:

plot(data.ts, axes=F, main = "Prediction using ARIMA(2,1,2) Model")                              
lines(data.pred$pred,col="blue")
box()
axis(1, seq(16200,16320,20), labels=as.Date(seq(16200,16320,20)))
axis(2)

#plotting the 95% C.I.
lines(data.pred$pred + 1.96*data.pred$se,col="red")   #UCI is represented by the red line
lines(data.pred$pred - 1.96*data.pred$se,col="green") #LCI is represented by the green line


#==========================================================================================
#PART 6: MAKING PREDICTION - FORECASTING
#==========================================================================================

library(forecast)

#Forecast 5 hours ahead:

forecast(data.fit1,5) 


#Building predictive function given date and time:
#=================================================
#the following function will take inputs as date and time and display the foreasted value and the 95% C.I. for that particular
#date and time.

pred <- function(date, time=0:23)
{
  
  d <- seq(as.Date("2014/9/14"),as.Date("2014/12/31"),"day")      #sequence of dates ahead of 2014-09-13 till 2014-12-31
  t <- seq(0,23,1)						  #hours of day from 0 hours to 23 hours
  
  
  n.days.ahead <- which(as.Date(date)==d)			  #No. of days ahead
  
  hour <- which(as.numeric(time) == t)				  #which() function is used to locate the position of a value
  								  #in a vector.
  
  
  x <- data.frame(forecast(data.fit, n.days.ahead*hour))          #n.days.ahead*hour = total no. of hours ahead
  
  val <- x[n.days.ahead*hour,1]
  lci <- x[n.days.ahead*hour,4]
  uci <- x[n.days.ahead*hour,5]
  
  out <- list(pred.val=c(val),CI=c(lci,uci))
  
  return(out)
}


#Test examples

pred(date="2014-09-15",time=9)
pred(date="2014-09-28",time=20)
pred(date="2014-09-14",time=23)
pred(date="2014-09-20",time=2)
pred(date="2014-09-23",time=10)
pred(date="2014-10-03",time=15)
pred(date="2014-10-26",time=8)
pred(date="2014-10-03",time=0)
pred(date="2014-11-12",time=5)
pred(date="2014-11-25",time=12)
pred(date="2014-12-31",time=23)

