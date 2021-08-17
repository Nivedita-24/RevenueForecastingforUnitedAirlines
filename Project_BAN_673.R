
## FINAL PROJECT BAN_673 
##---------------------------------------------------------------------------------------------------------------------------
##---------------------------------------------------------------------------------------------------------------------------

## GOAL
## The dataset is collected from 'Bureau of Transportation Statistics' website. 
## It consists of quarterly operating revenue of the airlines from 1st quarter of 2000 to 1st quarter of 2021. 
## The goal is to predict the operating revenue of the last three quarters (for 2021) and one quarter of 2022 for United Airlines. 

##---------------------------------------------------------------------------------------------------------------------------
##---------------------------------------------------------------------------------------------------------------------------

## IMPORTING AND LOADING R PACKAGES

library(forecast)
library(zoo)
library(dplyr)
#install.packages("stringr", repos='http://cran.us.r-project.org')
library("stringr")

# Set working directory for locating files.
setwd("/Users/niveditasharma/Documents/Business_Analytics_CSUEB/BAN_673/Project")

##--------------------------------------------------------------------------------------------------------------------------
##--------------------------------------------------------------------------------------------------------------------------

## GETTING THE DATA AND PRE-PROCESSING

# Create data frame.
OpR.data <- read.csv("Operating_Rev_7_9_2021 2_00_30 AM.csv")

#check the number of rows and columns in the dataset
dim(OpR.data)

#Check for duplicate data
OpR.data[duplicated(OpR.data)]

#Check for null data using Summary Statistics
summary(OpR.data)

#Check head and tail of data
head(OpR.data)
tail(OpR.data)

#Eliminating rows with Total value in Quarter column
OpR.data <- OpR.data[- grep("TOTAL", OpR.data$Quarter),]

#check the number of rows and columns in the dataset.
dim(OpR.data) 

#Check head and tail of data.
head(OpR.data)
tail(OpR.data)

#Adding Q in front for Quarter column.
OpR.data$Quarter<-sub("^","Q",OpR.data$Quarter)
OpR.data

#Combine Quarter and Year into one column.
OpR.data$Quarter_Year <- paste(OpR.data$Quarter, OpR.data$Year)
OpR.data

#Dropping unwanted columns
OpR.data = select(OpR.data, -1:-7)
OpR.data

#Reorder columns
OpR.data <- OpR.data[c(2,1)]
OpR.data
dim(OpR.data)

sapply(OpR.data, class) 
#Removing , from total column 
OpR.data$TOTAL <- str_replace_all(OpR.data$TOTAL, "[^[:alnum:]]", "")

#checking the data
head(OpR.data)

#To check the class of each column
sapply(OpR.data, class) 

#Writing the preprocessed data into new csv
write.csv(OpR.data, file="UnitedAirlinePreprocess.csv")

#Reading the cleaned dataset
United.data <-read.csv("UnitedAirlinePreprocess.csv")
summary(United.data)
dim(United.data) # dataset contains records for 85 quarters
str(United.data)

## Converting the Revenue in (000s) for readability
United.data$TOTAL <- (United.data[1:85,3]/(1000))

#See the first and last 6 records of the file.
head(United.data)
tail(United.data)

##---------------------------------------------------------------------------------------------------------------------------
## EXPLORING AND VISUALIZING SERIES

## USE ts() FUNCTION TO CREATE TIME SERIES DATA SET.
## USE stl() FUNCTION TO PLOT TIME SERIES COMPONENTS 
## USE Acf() FUNCTION TO IDENTIFY AUTOCORRELATION

# Function ts() takes three arguments: start, end, and freq.
# With monthly data, frequency (freq) of periods in a season (year) is 12. 
# With quarterly data, frequency in a season (year) is equal to 4.
# Arguments start and end are pairs: (season number, period number).
united.ts <- ts(United.data$TOTAL, 
                start = c(2000, 1), end = c(2021, 1), freq = 4)
united.ts

## VISUALIZING TIME SERIES---------------------------------------------------------------------

## Use plot() to plot time series data  
options(scipen = 100)
plot(united.ts, 
     xlab = "Time", ylab = "United Airline's Quarterly Operating Revenue (in 000s)", ylim = c(1475, 12000), main = "United Airlines Revenue",xaxt = "n", xlim = c(2000, 2022), col = "blue")
axis(1, at = seq(2000, 2022, 1), labels = format(seq(2000, 2022, 1)))

## TIME SERIES COMPONENTS-----------------------------------------------------------------------

# Use stl() function to plot times series components of the original data. 
# The plot includes original data, trend, seasonal, and reminder 
# (level and noise component).
united.stl <- stl(united.ts, s.window = "periodic")
autoplot(united.stl, main = "United Airline's Revenue Time Series Components")

## AUTOCORRELATION------------------------------------------------------------------------------

# Use Acf() function to identify autocorrelation and plot autocorrelation
# for different lags (up to maximum of 8).
autocor <- Acf(united.ts, lag.max = 8, main = "Autocorrelation for United Airline's Revenue")

# Display autocorrelation coefficients for various lags.
Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF) ## Showing strong trend and significant seasonality.

## TEST PREDICTABILITY OF THE DATASET-------------------------------------------------------------

# APPROACH 1 : HYPOTHESIS TESTING
# Use Arima() function to fit AR(1) model for United Airline's quarterly revenue
# The ARIMA model of order = c(1,0,0) gives an AR(1) model.
united.rev.ar1<- Arima(united.ts, order = c(1,0,0))
summary(united.rev.ar1)

# B1 = 0.9169 : test statistic
# HO: B1 = 1,  Ha: B1 <> 1
# Z-statistic = (B1 - 1) / s.e. = (0.9169 - 1)/0.0405 
zstat =  (0.9169 - 1)/0.0405
zstat # zstat = -2.051852

# Calculate p value for z-stat
p.value = pnorm(zstat)
p.value 
# pvalue is 0.02009203 # Since the p-value is below the threshold value of 0.05, 
# we can reject the null hypothesis which is B1 = 0 in this case, 
# Therefore, we can conclude that the B1 is not equal to 1 and the dataset is predictable.

# APPROACH 2: DIFFERENCED DATA

# Create first difference of the data using diff() function.
diff.united.rev <- diff(united.ts, lag = 1)
diff.united.rev

# Use plot() function to create plot for first differenced data. 
plot(diff.united.rev, 
     xlab = "Time", ylab = "Revenue in (000s)", main = "First Differencing of United Airline's Revenue", 
     xaxt = "n", bty = "l", lty = 5, lwd = 2, col="orange")
axis(1, at = seq(2000, 2022, 1), labels = format(seq(2000, 2022, 1)))

# Develop data frame with United Airline's Revenue, United Airline's Revenue lag 1, and first
# differenced data.
diff.df <- data.frame(united.ts, c("", round(united.ts[1:84],2)), 
                      c("", round(diff.united.rev,2)))

names(diff.df) <- c("United Airline's Revenue", "United Airline's Revenue lag 1", 
                    "First Difference")
diff.df 

# Use Acf() function to identify autocorrealtion for first differenced
# United Airline's Revenue and plot autocorrelation for different lags 
# (up to maximum of 8).
Acf(diff.united.rev, lag.max = 8, 
    main = "Autocorrelation for United Airline Differenced Revenue")

## looking at the autocorrelation chart we can see that the autocorrelation is non-significant in almost all the lags except for lag 2, which means that the data is most likely random walk as per this test.

## PREDCTABILITY DECISION
## As both the approaches are giving different results
## as per hypothesis test the dataset is predictable whereas as per differenced data approach the dataset appears to be random walk
## So, we can say that the results are inconclusive, So we'll go ahead and build various models and see which model performs best, based the accuracy metrics to forecast the revenue.

##---------------------------------------------------------------------------------------------------------------------------
##---------------------------------------------------------------------------------------------------------------------------

## PARTITIONING TIME SERIES
## PLOT DATA PARTITION.

# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
nValid <- 20 ## Partitioned the data in a ratio of 77% and 23% for training and validation data respectively.
nTrain <- length(united.ts) - nValid
train.ts <- window(united.ts, start = c(2000, 1), end = c(2000, nTrain))
valid.ts <- window(united.ts, start = c(2000, nTrain + 1), 
                   end = c(2000, nTrain + nValid))

# Plot the time series data and visualize partitions. 
plot(train.ts, 
     xlab = "Time", ylab = "United Airline's Quarterly Operating Revenue (in 000s)", ylim = c(1475, 12000), bty = "l",
     xaxt = "n", xlim = c(2000, 2023), main = "", lwd = 2) 
axis(1, at = seq(2000, 2023, 1), labels = format(seq(2000, 2023, 1)))
lines(valid.ts, col = "black", lty = 1, lwd = 2)

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2021.25 - 5, 2021.25 - 5), c(0, 12002))
lines(c(2021.25, 2021.25), c(0, 12002))
text(2008, 12000, "Training")
text(2018.80, 12000, "Validation")
text(2022.5, 12000, "Future")
arrows(2021.25 - 5.5, 11500, 2000, 11500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2021 , 11500, 2016.5, 11500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.5, 11500, 2021.5, 11500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## AUTOCORRELATION IN TRAINING AND VALIDATION DATA

# Use Acf() function to identify autocorrelation and plot autocorrelation for training and validation data
# for different lags (up to maximum of 8).
autocor <- Acf(train.ts, lag.max = 8, main = "Autocorrelation for United Airline's Training Dataset")
## Training partition captures data with strong trend and  seasonality.
autocor <- Acf(valid.ts, lag.max = 8, main = "Autocorrelation for United Airline's Validation Dataset")
## ## Validation partition captures data with strong trend but non-significant seasonality.

##---------------------------------------------------------------------------------------------------------------------------

## APPLYING FORECASTING METHODS

##---------------------------------------------------------------------------------------------------------------------------

# MODEL 1
## IDENTIFY NAIVE AND SEASONAL NAIVE FORECASTS.

# Use naive() to make naive forecast for validation data. 
# Use snaive() to make seasonal naive forecast for validation data. 
United.naive.pred <- naive(train.ts, h = nValid)
United.snaive.pred <- snaive(train.ts, h = nValid)

# Plot the predictions for naive forecast.
plot(United.naive.pred$mean, 
     xlab = "Time", ylab = "United Airline's Quarterly Operating Revenue (in 000s)", ylim = c(1475, 12000), bty = "l",
     xaxt = "n", xlim = c(2000, 2023), main = "Naive Forecast", col = "blue", lwd =2) 
axis(1, at = seq(2000, 2023, 1), labels = format(seq(2000, 2023, 1)))
lines(United.naive.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2021.25 - 5, 2021.25 - 5), c(0, 12002))
lines(c(2021.25, 2021.25), c(0, 12002))
text(2008, 12000, "Training")
text(2018.80, 12000, "Validation")
text(2022.5, 12000, "Future")
arrows(2021.25 - 5.5, 11500, 2000, 11500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2021 , 11500, 2016.5, 11500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.5, 11500, 2021.5, 11500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# Plot the predictions for seasonal naive forecast.
plot(United.snaive.pred$mean, 
     xlab = "Time", ylab = "United Airline's Quarterly Operating Revenue (in 000s)", ylim = c(1475, 12000), bty = "l",
     xaxt = "n", xlim = c(2000, 2023), main = "Seasonal Naive Forecast", col = "blue", lwd =2) 
axis(1, at = seq(2000, 2023, 1), labels = format(seq(2000, 2023, 1)))
lines(United.snaive.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2021.25 - 5, 2021.25 - 5), c(0, 12002))
lines(c(2021.25, 2021.25), c(0, 12002))
text(2008, 12000, "Training")
text(2018.80, 12000, "Validation")
text(2022.5, 12000, "Future")
arrows(2021.25 - 5.5, 11500, 2000, 11500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2021 , 11500, 2016.5, 11500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.5, 11500, 2021.5, 11500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## IDENTIFY FORECAST ACCURACY FOR NAIVE and SEASONAL NAIVAE FORECASTS.

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(United.naive.pred$mean, valid.ts), 3)
round(accuracy(United.snaive.pred$mean, valid.ts), 3)

##----------------------------------------------------------------------------------------------------------------------------
##----------------------------------------------------------------------------------------------------------------------------

## IMPORTANT NOTE:
## The MAPE on validation data for both the models is around 60%, which is quite high. 
## The plot of the entire timeseries shows that the there is a drastic decrease in the revenue during Covid-19, distorting the curve for an entire period of 2020 and first quarter of 2021
## Also, the correlogram for validation data shows that the relationships in the validation partition are mostly random except for lag-1.

## In order to incorporate the impact of these unprecendented times during covid-19 we have build all the models on the entire dataset in place of training partition
## And, selected the best model for forecast based on the accuracy metrics for entire dataset.

##----------------------------------------------------------------------------------------------------------------------------
##----------------------------------------------------------------------------------------------------------------------------

## MODEL2
## REGRESSION MODELS

## FIT REGRESSION MODEL WITH LINEAR TREND: MODEL 2A. 
## FORECAST AND PLOT DATA, AND MEASURE ACURACY.

# Use tslm() function (time series linear model) to create regression model with 
# linear trend.
united.lin <- tslm(united.ts ~ trend)

# See summary of linear trend model and associated parameters.
summary(united.lin)
## Adj R-sq is 0.44, which shows that 44% of variation in revenue can be explained by the variation in trend.

# Plot ts data, regression model with linear trend data
plot(united.lin$fitted, 
     xlab = "Time", ylab = "United Airline's Quarterly Operating Revenue (in 000s)", ylim = c(1475, 12000), bty = "l",
     xlim = c(2000, 2023), 
     main = "Regression Model with Linear Trend", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(2000, 2023, 1), labels = format(seq(2000, 2023, 1)))
lines(united.ts)
legend(2000,3000, legend = c("United Airline's Time Series", 
                             "Linear Trend Model for Entire Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training intervals.
lines(c(2021.25, 2021.25), c(0, 12002))
text(2010, 12000, "Training - Entire Dataset")
arrows(2021 , 11500, 2000, 11500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

##----------------------------------------------------------------------------------------------------------------------------

## FIT REGRESSION MODEL WITH EXPONENTIAL TREND: MODEL 2B
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create regression model with exponential trend.
# If lambda = 0, tslm function applies Box-Cox transformation
# for log(y) - exponential trend.
# If lambda = 1, tslm function will just have a linear trend
united.expo <- tslm(united.ts ~ trend, lambda = 0)

# See summary of exponential trend model and associated parameters.
summary(united.expo)
## Adj R-sq is 0.34, which shows that 34% of variation in revenue can be explained by the variation in trend.

# Plot ts data, regression model with exponential trend model
plot(united.expo$fitted, 
     xlab = "Time", ylab = "United Airline's Quarterly Operating Revenue (in 000s)", ylim = c(1475, 12000), bty = "l",
     xlim = c(2000, 2023), 
     main = "Regression Model with Exponential Trend", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(2000, 2023, 1), labels = format(seq(2000, 2023, 1)))
lines(united.ts)
legend(2000,3000, legend = c("United Airline's Time Series", 
                             "Exponential Trend Model for Entire Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training intervals.
lines(c(2021.25, 2021.25), c(0, 12002))
text(2010, 12000, "Training - Entire Dataset")
arrows(2021 , 11500, 2000, 11500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

##----------------------------------------------------------------------------------------------------------------------------
## FIT REGRESSION MODEL WITH QUADRATIC (POLYNOMIAL) TREND: MODEL 2C
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create quadratic (polynomial) trend model.
united.quad <- tslm(united.ts ~ trend + I(trend^2))

# See summary of quadratic trend model and associated parameters.
summary(united.quad)
## Adj R-sq is 0.45, which shows that 45% of variation in revenue can be explained by the variation in trend.


# Plot ts data, regression model with quadratic trend model
plot(united.quad$fitted, 
     xlab = "Time", ylab = "United Airline's Quarterly Operating Revenue (in 000s)", ylim = c(1475, 12000), bty = "l",
     xlim = c(2000, 2023), 
     main = "Regression Model with Quadratic Trend", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(2000, 2023, 1), labels = format(seq(2000, 2023, 1)))
lines(united.ts)
legend(2000,3000, legend = c("United Airline's Time Series", 
                             "Quadratic Trend Model for Entire Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training intervals.
lines(c(2021.25, 2021.25), c(0, 12002))
text(2010, 12000, "Training - Entire Dataset")
arrows(2021 , 11500, 2000, 11500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

##----------------------------------------------------------------------------------------------------------------------------

## FIT REGRESSION MODEL WITH 3rd ORDER (POLYNOMIAL) TREND: MODEL 2D
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create 3rd order (polynomial) trend model.
united.cube <- tslm(united.ts ~ trend + I(trend^2) + I(trend^3) )

# See summary of 3rd order (polynomial) trend model and associated parameters.
summary(united.cube)
## Adj R-sq is 0.71, which shows that 71% of variation in revenue can be explained by the variation in trend.

# Plot ts data, regression model with 3rd order(polynomial) trend model
plot(united.cube$fitted, 
     xlab = "Time", ylab = "United Airline's Quarterly Operating Revenue (in 000s)", ylim = c(1475, 12000), bty = "l",
     xlim = c(2000, 2023), 
     main = "Regression Model with 3rd Order (Polynomail) Trend", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(2000, 2023, 1), labels = format(seq(2000, 2023, 1)))
lines(united.ts)
legend(2000,3000, legend = c("United Airline's Time Series", 
                             "3rd Order(polynomial) Trend Model for Entire Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training intervals.
lines(c(2021.25, 2021.25), c(0, 12002))
text(2010, 12000, "Training - Entire Dataset")
arrows(2021 , 11500, 2000, 11500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## CHECKING ACCURACY OF REGRESSION MODELS WITH DIFFERENT TRENDS
## (1) Regression Model with linear trend
## (2) Regression Model with exponential trend
## (3) Regression Model with Quadratic trend
## (4) Regression Model with 3rd order polynomial trend

round(accuracy(united.lin$fitted, united.ts),3)
round(accuracy(united.expo$fitted, united.ts),3)
round(accuracy(united.quad$fitted, united.ts),3)
round(accuracy(united.cube$fitted, united.ts),3)

## Looking at the accuracy metrics of regression models for entire dataset, the two models with lowest MAPE are regression model with exponential trend and regression model with 3rd order (polynomial) trend
##----------------------------------------------------------------------------------------------------------------------------

## FIT REGRESSION MODEL WITH SEASONALITY: MODEL 2E. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create seasonal model.
united.season <- tslm(united.ts ~ season)

# See summary of seasonal model and associated parameters.
summary(united.season)
## Adj R-sq is 0.015, which shows that 1.5% of variation in revenue can be explained by the variation in seasonality.

# Plot ts data, regression model with seasonal model
plot(united.season$fitted, 
     xlab = "Time", ylab = "United Airline's Quarterly Operating Revenue (in 000s)", ylim = c(1475, 12000), bty = "l",
     xlim = c(2000, 2023), 
     main = "Regression Model with Seasonality", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(2000, 2023, 1), labels = format(seq(2000, 2023, 1)))
lines(united.ts)
legend(2000,3000, legend = c("United Airline's Time Series", 
                             "Seasonal Model for Entire Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training intervals.
lines(c(2021.25, 2021.25), c(0, 12002))
text(2010, 12000, "Training - Entire Dataset")
arrows(2021 , 11500, 2000, 11500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

##----------------------------------------------------------------------------------------------------------------------------

## FIT REGRESSION MODEL WITH EXPONENTIAL TREND AND SEASONALITY: MODEL 2F
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create exponential trend and seasonal model.
united.expotrend.season <- tslm(united.ts ~ trend + season, lambda = 0 )

# See summary of exponential trend and seasonality model and associated parameters.
summary(united.expotrend.season)
## Adj R-sq is 0.35, which shows that 35% of variation in revenue can be explained by the variation in trend and seasonality.

# Plot ts data, regression model with seasonal model
plot(united.expotrend.season$fitted, 
     xlab = "Time", ylab = "United Airline's Quarterly Operating Revenue (in 000s)", ylim = c(1475, 12000), bty = "l",
     xlim = c(2000, 2023), 
     main = "Regression Model with Exponential Trend and Seasonality", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(2000, 2023, 1), labels = format(seq(2000, 2023, 1)))
lines(united.ts)
legend(2000,3000, legend = c("United Airline's Time Series", 
                             "Exponential Trend and Seasonality Model for Entire Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training intervals.
lines(c(2021.25, 2021.25), c(0, 12002))
text(2010, 12000, "Training - Entire Dataset")
arrows(2021 , 11500, 2000, 11500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


##----------------------------------------------------------------------------------------------------------------------------

## FIT REGRESSION MODEL WITH 3rd ORDER (POLYNOMIAL) TREND AND SEASONALITY: MODEL 2G
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create 3rd order(polynomial) trend and seasonal model.
united.cubetrend.season <- tslm(united.ts ~ trend + I(trend^2) + I(trend^3) + season )

# See summary of exponential trend and seasonality model and associated parameters.
summary(united.cubetrend.season)
## Adj R-sq is 0.72, which shows that 72% of variation in revenue can be explained by the variation in trend and seasonality.


# Plot ts data, regression model with seasonal model
plot(united.cubetrend.season$fitted, 
     xlab = "Time", ylab = "United Airline's Quarterly Operating Revenue (in 000s)", ylim = c(1475, 12000), bty = "l",
     xlim = c(2000, 2023), 
     main = "Regression Model with 3rd Order Polynomial Trend and Seasonality", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(2000, 2023, 1), labels = format(seq(2000, 2023, 1)))
lines(united.ts)
legend(2000,3000, legend = c("United Airline's Time Series", 
                             "3rd Order Polynomial Trend and Seasonality Model for Entire Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training intervals.
lines(c(2021.25, 2021.25), c(0, 12002))
text(2010, 12000, "Training - Entire Dataset")
arrows(2021 , 11500, 2000, 11500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

## CHECKING ACCURACY OF REGRESSION MODELS WITH DIFFERENT TRENDS AND SEASONALITY
## (1) Regression Model with Exponential trend and Seasonality
## (2) Regression Model with 3rd order polynomial trend and Seasonality

round(accuracy(united.expotrend.season$fitted, united.ts),3)
round(accuracy(united.cubetrend.season$fitted, united.ts),3)

## Looking at the accuracy metrics we can see that the model with lowest MAPE is Regression Model with 3rd order polynomial trend and Seasonality.

#-----------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------
## MODEL 3
## TWO-LEVEL MODEL - REGRESSION WITH 3RD ORDER (POLYNOMIAL TREND) AND SEASONALITY + TRAILING MA

## CALCULATING RESIDUALS FOR REGRESSION WITH 3RD ORDER (POLYNOMIAL TREND) AND SEASONALITY

# Identify and display regression residuals for entire data set.
united.trend.seas.res <- united.cubetrend.season$residuals
united.trend.seas.res


## TRAILING MA FORECAST FOR RESIDUALS
# # Create trailing MA with window widths (number of periods)  k = 2, 4, 6, 8 and 12.

# In rollmean(), use argument align = "right" to calculate a trailing MA.
ma.trailing_2 <- rollmean(united.trend.seas.res, k = 2, align = "right")
ma.trailing_4 <- rollmean(united.trend.seas.res, k = 4, align = "right")
ma.trailing_6 <- rollmean(united.trend.seas.res, k = 6, align = "right")
ma.trailing_8 <- rollmean(united.trend.seas.res, k = 8, align = "right")
ma.trailing_12 <- rollmean(united.trend.seas.res, k = 12, align = "right")

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(ma.trailing_2, united.trend.seas.res), 3)
round(accuracy(ma.trailing_4, united.trend.seas.res), 3)
round(accuracy(ma.trailing_6, united.trend.seas.res), 3)
round(accuracy(ma.trailing_8, united.trend.seas.res), 3)
round(accuracy(ma.trailing_12, united.trend.seas.res), 3)

## MAPE is lowest for window width 6 for the entire dataset.

# Apply trailing MA for residuals with window width k = 6. 
ma.trail.res <- rollmean(united.trend.seas.res, k = 6, align = "right")
ma.trail.res

# Develop 2-level forecast for the entire dataset by combining 
# regression forecast and trailing MA for residuals for the entire dataset
tot.fst.2level <- united.cubetrend.season$fitted + ma.trail.res
tot.fst.2level

# Plot ts data, regression model with seasonal model
plot(tot.fst.2level, 
     xlab = "Time", ylab = "United Airline's Quarterly Operating Revenue (in 000s)", ylim = c(1475, 12000), bty = "l",
     xlim = c(2000, 2023), 
     main = "Two level Regression Trend and Seasonality + MA Model", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(2000, 2023, 1), labels = format(seq(2000, 2023, 1)))
lines(united.ts)
legend(2000,3000, legend = c("United Airline's Time Series", 
                             "Two level Model Regression Trend and Seasonality + Trailing MA for Entire Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training intervals.
lines(c(2021.25, 2021.25), c(0, 12002))
text(2010, 12000, "Training - Entire Dataset")
arrows(2021 , 11500, 2000, 11500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# MEASURE FORECAST ACCURACY FOR ENTIRE DATA SET.
round(accuracy(united.cubetrend.season$fitted+ma.trail.res, united.ts), 3)

## Accuracy improved from MAPE = 23% to 15% by incorporating trailing MA for residuals.

#-----------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------
## MODEL 4
## HOLT- WINTER'S MODEL

## HOLT-WINTER'S (HW) EXPONENTIAL SMOOTHING WITH PARTITIONED DATA, AUTOMATED
## ERROR, TREND and SEASONALITY (ZZZ) OPTIONS, AND OPTIMAL PARAMETERS
## ALPHA, BETA, AND GAMMA.

# Create Holt-Winter's (HW) exponential smoothing for the entire data.
# Use ets() function with model = "ZZZ", i.e., automated selection 
# error, trend, and seasonality options.
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw.ZZZ <- ets(united.ts, model = "ZZZ")
hw.ZZZ


# Plot ts data, regression model with Holt Winter's model
plot(hw.ZZZ$fitted, 
     xlab = "Time", ylab = "United Airline's Quarterly Operating Revenue (in 000s)", ylim = c(1475, 12000), bty = "l",
     xlim = c(2000, 2023), 
     main = "Holt-Winter's Model with Automated Selection of Model Options", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(2000, 2023, 1), labels = format(seq(2000, 2023, 1)))
lines(united.ts)
legend(2000,3000, legend = c("United Airline's Time Series", 
                             "Holt-Winter's Automated Model for Entire Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training intervals.
lines(c(2021.25, 2021.25), c(0, 12002))
text(2010, 12000, "Training - Entire Dataset")
arrows(2021 , 11500, 2000, 11500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# MEASURE FORECAST ACCURACY FOR ENTIRE DATA SET.
round(accuracy(hw.ZZZ$fitted, united.ts), 3)

#-----------------------------------------------------------------------------------------------------------------------------
## MODEL 5
## TWO LEVEL MODEL: REGRESSION WITH 3RD ORDER (POLYNOMIAL TREND) AND SEASONALITY + AUTOREGRESSIVE MODEL

## Residuals for Regression Model with 3rd Order (Polynomial) trend and seasonality

# Plot residuals of the predictions with Quadratic trend and seasonality.
plot(united.cubetrend.season$residuals, 
     xlab = "Time", ylab = "Residuals", bty = "l",
     xaxt = "n", xlim = c(2000, 2023), 
     main = "Regression Residuals for Entire Data", 
     col = "brown", lwd = 2) 
axis(1, at = seq(2000, 2023, 1), labels = format(seq(2000, 2023, 1)))

# plot on the chart vertical lines and horizontal arrows
# describing training intervals.
lines(c(2021.25, 2021.25), c(-6000, 5000))
text(2010, 3700, "Training - Entire Dataset")
arrows(2021 , 3500, 2000, 3500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use Acf() function to identify autocorrelation for the model residuals 
# plot autocorrelation for different 
# lags (up to maximum of 8).
Acf(united.cubetrend.season$residuals, lag.max = 8, 
    main = "Autocorrelation for United Airline's Residuals")

# We'll use order 1 auto regressive model because autocorrelation is highest for lag 1 for residuals.

## USE Arima() FUNCTION TO CREATE AR(1) MODEL FOR THE RESIDUALS.
## CREATE TWO-LEVEL MODEL WITH LINEAR TREND AND SEASONALITY MODEL 
## AND AR(1) RESIDUALS.
## PLOT DATA AND IDENTIFY ACCURACY MEASURES.

# Use Arima() function to fit AR(1) model for training residuals. The Arima model of 
# order = c(1,0,0) gives an AR(1) model.
# Use summary() to identify parameters of AR(1) model. 
res.ar1 <- Arima(united.cubetrend.season$residuals, order = c(1,0,0))
summary(res.ar1)
res.ar1$fitted

# Develop a data frame to demonstrate the training AR model results 
# vs. original training series, training regression model, 
# and its residuals.  
R_AR_Model.df <- data.frame(united.ts, united.cubetrend.season$fitted, 
                       united.cubetrend.season$residuals, res.ar1$fitted, res.ar1$residuals)
names(R_AR_Model.df) <- c("United Airline's Revenue", "Regression", "Residuals",
                     "AR.Model", "AR.Model.Residuals")
R_AR_Model.df

# Plot residuals of the residuals for training data before AR(1).
plot(united.cubetrend.season$residuals, 
     xlab = "Time", ylab = "Residuals",ylim = c(-6000,5000), bty = "l",
     xaxt = "n", xlim = c(2000, 2023), 
     main = "Regresssion Residuals for Entire Data before AR(1)", 
     col = "brown", lwd = 3) 
axis(1, at = seq(2000, 2023, 1), labels = format(seq(2000, 2023, 1)))

# plot on the chart vertical lines and horizontal arrows
# describing training intervals.
lines(c(2021.25, 2021.25), c(-6000, 5000))
text(2010, 4000, "Training - Entire Dataset")
arrows(2021 , 3500, 2000, 3500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# Plot residuals of the residuals for training data after AR(1).
plot(res.ar1$residuals, 
     xlab = "Time", ylab = "Residuals",ylim = c(-6000,5000), bty = "l",
     xaxt = "n", xlim = c(2000, 2023), 
     main = "Regresssion Residuals for Entire Data after AR(1)", 
     col = "brown", lwd = 3) 
axis(1, at = seq(2000, 2023, 1), labels = format(seq(2000, 2023, 1)))

# plot on the chart vertical lines and horizontal arrows
# describing training intervals.
lines(c(2021.25, 2021.25), c(-6000, 5000))
text(2010, 4000, "Training - Entire Dataset")
arrows(2021 , 3500, 2000, 3500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# Use Acf() function to identify autocorrelation for the entire dataset 
# residual of residuals and plot autocorrelation for different lags 
# (up to maximum of 8).
Acf(res.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for United Airline's Training Residuals of Residuals")

## We can observe that all the relationships in the dataset are incorporated in the model leaving just noise/randomness in the residuals.

# plot ts data, trend and seasonality data, and predictions for validation period.
plot(united.cubetrend.season$fitted + res.ar1$fitted, 
     xlab = "Time", ylab = "United Airline's Quarterly Operating Revenue (in 000s)", ylim = c(1475, 12000), bty = "l",
     xlim = c(2000, 2023), 
     main = "Two level Regression Trend and Seasonality + AR(1) Model", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(2000, 2023, 1), labels = format(seq(2000, 2023, 1)))
lines(united.ts)
legend(2000,3000, legend = c("United Airline's Time Series", 
                             "Two level Model Regression Trend and Seasonality + AR(1) for Entire Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")


# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
# plot on the chart vertical lines and horizontal arrows
# describing training intervals.
lines(c(2021.25, 2021.25), c(0, 12002))
text(2010, 12000, "Training - Entire Dataset")
arrows(2021 , 11500, 2000, 11500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# MEASURE FORECAST ACCURACY FOR ENTIRE DATA SET.
round(accuracy(united.cubetrend.season$fitted + res.ar1$fitted, united.ts), 3)
#-----------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------
## MODEL 6
## ARIMA Model

## FIT AUTO ARIMA MODELS FOR ENTIRE DATA SET. 

# Use auto.arima() function to fit ARIMA model for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
auto.arima <- auto.arima(united.ts)
summary(auto.arima)

# Use Acf() function to create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(auto.arima$residuals, lag.max = 8, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")

# Plot historical data, predictions for historical data, and Auto ARIMA 
plot(auto.arima$fitted, 
     xlab = "Time", ylab = "United Airline's Quarterly Operating Revenue (in 000s)", ylim = c(1475, 12000), bty = "l",
     xlim = c(2000, 2023), 
     main = "ARIMA Model with Automated Selection of Model Options", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(2000, 2023, 1), labels = format(seq(2000, 2023, 1)))
lines(united.ts)
legend(2000,3000, legend = c("United Airline's Time Series", 
                             "Auto ARIMA Model for Entire Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")


# plot on the chart vertical lines and horizontal arrows
# describing training intervals.
lines(c(2021.25, 2021.25), c(0, 12002))
text(2010, 12000, "Training - Entire Dataset")
arrows(2021 , 11500, 2000, 11500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# MEASURE FORECAST ACCURACY FOR ENTIRE DATA SET.
round(accuracy(auto.arima$fitted, united.ts), 3)

##----------------------------------------------------------------------------------------------------------------------------
##----------------------------------------------------------------------------------------------------------------------------

## EVALUATING AND COMPARING PERFORMANCE (ENTIRE DATASET)
##----------------------------------------------------------------------------------------------------------------------------

#---------------------------Naive and Seasonal Naive models accuracy----------------------------------------------------------
#Accuracy for Naive Forecast
round(accuracy((naive(united.ts))$fitted, united.ts), 3)

#Accuracy for Seasonal Naive Forecast
round(accuracy((snaive(united.ts))$fitted, united.ts), 3)

#---------------------------Regressions models accuracy-----------------------------------------------------------------------

#Accuracy for Model 2a: Regression model with linear trend
round(accuracy(united.lin$fitted, united.ts),3)

#Accuracy for Model 2b:  Regression Model with Exponential Trend:
round(accuracy(united.expo$fitted, united.ts),3)

#Accuracy for Model 2c:Regression mode with quadratic trend
round(accuracy(united.quad$fitted, united.ts),3)

#Accuracy for Model 2d:Regression mode with 3rd Order (polynomial) trend
round(accuracy(united.cube$fitted, united.ts),3)

#Accuracy for Model 2e:Regression model with seasonality
round(accuracy(united.season$fitted, united.ts),3)

#Accuracy for Model 2f: Regression model with Exponential trend and seasonality.  
round(accuracy(united.expotrend.season$fitted, united.ts),3)

#Accuracy for Model 2g: Regression model with 3rd Order(Polynomial) trend and seasonality.  
round(accuracy(united.cubetrend.season$fitted, united.ts),3)
#-----------------------------------------------------------------------------------------------------------------------------

#------------TWO level model with Regression and trailing_MA----------------------------------------------------------------
round(accuracy(united.cubetrend.season$fitted+ma.trail.res, united.ts), 3)
#-----------------------------------------------------------------------------------------------------------------------------

#---------------------------Holts Winter model--------------------------------------------------------------------------------
round(accuracy(hw.ZZZ$fitted, united.ts), 3)
#------------------------------------------------------------------------------------------------------------------------------

#------------TWO level model with Regression and AR(1)----------------------------------------------------------------
round(accuracy(united.cubetrend.season$fitted + res.ar1$fitted, united.ts), 3)
#------------------------------------------------------------------------------------------------------------------------------

#--------------------------------ARIMA MODEL-----------------------------------------------------------------------------------
round(accuracy(auto.arima$fitted, united.ts), 3)
#------------------------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------------------------

## IMPLEMENT THE BEST MODEL(ARIMA) TO FORECAST THE REVENUE OF UNITED AIRLINES
##-----------------------------------------------------------------------------------------------------------------------------
## ARIMA MODELS
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 4 periods. 
auto.arima.pred <- forecast(auto.arima, h = 4, level = c(80,95))
auto.arima.pred

# Plot historical data, predictions for historical data, and Auto ARIMA 
# forecast for 4 future periods.
plot(united.ts, 
     xlab = "Time", ylab = "United Airline's Quarterly Operating Revenue (in 000s)", ylim = c(1475, 12000), bty = "l",
     xaxt = "n", xlim = c(2000, 2023), lwd = 2,
     main = "Auto ARIMA Model for Entire Dataset") 
axis(1, at = seq(2000, 2023, 1), labels = format(seq(2000, 2023, 1)))
lines(auto.arima$fitted, col = "blue", lwd = 2)
lines(auto.arima.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(2000,3000, legend = c("United Airline's Revenue Series", 
                             "Auto ARIMA Forecast", 
                             "Auto ARIMA Forecast for 4 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.

lines(c(2021.25, 2021.25), c(0, 12002))
text(2010, 12000, "Training - Entire Dataset")
text(2022.5, 12000, "Future")
arrows(2021 , 11500, 2000, 11500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.5, 11500, 2021.5, 11500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

##------------------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------------------

