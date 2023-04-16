# milk.R

library(readxl)
data <- read_excel("data/milk_pounds.xlsx")

library(tseries)
milk.ts = ts(data[, -1], frequency = 12, start=c(1962, 1, 1))


# Plot --------------------------------------------------------------------

# Question: what does ts() do?
# Answer: changes vector to time series.  Given frequency & start date.

plot(milk.ts, type="l",main="Monthly milk production in pounds\n
     1962-01 through 1975_12")

# Stationary? -------------------------------------------------------------
# Question: Is this a stationary system?
# Answer: No, it grows over time.

library(roll)
milkmu <- roll_mean(milk.ts,12)

# Question: What does rolling mean do?
# Answer: If it's not parallel to X axis then it's not stationary.

par(mfrow=c(2,1), mar=c(4,4,4,4))
plot(milkmu, type="l", main="Rolling mean of milk production")
### mean is increasing over time => not stationary  


# Question: What does rolling mean of SD do?
# Answer: Shows it's not stationary.


milksd <- roll_sd(milk.ts,12)
plot(milksd, type="l", main="Rolling standard deviation of milk production")  
### sd is not stable. Has different shape as time increases => not stationary




# Partition the series into train and test data sets ----------------------

# milk.all <- ts(data[2], frequency = 12, start=c(1962, 1, 1))
milk.3yrs <- ts(data[133:168,2],frequency = 12, start=c(1973, 1, 1))
milk.train <- ts(data[1:156,2], frequency = 12, start=c(1962, 1, 1))
milk.test <- ts(data[157:168,2],frequency = 12, start=c(1975, 1, 1))

# Decompose the series to remove seasonal effects -------------------------

# Question: What does stl() do?
# Answer: decomposes the series it's seasonalized components so they can 
# be subtracted from original series, then you have deseasonalized data.
# Seasonal 

milk.stl <- stl(milk.train[,1], "periodic")
milk.sea <- milk.stl$time[,1]
milk.desea <- milk.train - milk.sea
milk.desea.series <- as.numeric(milk.desea)


par(mfrow=c(2,1), mar=c(4,4,4,4))
plot(milk.desea.series, type="l", xlab="Month", ylab="Deseasonalized pounds",
     xaxt="n") 
plot(milk.train, type="l", xlab="Month", ylab="train",
     xaxt="n")
plot(milk.sea, type="l", xlab="Month", ylab="seasons",
     xaxt="n")

acf(milk.desea.series,25,xlab="Lag", ylab="ACF", main="")
# QUestion: Why is this always hard to interpret?
# Answer: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# QUestion: why does the 1st one always go to unity?
# Answer: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Question: What is the prediction interval?
# Answer: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# # do same for test data  [use only last three years for stl]
test.stl <- stl(milk.3yrs[,1], "periodic")
test.sea <- test.stl$time[,1]
test.desea <- milk.3yrs[,1] - test.sea
test.desea.series <- as.numeric(test.desea)


# ADF test ----------------------------------------------------------------


adf.test(milk.desea)
#             Augmented Dickey-Fuller Test
# 
# data:  milk.desea
# Dickey-Fuller = -2.0837, Lag order = 5, p-value = 0.5415
# alternative hypothesis: stationary

# The ADF test is a hypothesis test with the null hypothesis being there is
# a unit root (non-stationary) and the alternative being there is not a unit
# root (stationary)
# Accept null hypothesis that series is non-stationary



# Remove increasing mean --------------------------------------------------
# take log of deseasonalized series, to lower the mean.

plot(milk.desea, main="No Log, Higher Mean")

milklog <- log(milk.desea)
plot(milklog, main="Log Means Lower Mean")

# Question: Why do we want a lower mean?
# Answer: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXx

# subtract the rolling mean
milkLogMu <- roll_mean(milklog,12)
plot(milklog, main="Raw Data")
plot(milkLogMu, main="The Rolling Mean")



milkNoMean <- milklog - milkLogMu
# You may need to widen the window to display this.
plot(milkNoMean, main="Subtract the Rolling Mean")

# do similar for test data
testlog <- log(test.desea)
plot(testlog, main="Deseasonalized Test Data")

# subtract the mean
testLogMu <-  mean(testlog)    # roll_mean(testlog,12)
testNoMean <- testlog - testLogMu
plot(testNoMean, main="Test Data: Log - Mean")

# library(tseries)
adf.test(milkNoMean[12:156])

#       Augmented Dickey-Fuller Test
# 
# data:  milkNoMean[12:156]
# Dickey-Fuller = -4.2072, Lag order = 5, p-value = 0.01
# alternative hypothesis: stationary
# 
# Warning message:
#   In adf.test(milkNoMean[12:156]) : p-value smaller than printed p-value


# The p-value 0.01 is less than the threshold of 0.05; hence the null hypothesis  
# that the series has a unit root should not be rejected and an ARIMA(p,1,q)
# might be appropriate for the transformed monthly milk production series


# 7m40s
# Question: What is meant by unit root?
# Answer: XXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Question: Why does this mean non-stationary?
# Answer: XXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Question: Why does ARIMA(p,1,q) mean?  v7m30s
# Answer: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Question: what are ways to convert non-stationary series to 
# a stationary one?
# a) log transform
# b) subtract the rolling mean
# c) apply exponential decay
# d) apply time shifting

# Question: examples of exponential decay and time shifting?
# Answer: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXx Need to get this.

# Question: What does a ADF or augmented dickey fuller test for?  
# Answer: stationary process (stationary if value is low)



# Search for p (#AR terms) and q(#MA terms) -------------------------------
# p & q are parameters of the ARIMA process.

# create an emptry matrix
milkAIC <- matrix(0,4,4)
milkAIC

# fill the matrix with AIC numbers.
for (i in 1:3) for (j in 0:3) {
  fit.arima <- arima(milkNoMean, order = c(i,1,j))                               
  milkAIC[i+1,j+1] <- fit.arima$aic
}
# Print out AIC values
milkAIC


# find coordinates of minimum aic
xy <- as.vector(which(milkAIC==min(milkAIC), arr.ind=T))
# Print out coordinates of the lowest AIC
# Value: -941.4165
# Coordinates: 4 4
xy

# Convert indexes to parameters 
# 4,4 becomes 33
xy[1] # Returns 4
xy[1]-1 # Returns 3
aic.row <- xy[1]-1    # since origin of matrix is (0,0)
aic.column <- xy[2]-1

# fit the arima
fit.arima.best <- arima(milkNoMean, order= c(aic.row,1,aic.column))
fit.arima.best

# Question: Why fit to milkNoMean instead of original trained data
# Answer: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Question: Why do we set c(aic.row,1,aic.column)
# Answer: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXx

# NB, fit.arima.best is performed on transformed train data, milkNoMean
# https://machinelearningmastery.com/arima-for-time-series-forecasting-with-python/


# Prediction --------------------------------------------------------------

milk.pred <- predict(fit.arima.best, n.ahead = 12)
# Question: What does n.ahead mean?
# Answer: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

milk.pred.summary <- cbind(                                                      
                           milk.pred$pred+milk.pred$se,
                           milk.pred$pred-1.96*milk.pred$se,
                           milk.pred$pred+1.96*milk.pred$se,
                           testNoMean[25:36])
# Question: What does cbind() do?
# Answer: XXXXXXXXXXXXXXXX

# Question: Explain The following
# milk.pred$pred+milk.pred$se,
# milk.pred$pred-1.96*milk.pred$se,
# milk.pred$pred+1.96*milk.pred$se,
# testNoMean[25:36])
# Answer: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxx


colnames(milk.pred.summary) <- c("predict","p.i. lower","p.i.upper","true")                




par(mfrow=c(1,1), mar=c(4,4,4,4))

plot(c(1,12), range(milk.pred.summary), type="n", xaxt="n", xlab="",
     ylab = "Transformed milk production")
lines(seq(1,12),milk.pred.summary[,1], lty=2) 
points(seq(1,12),milk.pred.summary[,1], pch=1)
lines(seq(1,12),milk.pred.summary[,2], lty=3) 
points(seq(1,12),milk.pred.summary[,2], pch=0)
lines(seq(1,12),milk.pred.summary[,3], lty=3) 
points(seq(1,12),milk.pred.summary[,3], pch=0)
lines(seq(1,12),milk.pred.summary[,4], lty=1) 
points(seq(1,12),milk.pred.summary[,4], pch=16)
legend("topleft",c("predicted","pred. int.", "pred. int.", "transformed true"),
       pch = c(1,0,0,16), lty = c(2,3,3,1))
x.pos <- c(1,7,12)
x.label <- c("Jan-76","Jul-76","Dec-76")
axis(1, x.pos, x.label)
