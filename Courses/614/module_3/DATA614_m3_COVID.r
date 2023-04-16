# COVID.r
# Examine Texas COVID-19 cases first half of 2020

# Load the COVID data -----------------------------------------------------

load("COVID.RData")

# Moving Averages ----------------------------------------------------------

library(zoo) # moving averages

cases <- COVID.df$ConfirmedCases
cases_03da = zoo::rollmean(COVID.df$ConfirmedCases, k = 3, fill = NA)
cases_21da = zoo::rollmean(COVID.df$ConfirmedCases, k = 21, fill = NA)

### Fig 0.1
par(mfrow=c(1,1), mar=c(5, 5, 0.5, 0.5) + 0.1)
plot(c(121,140), range(cases), type = "n",  # "n" = no plotting
     xlab="Day", ylab="Confirmed Cases", xaxt="n",
     main = "Moving Averages")    # "n" = no x axis ticks
lines(seq(121,140), cases[121:140], lty=1)
points(seq(121,140), cases[121:140], pch=16)
lines(seq(121,140), cases_03da[121:140], lty=2)
points(seq(121,140), cases_03da[121:140], pch=1)
lines(seq(121,140), cases_21da[121:140], lty=3)
points(seq(121,140), cases_21da[121:140], pch=0)
legend(x="topleft",c("true", "MA 3", "MA 21"), lty=c(1,2,3), pch=c(16,1,0),
       y.intersp = 1)
x.pos <- c(122,130,138)
x.label <- c("2020-05-23","2020-05-31","2020-06-08")
axis(1,x.pos, x.label)

# Exponential Smoothing ----------------------------------------------------

library(smooth)
library(greybox)
# holdout 10 cases at the end
es1 <-es(COVID.df$ConfirmedCases, h=10, holdout=TRUE, silent=FALSE) # 18? why
AIC(es1)  # AIC = ~1978
fes1 <- forecast(es1)

### Fig 0.2
par(mfrow=c(1,1), mar=c(5, 4, 4, 2) + 0.1)  # default

plot(fes1, main="Exponential Smoothing")


# ARIMA models ------------------------------------------------------------

COVID <- COVID.df$ConfirmedCases    # target column
COVID.ts <- ts(COVID,frequency=7)

# plot raw data
### Fig 2
par(mfrow=c(3,1), mar=c(2, 5, 2, 1) + 0.1)

plot(COVID, type="l", xaxt="n", xlab = "Day", ylab = "#Confirmed Cases",
     main = "Covid Cases, Texas")
x.pos <- c(10, 70, 141)
x.label <- c("2020-02-01","2020-04-01", "2020-06-01")
axis(1,x.pos,x.label)
# autocovariance or autocorrelation function
acf(COVID, xlab="Date", ylab="ACF", main="")  
acf(COVID, type="partial", xlab="Date", ylab="Partial ACF", main="")

# partition into train and test
COVID.train <- ts(COVID[1:130],  frequency=7)
COVID.test <- ts(COVID[131:140],  frequency=7)

# function stl() decomposes the training period into a trend,
# a seasonal component, and remainder series with a period of 7 days
COVID.stl <- stl(COVID.train,"periodic")                                         
COVID.sea <- COVID.stl$time[,1]
COVID.desea <- COVID.train - COVID.sea
COVID.desea.series <- as.numeric(COVID.desea)

### Figure 5.1    
par(mfrow=c(3,1), mar=c(2, 5, 2, 1) + 0.1)  
plot(COVID.desea.series, type="l", xlab="Day", main = "Deseaonalized")  
axis(1,x.pos,x.label)
acf(COVID.desea.series, 25, xlab="Lag", ylab="ACF", main="")
acf(COVID.desea.series, 25, type="partial", xlab="Lag", 
    ylab="Partial ACF", main="")

# ADF test indicates that there is NOT a unit root                                 
adf.test(COVID.desea)  # ACF p-value = 0.2618.
                       # The null hypothesis that the series has a unit root
                       # should be rejected

# WE WILL CONTINUE TO DEVELOP AN ARIMA MODEL FOR ILLUSTRATION

### Select ARIMA models via AICs
COVID.desea.aic <- matrix(0,4,4)
for (i in 0:3) for (j in 0:3) {
  fit.arima <- arima(COVID.desea, order=c(i,1,j))
  print(fit.arima$aic)
  COVID.desea.aic[i+1,j+1] <-  fit.arima$aic
}
COVID.desea.aic
min(COVID.desea.aic)  # 1934.601
# find coordinates of minimum aic
xy <- as.vector(which(COVID.desea.aic==min(COVID.desea.aic), arr.ind=T))
xy
aic.row <- xy[1]-1    # since origin of matrix is (0,0)
aic.column <- xy[2]-1


fit.arima.best <- arima(COVID.desea.series, order= c(aic.row,1,aic.column))
fit.arima.best

### Figure 5.3
par(mfrow=c(1,1), mar=c(1, 2, 1, 2) + 0.1)
tsdiag(fit.arima.best) # standardized residuals are heteroskedastic 
Box.test(fit.arima.best$residuals)  # p-value = 0.7297 >> 0.05
                                    # indicating that the fitted model
                                    # is appropriate

# Prediction using best ARIMA model ---------------------------------------

fit.arima.best <- arima(COVID.desea, order=c(aic.row,1,aic.column))
COVID.pred <-  predict(fit.arima.best, n.ahead=10)

COVID.pred.summary <-  cbind(COVID.test[1:10], COVID.pred$pred+COVID.sea[1:10],
                             COVID.pred$pred-1.96*COVID.pred$se+COVID.sea[1:10],
                             COVID.pred$pred+1.96*COVID.pred$se+COVID.sea[1:10])
colnames(COVID.pred.summary) <- c("true", "predict", "ci.lower", "ci.upper")

### Fig 5.4
par(mfrow=c(1,1), mar=c(5, 5, 0.5, 0.5) + 0.1)                                  
plot(c(1,10), range(COVID.pred.summary), type = "n",  # "n" = no plotting           
     xlab="Day", ylab="Confirmed Cases", xaxt="n")    # "n" = no x axis ticks                          
lines(seq(1,10), COVID.pred.summary[,1], lty=1)
points(seq(1,10), COVID.pred.summary[,1], pch=16)
lines(seq(1,10), COVID.pred.summary[,2], lty=2)
points(seq(1,10), COVID.pred.summary[,2], pch=1)
lines(seq(1,10), COVID.pred.summary[,3], lty=3)
points(seq(1,10), COVID.pred.summary[,3], pch=0)
lines(seq(1,10), COVID.pred.summary[,4], lty=3)
points(seq(1,10), COVID.pred.summary[,4], pch=0)
legend(x="topleft",c("true", "ARIMA predicted", "ARIMA pred. int.",
                      "ARIMA pred.int."), lty=c(1,2,3,3), pch=c(16,1,0,0),
                      y.intersp = 1)
x.pos <- c(2,4,6,8,10)
x.label <- c("2020-06-02","2020-06-04","2020-06-06","2020-06-08","2020-06-10")
axis(1,x.pos, x.label)




