# TrendAnlysis.r
# Analyze trend using MA with equal and symmetric weights
# and by Spencer's 15-point MA


# Load the COVID data -----------------------------------------------------

load("COVID.RData")


# Define two functions ----------------------------------------------------

smooth.sym <- function(my.ts, window.q){
  window.size <- 2*window.q + 1
  my.ts.sm <- rep(0, length(my.ts)-window.size)
  for (i in 1:length(my.ts.sm)){
    my.ts.sm[i] <- mean(my.ts[i:(i+window.size-1)])
  }
  my.ts.sm
}  # moving average with equal and symmetric weights

smooth.spencer <- function(my.ts){
  weight <- c(-3,-6,-5,3,21,46,67,74,67,46,21,3,-5,-6,-3)/320
  my.ts.sm <- rep(0, length(my.ts)-15)
  for (i in 1:length(my.ts.sm)) {
    my.ts.sm[i] <- sum(my.ts[i:(i+14)]*weight)
  }
  my.ts.sm
}  # Spencer's 15-point moving average

cc <- COVID.df$ConfirmedCases

library(tseries)
COVID.sm <- smooth.sym(cc,7)
COVID.spencer <- smooth.spencer(cc)
x.pos <- c(2,70,138)
x.dates <- c("2020-01-24", "2020-04-01", "2020-06-08")
par(mfrow=c(3,1), mar=c(4,4,4,4))
plot(cc, type="l", xlab="Day", xaxt="n", main="Texas Covid Cases")
axis(1, x.pos, x.dates)
plot(c(1,length(cc)), c(0, max(cc)), type="n", xlab="Day", 
     main="MA with equal and symmetric weights",
     ylab="Index", xaxt="n")
lines(seq(8, length(cc)-8), COVID.sm)
axis(1, x.pos, x.dates)
plot(c(1,length(cc)), c(0,max(cc)), type="n", xlab="Day",
     main="Spencer's 15-point MA",
     ylab="Index", xaxt="n")
lines(seq(8, length(cc)-8), COVID.spencer)
axis(1, x.pos, x.dates)


# Autocorrelation function (ac.f) -----------------------------------------

par(mfrow=c(2,1), mar=c(3,4,3,4))
plot(cc, type="l", xlab="", ylab="",main="Texas Covid Cases")
title(xlab="Day", line=1, cex.lab=1.2)
acf(cc, ylab="", main="")
title( ylab="ACF", line=2)

