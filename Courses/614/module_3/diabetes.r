# Diabetes Trend Analysis in R
# Analysis using MA with equivelant metric weights
# and by spencers 15 point MA

print(utils::getSrcFilename(function(){}, full.names = TRUE))

getwd()
#install.packages("rstudioapi")
library(rstudioapi)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Aquire the data

library(readxl)
D.df <- read_excel("data/data-65_all.xlsx")
colnames(D.df) <- c("date","time","pre_breakfast_glucose")
head(D.df)
View(D.df)

# Define Functions
smooth.sym <- function(my.ts, window.q){
  window.size <- 2*window.q + 1
  my.ts.sm <- rep(0, length(my.ts)-window.size)
  for (i in 1:length(my.ts.sm)){
    my.ts.sm[i] <-mean(my.ts[i:(i+window.size-1)])
  }
  my.ts.sm
}

smooth.spencer <- function(my.ts){
  weight <- c(-3,-6,-5,3,21,46,67,74,67,46,21,3,-5,-6,-3)/320
  my.ts.sm <- rep(0, length(my.ts)-15)
  for (i in 1:length(my.ts.sm)){
    my.ts.sm[i] <-sum(my.ts[i:(i+14)]*weight)
  }
  my.ts.sm
}

gluc<-D.df$pre_breakfast_glucose
library(tseries)
GLUC.sm<-smooth.sym(gluc,7)
GLUC.spencer<-smooth.spencer(gluc)
x.pos<-c(3,68,134)
x.dates<-c("1989-04-20","1989-06-24","1989-08-29")
par(mfrow=c(3,1),mar=c(4,4,4,4))
head(gluc)
gluc
plot(gluc, type="l",xlab="Date",xaxt="n",main="pre breakfast glucose")
axis(1,x.pos,x.dates)
plot(c(1,length(gluc)),c(0,max(gluc)),type="n",xlab="Date",
   main="MA with equal and symmetric weights",
   ylab="Index", xaxt="n")
lines(seq(8,length(gluc)-8),GLUC.sm)
axis(1,x.pos,x.dates)
plot(c(1,length(gluc)),c(0,max(gluc)),type="n",xlab="Date",
     main="Spencer 15 point MA",
     ylab="Index", xaxt="n")
lines(seq(8,length(gluc)-8),GLUC.spencer)
axis(1,x.pos,x.dates)



# Auto correlation function

par(mfrow=c(2,1), mar=c(3,4,3,4))
plot(gluc,type="l",xlab="",ylab="",main="pre-breakfast glucose")
title(xlab="Date",line=1,cex.lab=1.2)
acf(gluc,ylab="",main="")
title(ylab="ACF",line=2)
