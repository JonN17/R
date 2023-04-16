getwd()
#install.packages("rstudioapi")
library(rstudioapi)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Aquire the data

library(readxl)
data <- read_excel("data/Monthly_Sales.xlsx")
View(data)
freq=12

sales.ts<-ts(data[,3],frequency=freq,start=c(2015,1))
sales.de<-decompose(sales.ts,type="additive")
plot(sales.de)

log.ts<-ts(log(data$Sales_Quantity),frequency=freq,start=c(2015,1))
log.de<-decompose(log.ts,type="additive")
plot(log.de)