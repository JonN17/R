# MakeCOVID.df.r
# Make a data frame of the Texas COVID-19 cases, first half 2020

library(readxl)
data <- read_excel("~/DATA614/M3/COVID_TX_sorted.xlsx")

# Prepare the data --------------------------------------------------------

summary(data)  # no missing cases data but there are negative TargetValues

for (i in 1:nrow(data)) {   # get rid of negative numbers, set to NA
  if (data[i,9] < 0 ) data[i,9] <- NA
}

### multiple imputation by chained equations (MICE)
# impute values for the new NAs

x=cbind(data$Population, data$TargetValue) # MICE needs at least two columns

# Impute the missing data
mtrx <- as.matrix(x)  # MICE expects a matrix, not a data frame

library(mice)  
# we request only one output data set, m=1
imputed_Data <- mice(mtrx, m=1, maxit = 50, method = 'pmm', seed = 500)

#get complete data (data set 1)
complete1 <- complete(imputed_Data,1)

# Build the data frame for the train data
complete <- cbind(data[,1],complete1[,1], data[,7], complete1[,2])
colnames(complete) <- c("Id", "Population", "Date", "ConfirmedCases")
str(complete)

# Aggregate Confirmed Cases by Date ----------------------------------------
# aggregate the cases from counties with small population with those
# of large population to have a single value by date
COVID.df <- aggregate(complete$ConfirmedCases, list(complete$Date), FUN=sum)
colnames(COVID.df) <- c("Date","ConfirmedCases")


# Save the COVID data frame as an R file in working directory -------------

remove(complete, complete1, data,imputed_Data, mtrx,x)

save(COVID.df, file = "COVID.RData")
