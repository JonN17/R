

# If .txt tab file, use this
#my_data <- read.delim(file.choose())
# Or, if .csv file, use this
#my_data <- read.csv(file.choose())

# Store the data in the variable my_data
my_data <- iris

# Print the first 6 rows
head(my_data, 6)

# Compute the mean value
mean(my_data$Sepal.Length)

# Compute the median value
median(my_data$Sepal.Length)

# Compute the mode
# install.packages("modeest") - must be executed via command line
require(modeest)
var <- mfv(my_data$Sepal.Length)
print(paste0("MODE: ", var))

# Compute the minimum value
min(my_data$Sepal.Length)

# Compute the maximum value
max(my_data$Sepal.Length)

# Range
range(my_data$Sepal.Length)

# Interquartile range
# interquartile range (IQR) - corresponding to the difference between the first and third quartiles - 
# is  sometimes used as a robust alternative to the standard deviation
# quantile(x, probs = seq(0, 1, 0.25))

quantile(my_data$Sepal.Length)

# To compute deciles (0.1, 0.2, 0.3, …., 0.9), use this:
quantile(my_data$Sepal.Length, seq(0, 1, 0.1))

# To compute the interquartile range, type this:
IQR(my_data$Sepal.Length)


# Compute the variance
var(my_data$Sepal.Length)
# Compute the standard deviation =
# square root of th variance
sd(my_data$Sepal.Length)

# Compute the median
median(my_data$Sepal.Length)
# Compute the median absolute deviation
mad(my_data$Sepal.Length)

# Summary of a single variable. Five values are returned: the mean, median, 25th and 75th quartiles, min and max in one single line call:
print("summary")
summary(my_data$Sepal.Length)


# Summary of dataframe
summary(my_data, digits = 1)

# Compute the mean of each column
sapply(my_data[, -5], mean)

# Compute quartiles
sapply(my_data[, -5], quantile)

#my_data
# ???? What does -5 or -2 do?
print("---5")
head(my_data[, -5])
print("---2")
head(my_data[, -2])


# install.packages("pastecs")
# Compute descriptive statistics
library(pastecs)
res <- stat.desc(my_data[, -5])
round(res, 2)

# mean() function will return NA if even only one value is missing in a vector. 
# This can be avoided using the argument na.rm = TRUE, which tells to the function 
# to remove any NAs before calculations. An example using the mean function is as follow:
mean(my_data$Sepal.Length, na.rm = TRUE)

# Install github
#if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/ggpubr")

# Or, install from CRAN as follow:
# install.packages("ggpubr")
library(ggpubr)

# Box plots
ggboxplot(my_data, y = "Sepal.Length", width = 0.5)

# Histogram
gghistogram(my_data, x = "Sepal.Length", bins = 9, add = "mean")

# Empirical cumulative distribution function (ECDF)
ggecdf(my_data, x = "Sepal.Length")

# Q-Q plots
ggqqplot(my_data, x = "Sepal.Length")


# Descriptive statistics by groups
# install.packages("dplyr")


# Descriptive statistics by groups
# To compute summary statistics by groups, the functions group_by() and summarise() [in dplyr package] 
# can be used.

# We want to group the data by Species and then:
# compute the number of element in each group. R function: n()
# compute the mean. R function mean()
# and the standard deviation. R function sd()
library(dplyr)
group_by(my_data, Species) %>% 
summarise(
  count = n(), 
  mean = mean(Sepal.Length, na.rm = TRUE),
  sd = sd(Sepal.Length, na.rm = TRUE)
  )


# magrittr is required for %>%
library(magrittr)
# forward-pipe operator
# The man page https://cran.r-project.org/web/packages/magrittr/index.html
# There is flexible support for the type of right-hand side expressions
# To quote Rene Magritte, "Ceci n'est pas un pipe." or "This Is Not a Pipe"
iris %>% head()
# is the same as 
head(iris)

