
##########################################
# section 3.1 Introduction to R
##########################################

# import a csv file of the total annual sales for each customer
sales <- read.csv("c:/data/yearly_sales.csv")

# examine the imported dataset
head(sales)
summary(sales)

# plot num_of_orders vs. sales
plot(sales$num_of_orders,sales$sales_total,
     main="Number of Orders vs. Sales")

# perform a statistical analysis (fit a linear regression model)
results <- lm(sales$sales_total ~ sales$num_of_orders)
results
summary(results)

# perform some diagnostics on the fitted model
# plot histogram of the residuals
hist(results$residuals, breaks = 800)

##########################################
# section 3.1.2 Data Import and Export
##########################################

# Full Path
sales <- read.csv("c:/data/yearly_sales.csv")

# No need for full path
setwd("c:/data/")
sales <- read.csv("yearly_sales.csv")

# Different functions for importing
sales_table <- read.table("yearly_sales.csv", header=TRUE, sep=",")
sales_delim <- read.delim("yearly_sales.csv", sep=",")

# add a column for the average sales per order
sales$per_order <- sales$sales_total/sales$num_of_orders

# export data as tab delimited without the row names
# This will contain an extra row called "per_order" which is
# per_order = sales_total / num_of_orders
write.table(sales,"sales_modified.txt", sep="\t", row.names=FALSE)

############################################################
# the following code is for illustrative purposes only
# a functioning SQL database is required
install.packages("RODBC")
library(RODBC)
# This doesn't work, see above "illustrative purposes only"
conn <- odbcConnect("training2", uid="user", pwd="password")
housing_data <- sqlQuery(conn, "select serialno, state, persons, rooms
                                from housing
                                where hinc > 1000000")
head(housing_data)
# end of SQL-related R code
############################################################

# export a histogram to a jpeg
jpeg(file="c:/data/sales_hist.jpeg") # create a new jpeg file
hist(sales$num_of_orders) # export histogram to jpeg
dev.off() # shut off the graphic device


##########################################
# section 3.1.3 Attribute and Data Types
##########################################

# Numeric, Character, and Logical Data Types
i <- 1                      # create a numeric variable
sport <- "football"         # create a character variable
flag <- TRUE                # create a logical variable

class(i)                    # returns "numeric"
typeof(i)                   # returns "double"
class(sport)                # returns "character"
typeof(sport)               # returns "character"
class(flag)                 # returns "logical"
typeof(flag)                # returns "logical"

is.integer(i)               # returns FALSE
j <- as.integer(i)          # coerces contents of i into an integer
is.integer(j)               # returns TRUE

length(i)                   # returns 1
length(flag)                # returns 1
# this can be counter intuitive
length(sport)               # returns 1 (not 8 for "football")


# Vectors
is.vector(i)                # returns TRUE
is.vector(flag)             # returns TRUE
is.vector(sport)            # returns TRUE

u <- c("red", "yellow", "blue") # create a vector "red" "yellow" "blue"
u                               # returns "red" "yellow" "blue"
u[1]                            # returns "red" (1st element in u)

# Very efficient way of reducing work
v <- 1:5                        # create a vector 1 2 3 4 5
v                               # returns 1 2 3 4 5
sum(v)                          # returns 15
# Very efficient way of reducing work
w <- v * 2                      # create a vector 2 4 6 8 10
w                               # returns 2 4 6 8 10
w[3]                            # returns 6 (the 3rd element of w)
z <- v + w                      # sums two vectors element by element
z                               # returns 3 6 9 12 15
z > 8                           # returns FALSE FALSE TRUE TRUE TRUE

# This is fun
z[z > 8]                        # returns 9 12 15
z[z > 8 | z < 5]                # returns 3 9 12 15 ("|" denotes "or")

# logical vector is set to false by default
a <- vector(length=3)           # create a logical vector of length 3
a                               # returns FALSE FALSE FALSE
b <- vector(mode="numeric", 3)  # create a numeric vector of length 3
typeof(a)                       # returns "logical"
typeof(b)                       # returns "double"
b[2] <- 3.1                     # assign 3.1 to the 2nd element
b                               # returns 0.0 3.1 0.0
c <- vector(mode="integer", 0)  # create an integer vector of length 0
c                               # returns integer(0)
length(c)                       # returns 0
length(b)                       # returns 3
dim(b)                          # returns NULL (an undefined value)

# Arrays and Matrices

# the dimensions are 3 regions, 4 quarters, and 2 years
# height, width, array
quarterly_sales <- array(0, dim=c(3,4,2))
quarterly_sales[2,1,1] <- 158000
quarterly_sales

# 0 is what it's filled with by default.
sales_matrix <- matrix(0, nrow = 3, ncol = 4)
sales_matrix

install.packages("matrixcalc")                      # install, if necessary
library(matrixcalc)
# This is created vertically
M <- matrix(c(1,3,3,5,0,4,3,3,3),nrow = 3,ncol = 3) # build a 3x3 matrix
# Use ?'%*%' to get documentation
M %*% matrix.inverse(M)                             # multiply M by inverse(M)

# My addition
M1 <- matrix(c(1,4,7,2,5,8,3,6,9),nrow = 3,ncol = 3)
M2 <- matrix(c(10,13,16,11,14,17,12,15,18),nrow = 3,ncol = 3)
M1 %*% M2
# Matches https://www.youtube.com/watch?v=Spr2F6jEuaw

# Data Frames

#import a CSV file of the total annual sales for each customer
sales <- read.csv("c:/data/yearly_sales.csv")
is.data.frame(sales)              # returns TRUE

length(sales$num_of_orders)       # returns 10000 (number of customers)
is.vector(sales$cust_id)          # returns TRUE
is.vector(sales$sales_total)      # returns TRUE
is.vector(sales$num_of_orders)    # returns TRUE
is.vector(sales$gender)           # returns FALSE
is.factor(sales$gender)           # returns TRUE
?is.vector()                      # returns documentation  
# !?!?!?!?!?
# Deep dive on vectors https://www.youtube.com/watch?v=HqC_JfJWMXQ

library(dplyr)
glimpse(sales)

str(sales) # display structure of the data frame object

# extract the fourth column of the sales data frame
sales[,4]
# extract the gender column of the sales data frame
sales$gender
# retrieve the first two rows of the data frame
sales[1:2,]
# retrieve the first, third, and fourth columns
sales[,c(1,3,4)]
# retrieve both the cust_id and the sales_total columns
sales[,c("cust_id", "sales_total")]
# retrieve all the records whose gender is female
sales[sales$gender=="F",]


# class() is "data.frame" but typeof() is "list"?!?!??!?!
class(sales)
typeof(sales)
head(sales)
# Lists

# build an assorted list of a string, a numeric, a list, a vector,
# and a matrix
# v and M were defined above.
housing <- list("own", "rent")
assortment <- list("football", 7.5, housing, v, M)
assortment

# examine the fifth object, M, in the list
class(assortment[5])               # returns "list"
length(assortment[5])              # returns 1
# ????????????Why do you need the double brackets ???????????????
class(assortment[[5]])             # returns "matrix"
length(assortment[[5]])            # returns 9 (for the 3x3 matrix)

str(assortment)

# Factors

class(sales$gender)       # returns "factor"
is.ordered(sales$gender)  # returns FALSE

# First 6 entries only!!
head(sales$gender)        # display first six values and the levels

# My noodling
temp<-head(sales$gender)
temp <- c("F", "F", "F", "F", "F", "M") # c() function returns a vector (a one dimensional array)
is.ordered(temp) # returns FALSE
temp <- c("M", "F", "F", "F", "F", "F")
is.ordered(temp) # returns FALSE
is.vector(temp) # returns true

temp <- c("F", "F", "F", "F", "F", "M")
order(temp) # returns [1] 1 2 3 4 5 6
is.ordered(temp) # returns FALSE

install.packages("ggplot2")  # install ggplot, if necessary
library(ggplot2)
# type data() to see list of all datasets.
data(diamonds)            # load the data frame into the R workspace
str(diamonds)

head(diamonds$cut)        # display first six values and the levels

# build an empty character vector of the same length as sales
sales_group <- vector(mode="character",
                      length=length(sales$sales_total))

# group the customers according to the sales amount
sales_group[sales$sales_total<100] <- "small"
sales_group[sales$sales_total>=100 & sales$sales_total<500] <- "medium"
sales_group[sales$sales_total>=500] <- "big"

# create and add the ordered factor to the sales data frame
spender <- factor(sales_group,levels=c("small", "medium", "big"),
                  ordered = TRUE)
head(sales)
sales <- cbind(sales,spender)
head(sales)

str(sales$spender)

head(sales$spender)

# Contingency Tables

# build a contingency table based on the gender and spender factors
sales_table <- table(sales$gender,sales$spender)
sales_table

class(sales_table)            # returns "table"
typeof(sales_table)           # returns "integer"
dim(sales_table)              # returns 2 3 or dimensions of object (2 high, 3 wide)

# performs a chi-squared test (report Chisq , df or degrees of freedom and p-value)
summary(sales_table)

##########################################
# section 3.1.4 Descriptive Statistics
##########################################

summary(sales) # minimum, quartiles, median, mean, maximum

# to simplify the function calls, assign
x <- sales$sales_total
y <- sales$num_of_orders

cor(x,y)         # returns 0.7508015 (correlation)
cov(x,y)         # returns 345.2111 (covariance)
IQR(x)           # returns 215.21 (interquartile range)
mean(x)          # returns 249.4557 (mean)
median(x)        # returns 151.65 (median)
range(x)         # returns 30.02 7606.09 (min max)
sd(x)            # returns 319.0508 (std. dev.)
var(x)           # returns 101793.4 (variance)

# Reports stadard deviation over columns 1,2 and 3
apply(sales[,c(1:3)], MARGIN=2, FUN=sd)
# ????????????????????? need clarity on margin 1,2 and 3
sd(sales[,c(1)])
sd(sales[,c(2)])
sd(sales[,c(3)])

# build a function to provide the difference between
# the maximum and the minimum values
my_range <- function(v) {range(v)[2] - range(v)[1]}
my_range(x)


head(sales[,c(1)])                  # returns [1] 100001 100002 100003 100004 100005 100006
my_range(head(sales[,c(1)]))        # returns 5

head(sales[,c(2)])                  # returns [1] 800.64 217.53  74.58 498.60 723.11  69.43
my_range(head(sales[,c(2)]))        # returns 731.21
