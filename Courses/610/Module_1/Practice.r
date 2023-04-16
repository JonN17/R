s
# DATA610 – Introduction to Data Mining
# Practice Homework #1 – Review of R
# Instructions:
# Use the SalaryData.txt file available on Canvas and complete the following steps. The dataset contains
# work related data about 1,000 individuals. The variables are: age, workClass, educationLevel,
# educationNum, maritalStatus, occupation, race, sex, hoursPerWeek, nativeCountry, and salary.
# Write R commands for the following. Use the Rmarkdown environment to produce a Word file
# containing the output of your work. Include your analyses and interpretations in the Rmarkdown
# right below the output using R comments (#):
# 1. Read the dataset.
data<-read.table("C:/Users/jnowa/OneDrive/Pulpit/SalaryData.txt", header = TRUE,sep = "\t")
# 2. Identify and define factor variables.
https://stackoverflow.com/questions/17907944/how-to-select-all-factor-variables-in-r


# 3. Provide a full description of the dataset you have (number of rows, columns, types of variables,
# etc).

str(data)

'data.frame':	1000 obs. of  16 variables:
 $ age          : int  20 21 22 30 19 45 31 51 29 57 ...
 $ workclass    : chr  "Private" "Private" "Private" "Private" ...
 $ fntwgt       : int  528616 253612 347530 137076 533147 117556 34862 172281 183111 108426 ...
 $ education    : chr  "5th-6th" "Some-college" "HS-grad" "HS-grad" ...
 $ educationNum : int  3 10 9 9 9 10 12 13 11 9 ...
 $ maritalStatus: chr  "Never-married" "Never-married" "Separated" "Married-civ-spouse" ...
 $ occupation   : chr  "Other-service" "Prof-specialty" "Other-service" "Transport-moving" ...
 $ relationship : chr  "Other-relative" "Own-child" "Unmarried" "Husband" ...
 $ race         : chr  "White" "White" "Black" "White" ...
 $ sex          : chr  "Male" "Female" "Female" "Male" ...
 $ capitalGain  : int  0 1055 0 0 0 0 0 0 0 0 ...
 $ capitalLoss  : int  0 0 0 0 0 0 0 0 0 0 ...
 $ hoursPerWeek : int  40 32 35 40 30 32 40 40 40 48 ...
 $ nativeCountry: chr  "Mexico" "United-States" "United-States" "United-States" ...
 $ salary       : chr  "<=50K" "<=50K" "<=50K" ">50K" ...
 $ salres       : int  0 0 0 1 0 0 0 0 0 0 ...


> glimpse(data)
Rows: 1,000
Columns: 16
$ age           <int> 20, 21, 22, 30, 19, 45, 31, 51, 29, 57, 49, 49, 26, 41…
$ workclass     <chr> "Private", "Private", "Private", "Private", "Private",…
$ fntwgt        <int> 528616, 253612, 347530, 137076, 533147, 117556, 34862,…
$ education     <chr> "5th-6th", "Some-college", "HS-grad", "HS-grad", "HS-g…"
$ educationNum  <int> 3, 10, 9, 9, 9, 10, 12, 13, 11, 9, 10, 11, 10, 10, 4, …
$ maritalStatus <chr> "Never-married", "Never-married", "Separated", "Marrie…"
$ occupation    <chr> "Other-service", "Prof-specialty", "Other-service", "T…"
$ relationship  <chr> "Other-relative", "Own-child", "Unmarried", "Husband",…
$ race          <chr> "White", "White", "Black", "White", "White", "Black", …
$ sex           <chr> "Male", "Female", "Female", "Male", "Male", "Female", …
$ capitalGain   <int> 0, 1055, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
$ capitalLoss   <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
$ hoursPerWeek  <int> 40, 32, 35, 40, 30, 32, 40, 40, 40, 48, 40, 35, 42, 56…
$ nativeCountry <chr> "Mexico", "United-States", "United-States", "United-St…"
$ salary        <chr> "<=50K", "<=50K", "<=50K", ">50K", "<=50K", "<=50K", "…"
$ salres        <int> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, …

# 4. For each quantitative variable generate the following and interpret your observations:
# a. Numerical descriptive statistics: mean, standard deviation, etc.
mean(data$age)
sd(data$age)
summary(data)
# b. Graphical descriptive statistics: histogram, boxplot, normal probability plot, etc.

# histogram

# boxplot


# normal probability plot

# 5. For each qualitative variable generate the following and state and interpret your observations:
# a. Frequency tables and relative frequency tables

# Frequency tables 

# relative frequency tables


# b. Graphical descriptive statistics: barchart, pie chart

# barchart

# pie chart


# 6. Using 2 pairs of meaningful qualitative variables generate the following and interpret them:
# a. Contingency tables

# b. Graphical descriptive statistics

# 7. Using 2 combinations of qualitative and quantitative pairs (for example one pair could be age and
# race), generate the following and interpret them:
# a. Contingency tables


# b. Graphical descriptive statistics
# 8. Pick two races and compare the mean age using appropriate hypothesis test.


# 9. Based on the educationLevel variable, compare hoursPerWeek using appropriate hypothesis test.


# 10. Check outliers in the hoursPerWeek variable. Identify the records that belong to those outliers.
# Remove those records from the dataset and save the dataset into a new file.


