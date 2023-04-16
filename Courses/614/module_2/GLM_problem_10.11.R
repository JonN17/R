# GLM_problem_10.11.R
# Cervical cancer in women of different ages in 4 European countries

# Key Questions
# What is an offset, and how is it affected in model building?
# Answer: A priori term.
# 
# What role does the function rstandard() play in plotting the residuals 
# of a fitted model?

# My ANSWER: Helps you find the residuals.   XXXXXXXXXXXXXXXXXXX
# My ANSWER:


library(MASS) # Calculations
library(GLMsData)
data("cervical")
cervical$AgeNum <- rep(c(25,35,45,55),4)
# the lower value of age ranges, for 4 countries
par(mfrow=c(1,1))
   ### Part 1
# Wyears = women years
with(cervical, {
  plot(Deaths/Wyears~AgeNum, type="n")
  lines(Deaths/Wyears~AgeNum, lty=1,
        subset=(Country==unique(Country)[1]) )
  lines(Deaths/Wyears~AgeNum, lty=2,
        subset=(Country==unique(Country)[2]) )
  lines(Deaths/Wyears~AgeNum, lty=3,
        subset=(Country==unique(Country)[3]) )
  lines(Deaths/Wyears~AgeNum, lty=4,
        subset=(Country==unique(Country)[4]) )
  legend("topleft", lty=1:4, legend=unique(cervical$Country) )
})
   ### Part 2
# An offset is a term which is known a priori. Offsets are frequently seen
# in Poisson GLMs. Here the number of cancer cases depends on the population
# of the country. Assuming a logarithmic link function, the offset 
# is offset log(Wyears). An offset is useful when fitting a GLM to the data
# to reduce the exposure to whatever causes cervical cancer.

# Question: What is exposure?
# Answer: XXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Question: Youtube video for offset?
# https://www.youtube.com/watch?v=3Jbaxp3whDA
# converts rate to count 
# More detail
# https://www.youtube.com/watch?v=EyKba_sOp0k
# 
# Question offset()

   ### Part 3
# Apply GLM with age and country as explanatory variables.
# produce a plot of residuals against the values and evaluate the model.
cc.m0 <- glm(Deaths~offset(log(Wyears)) + Age + Country,
             data=cervical, family = poisson)
plot(rstandard(cc.m0) ~ fitted(cc.m0), main="Poissn glm") 

# This has a residual of -30 which means it doesn't fit well.

# Question - what does rstandard() do?
# Answer: Regression Deletion Diagnostics XXXXXXXXXXXXXXXXXXXx
# Question - what does fitted() do?
# Answer: Extract Model Fitted Values XXXXXXXXXXXXXXXXXXXx
# Fun video: https://www.youtube.com/watch?v=luvliCq6QuQ
# Difference between fitted and predicted
# https://stackoverflow.com/questions/12201439/is-there-a-difference-between-the-r-functions-fitted-and-predict
# fitted() returns the y-hat values associated with the data used to fit the model
# predict() returns predictions for a new set of predictor variables.
# predict returns the fitted values before the inverse of the link function is applied (to return the data to the same scale as the response variable), 
# fitted shows it after it is applied.

   ### Part 4
cc.m0Q <- glm(Deaths~offset(log(Wyears)) + Age + Country,
             data=cervical, family = quasipoisson)
plot(rstandard(cc.m0Q) ~ fitted(cc.m0Q), main="Quasi-Poisson glm") 
   ### Part 5
cc.m0NB <- glm(Deaths~offset(log(Wyears)) + Age + Country,
              data=cervical)
cc.m0NB <- glm.convert(cc.m0NB)
plot(rstandard(cc.m0NB) ~ fitted(cc.m0NB), main="Neg. bi. glm")


# Fit the scales
plot(rstandard(cc.m0) ~ fitted(cc.m0), main="Poissn glm", ylim=c(-30,30))
abline(h=0, col="blue")
plot(rstandard(cc.m0Q) ~ fitted(cc.m0Q), main="Quasi-Poisson glm", ylim=c(-30,30)) 
abline(h=0, col="blue")
plot(rstandard(cc.m0NB) ~ fitted(cc.m0NB), main="Neg. bi. glm", ylim=c(-30,30))
abline(h=0, col="blue")


# Question:  Why Negative binomial glm over Quasi-Poisson glm
# Answers: 

   ### Part 6
# All models seem to have a large negative outlier, but clearly the 
# Poisson model does not accommodate the variation correctly
summary(cc.m0)
