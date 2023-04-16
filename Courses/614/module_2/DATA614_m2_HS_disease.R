# DATA614_m2_HS_disease.R

# The data set consists of counts of high school students diagnosed with an
# infectious disease within a period of days from an initial outbreak

# Prep data ---------------------------------------------------------------

cases <-  
  structure(list(Days = c(1L, 2L, 3L, 3L, 4L, 4L, 4L, 6L, 7L, 8L, 
                          8L, 8L, 8L, 12L, 14L, 15L, 17L, 17L, 17L, 18L, 19L, 19L, 20L, 
                          23L, 23L, 23L, 24L, 24L, 25L, 26L, 27L, 28L, 29L, 34L, 36L, 36L, 
                          42L, 42L, 43L, 43L, 44L, 44L, 44L, 44L, 45L, 46L, 48L, 48L, 49L, 
                          49L, 53L, 53L, 53L, 54L, 55L, 56L, 56L, 58L, 60L, 63L, 65L, 67L, 
                          67L, 68L, 71L, 71L, 72L, 72L, 72L, 73L, 74L, 74L, 74L, 75L, 75L, 
                          80L, 81L, 81L, 81L, 81L, 88L, 88L, 90L, 93L, 93L, 94L, 95L, 95L, 
                          95L, 96L, 96L, 97L, 98L, 100L, 101L, 102L, 103L, 104L, 105L, 
                          106L, 107L, 108L, 109L, 110L, 111L, 112L, 113L, 114L, 115L), 
                 Students = c(6L, 8L, 12L, 9L, 3L, 3L, 11L, 5L, 7L, 3L, 8L, 
                              4L, 6L, 8L, 3L, 6L, 3L, 2L, 2L, 6L, 3L, 7L, 7L, 2L, 2L, 8L, 
                              3L, 6L, 5L, 7L, 6L, 4L, 4L, 3L, 3L, 5L, 3L, 3L, 3L, 5L, 3L, 
                              5L, 6L, 3L, 3L, 3L, 3L, 2L, 3L, 1L, 3L, 3L, 5L, 4L, 4L, 3L, 
                              5L, 4L, 3L, 5L, 3L, 4L, 2L, 3L, 3L, 1L, 3L, 2L, 5L, 4L, 3L, 
                              0L, 3L, 3L, 4L, 0L, 3L, 3L, 4L, 0L, 2L, 2L, 1L, 1L, 2L, 0L, 
                              2L, 1L, 1L, 0L, 0L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 0L, 0L, 
                              0L, 1L, 1L, 0L, 0L, 0L, 0L, 0L)), .Names = c("Days", "Students"
                              ), class = "data.frame", row.names = c(NA, -109L)) 

head(cases)
#   Days Students
# 1    1        6
# 2    2        8
# 3    3       12
# 4    3        9
# 5    4        3
# 6    4        3

# Question what family does the data suggest
# - Poison, Binomial, Negative Binomial, Quasi Poisson
# Answer: Poison, since we are counting the number of cases per day
# Suggestion: Explain why it's not binomial, negative binomial and Quasi poisson


# 


# Plot data ---------------------------------------------------------------

plot(cases$Days, cases$Students, xlab = "DAYS SINCE OUTBREAK",
     ylab = "# STUDENTS INFECTED", pch = 16)
# As days increase, the number of infected students decreases

# Build Poisson model -----------------------------------------------------

# The natural log is the default link function for the Poisson error
# distribution.
# It works well for count data as it forces all of the predicted values
# to be positive.
# XXXXXXXXXXXX forcing to be positive how?
model1 <- glm(Students ~ Days, family=poisson, data = cases)

summary(model1)
# Deviance Residuals: 
#      Min        1Q    Median        3Q       Max  
# -2.00482  -0.85719  -0.09331   0.63969   1.73696  
# 
# Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  1.990235   0.083935   23.71   <2e-16 ***
# Days        -0.017463   0.001727  -10.11   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance:     215.36  on 108  degrees of freedom
# Residual deviance: 101.17  on 107  degrees of freedom
# AIC: 393.11
# 
# Number of Fisher Scoring iterations: 5


# Question: How do we know days is highly significant?
# Answer: it has ***, p value is near zero, 2e-16

# Question: is dispersion assumed or calculated for quasi-poisson regression
# Answer: For poisson it is assumed to be 1, it is calculated for quasi poisson regression.
# Clarification: Why?  XXXXXXXXXXXXXXXXXXXXXXXXXXX

# Question: What is the null deviance?   
# Answer: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Question: AIC isn't that bad... why not?  What's a good / bad AIC?
# Answer: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX



# Model interpretation ----------------------------------------------------

# The negative coefficient for Days indicates that as days increase,
# the mean number of students with the disease is smaller.
# (We noticed this in the data plot)

# This coefficient is highly significant (p < 2e-16).

# Is there overdispersion? ------------------------------------------------

# Question: Is there over dispersion?
# Answer: No, the residual deviance (101.17)
# is not greater than the degrees of freedom.
# Residual deviance: 101.17  on 107  degrees of freedom

# Question: What is over dispersion?
# Answer: More variability than expected
# Answer: Variance is > mean

# Question: How to check for dispersion parameters?
# dispersion parameter ~= residual deviance / degrees of freedom
# Video https://www.youtube.com/watch?v=0W5QF_OnR7w
# 

# Question: Cause of over dispersion?
# Answer: Model is wrong
# Answer: Model is missing an interaction term. (lack of fit... goodness of fit test)
# Answer: non-linearity
# Answer: missing an important X variable
#
# 2nd most common issue 
# Answer: Excessive zeros
#
# 3rd
# Answer: It just is... doesn't fit poisson

# Class material: https://www.youtube.com/watch?v=0W5QF_OnR7w
# THis is excellent


# ?????????????? XXXXXXXXXXXX how does this relate to the video
# Residual deviance, 101.17, is not greater than the degrees of freedom, 107,
# so the model does not exhibit overdispersion

# Is the model adequate? --------------------------------------------------
# Question: what's best 
# Pearson chi square
# Wald
# anova =F
# score test
# wald & Score
# ANSWER: pearson chi square .....
# ANSWER: Observed - is the deviance of the model
# ANSWER: Expected - degrees of freedom of the residuals
# ??????? XXXXXXXXXXXXXX must dig deeper.


# Pearson chi square
pchisq(deviance(model1), df.residual(model1), lower.tail = FALSE)
# 0.6405459. Do not reject the null hypothesis that the model is adequate

# NOTE ... I assume 0.05 is what we should go for?