# GLM_energy v4.R

library(GLMsData)
data(energy)

# The energy expenditure for 104 females at rest for a 24 hour period
# The total mass of each subject is the sum of fat and fat-free tissue masses
# Use this information to calculate total mass

energy$totmass <- energy$Fat + energy$NonFat # fat plus muscle

plot(energy$totmass,energy$Energy)
plot(log(energy$totmass),energy$Energy) # log plot is slightly more condensed
# I'm not sure I agree this is more condensed
# We are going with log(totmass)
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# ???? This isn't super clear

plot(energy$Fat,energy$Energy)
plot(log(energy$Fat),energy$Energy)

plot(energy$NonFat,energy$Energy)
plot(log(energy$NonFat),energy$Energy)
# Selecting log(nonfat)
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# ??????? it's compressed on X axis but not Y axis.  This isn't super clear.

# Wald Tests --------------------------------------------------------------

e.m2 <- glm(Energy ~ log(totmass) + log(NonFat), family = Gamma(link="log"),
            data = energy)
# ?????  Other options than gamma?
# ?????  Other link options?

# Question: in the model why do we not include fat?
# Answer: it's redundant to totmass (total mass), and therefore you would lose a degree of freedom.
# Question: Why would you lose a degree of freedom?
# Answer ToDo:: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# NB: If phi is estimated, the Wald statistic is labelled t
# R uses Pearson estimate of phi, as opposed to the mean deviation estimate
# Question: What is phi?
# Answer: Phi Coefficient (Mean Square Contingency Coefficient)
# Answer Detailed: https://www.youtube.com/watch?v=nYDDjXM2SyU
# Answer Detailed Excellent: https://www.youtube.com/watch?v=7Z1eheohSAA (Phi Coefficient - Software Debugging)
# Answer ToDo: Find somethign for pearson XXXXXXXXXXXXXXXXXXXXXXXXX
# Question: Why does the wald statistic use t? (4m0s)
# Answer: XXXXXXXXXXXXXXXXXXXXXXXXX




# R uses Pearson coefficient of Phi, as opposed to the deviation estimate.
# Question: Why does R use Pearson over deviation estimate? 
# Answer: XXXXXXXXXXXXXXXXXXXXXXXXX

# Question: Explain Pr(>|t|).  What is t?
# Answer:XXXXXXXXXXXXXXXXXXXXXXXXX
# Note: https://feliperego.github.io/blog/2015/10/23/Interpreting-Model-Output-In-R
# Note: The Pr(>t) acronym found in the model output relates to the probability of observing any value equal or larger than t. 
# Note: The t-value measures the ratio between the coefficient and its standard error.
# Note: https://support.minitab.com/en-us/minitab/21/help-and-how-to/statistical-modeling/regression/how-to/fit-regression-model/interpret-the-results/all-statistics-and-graphs/coefficients-table/
# Note: https://www.youtube.com/watch?v=YzuYiKh8ec0 Extract Regression Coefficients of Linear Model in R (Example) 
# Fun fact: https://www.youtube.com/watch?v=pTmLQvMM-1M <- Excellent video
# T-test comes from guinesss!!!!
# Question: Issues:
# Question: do these coefficients have normal distribution?
# Question: less than 20-30 for samples to satisfy t-value requirements.
# Question: Wald test for t value
# https://www.statisticshowto.com/wald-test/

# Question: Why is there no wald.test() 
# Answer:XXXXXXXXXXXXXXXXXXXXXXXXX
# Notes: https://www.statology.org/wald-test-in-r/
# 

printCoefmat(coef(summary(e.m2))) 
#          Estimate Std.     Error t value  Pr(>|t|)    
# (Intercept)  1.875075   0.329204  5.6958 1.215e-07 ***
# log(totmass) 0.318133   0.065287  4.8728 4.090e-06 ***
# log(NonFat)  0.288076   0.112818  2.5535   0.01216 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# The evidence suggests coefficients of totmass and NonFat are non-zero

# Confidence intervals for coefficients -----------------------------------
# NOTE: the metrics below overlap the above.  
# (Intercept)  1.875075 is between (Intercept) 1.2320347 2.5221106
# log(totmass) 0.318133 is between log(totmass) 0.1902225 0.4454280
# log(NonFat)  0.288076 is between log(NonFat) 0.0649014 0.5104465
# so everything looks good.
confint(e.m2)
#                    2.5 %    97.5 %
#   (Intercept)  1.2320347 2.5221106
#   log(totmass) 0.1902225 0.4454280
#   log(NonFat)  0.0649014 0.5104465
# All coefficients are covered by their confidence intervals

# Calculate dispersion, phi, two ways -------------------------------------

# Quiz - Confidence invervals cover the coefficients, not the other way.
# Quiz - estimates are fixed, correct way is to say "confidence intervals cover coefficients"

# Calculate dispersion values
# Mean deviation
phi.meandev <- deviance((e.m2)) / df.residual(e.m2)         
# Pearson value
phi.Pearson <- summary(e.m2)$dispersion
c(Mean.deviance=phi.meandev, Pearson=phi.Pearson)
# Mean.deviance       Pearson 
# 0.01219603       0.01227950 
# Values close , implying either estimate of phi is acceptable
# R uses Pearson estimate

# Wald statistic with mean deviance estimator
# R uses Pearson estimator of phi as default. 
# To use the meandev estimator: dispersion = phi.meandev
printCoefmat(coef(summary(e.m2, dispersion = phi.meandev)))                     
#           Estimate Std.      Error z value  Pr(>|z|)    
#   (Intercept)  1.875075   0.328083  5.7152 1.095e-08 ***
#   log(totmass) 0.318133   0.065065  4.8895 1.011e-06 ***
#   log(NonFat)  0.288076   0.112434  2.5622    0.0104 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Either estimator yields both independent variables are non-zero

# Quiz: do coefficients (7m33s) fall within confidence invervals
# Answer: false (added a bunch of gobilty gook for confusion)
# Stupid slight of hand trick



# Reverse order -----------------------------------------------------------


# Quiz: What does anova function do when fed a GLM
# Answer: provides analysis of deviance table
# test=f provides p value of deviance differences.

# Score test: 

e.rev <- glm(Energy ~ log(NonFat) + log(totmass),
             family=Gamma(link="log"), data=energy)
anova(e.rev , test="F")
#              Df Deviance Resid. Df Resid. Dev      F    Pr(>F)    
# NULL                           103     2.3365                     
# log(NonFat)   1  0.81508       102     1.5215 66.377 1.035e-12 ***
# log(totmass)  1  0.28968       101     1.2318 23.590 4.363e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Order of entry not important. Either way supports both coefficients
# are non-zero

# Score tests -------------------------------------------------------------


library(statmod)
mA <- glm(Energy~log(NonFat), family=Gamma(link = "log"), data=energy)
t.tot <- glm.scoretest(mA, log(energy$totmass))
p.tot <- 2 * pt(-abs(t.tot), df=df.residual(mA))  # two-tailed p-value
tbl <- data.frame(Score.stat=t.tot, P.Value=p.tot)
print(tbl, digits=3)
# Score.stat  P.Value
#       4.37 2.98e-05

# Now consider NonFat, conditional on totmass
mB <- glm(Energy~log(totmass), family=Gamma(link = "log"), data=energy)
t.NonFat <- glm.scoretest(mB, log(energy$NonFat))
p.NonFat <- 2 * pt(-abs(t.NonFat), df=df.residual(mB))
tbl <- data.frame(Score.test=t.NonFat, P.Value=p.NonFat)
print(tbl, digits=3)
# Score.test P.Value
#       2.44  0.0165
#
# We conclude that both explanatory variables are needed

# Question: doesn't this conflict with the above?
# Question NonFat is part of totmass
# Answer: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


# Quiz: (10m50sec) What is better for this situation?  Wald test or score test?  Why? 
# Answer: Wald test is better for individual coefficients.  
# Answer: Score test is better for deciding if a new explanatory variable should be added.

