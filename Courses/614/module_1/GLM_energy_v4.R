# GLM_energy v4.R

library(GLMsData)
data(energy)

# The energy expenditure for 104 females at rest for a 24 hour period
# The total mass of each subject is the sum of fat and fat-free tissue masses
# Use this information to calculate total mass

energy$totmass <- energy$Fat + energy$NonFat
plot(energy$totmass,energy$Energy)
plot(log(energy$totmass),energy$Energy)

plot(energy$Fat,energy$Energy)
plot(log(energy$Fat),energy$Energy)

plot(energy$NonFat,energy$Energy)
plot(log(energy$NonFat),energy$Energy)


# Wald Tests --------------------------------------------------------------

e.m2 <- glm(Energy ~ log(totmass) + log(NonFat), family = Gamma(link="log"),
            data = energy)

# NB: If phi is estimated, the Wald statistic is labelled t
# R uses Pearson estimate of phi, as opposed to the mean deviation estimate
printCoefmat(coef(summary(e.m2))) 
#          Estimate Std.     Error t value  Pr(>|t|)    
# (Intercept)  1.875075   0.329204  5.6958 1.215e-07 ***
# log(totmass) 0.318133   0.065287  4.8728 4.090e-06 ***
# log(NonFat)  0.288076   0.112818  2.5535   0.01216 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# The evidence suggests coefficients of totmass and NonFat are non-zero

# Confidence intervals for coefficients -----------------------------------

confint(e.m2)
#                    2.5 %    97.5 %
#   (Intercept)  1.2320347 2.5221106
#   log(totmass) 0.1902225 0.4454280
#   log(NonFat)  0.0649014 0.5104465
# All coefficients are covered by their confidence intervals

# Calculate dispersion, phi, two ways -------------------------------------


phi.meandev <- deviance((e.m2)) / df.residual(e.m2)                             
phi.Pearson <- summary(e.m2)$dispersion
c(Mean.deviance=phi.meandev, Pearson=phi.Pearson)
# Mean.deviance       Pearson 
# 0.01219603       0.01227950 
# Values close , implying either estimate of phi is acceptable
# R uses Pearson estimate

# Wald statistic with mean deviance estimator
# R uses Pearson estimator of phi. To use the meandev estimator:
printCoefmat(coef(summary(e.m2, dispersion = phi.meandev)))                     
#           Estimate Std.      Error z value  Pr(>|z|)    
#   (Intercept)  1.875075   0.328083  5.7152 1.095e-08 ***
#   log(totmass) 0.318133   0.065065  4.8895 1.011e-06 ***
#   log(NonFat)  0.288076   0.112434  2.5622    0.0104 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Either estimator yields both independent variables are non-zero

# Reverse order -----------------------------------------------------------


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

