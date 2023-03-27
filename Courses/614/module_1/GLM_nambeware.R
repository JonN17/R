# GLM_nambeware.R

library(GLMsData)
data(nambeware)
summary(nambeware)

# Plots -------------------------------------------------------------------


plot(nambeware$Price,nambeware$Diam)
plot(nambeware$Price,nambeware$Time)

# Price increases with diameter and with time
# Plot exhibits heteroskedastic distribution
# 1) Variance increases with the mean, V(mu) == (mu)^2
# 2) Appears Diam and Time are both linearly related to Price

# Build model, one variable at a time -------------------------------------
# When phi, the dispersion statistic is estimated as it is here,
# the Wald statistic is represented by t

# Diam
n.mD <- glm(Price ~ Diam,  data=nambeware, family = Gamma(link="inverse"),
            control = list(trace=TRUE))
summary(n.mD)
 
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  2.142e-02  1.573e-03  13.613  < 2e-16 ***
#   Diam      -7.736e-04  8.576e-05  -9.021 1.43e-12 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Time
n.mT <- glm(Price ~ Time,  data=nambeware, family = Gamma(link="inverse"),
            control = list(trace=TRUE))
summary(n.mT)

# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.0186244  0.0010579   17.61   <2e-16 ***
#   Time      -0.0001530  0.0000132  -11.59   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Both together:
n.m1 <- glm(Price ~ Diam + Time, data=nambeware, family = Gamma(link="inverse"),
            control = list(trace=TRUE))
summary(n.m1)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -0.92891  -0.36172  -0.00687   0.28710   0.77900  
# 
# Coefficients:
#               Estimate  Std. Error t value Pr(>|t|)    
# (Intercept)  1.933e-02  1.361e-03  14.202  < 2e-16 ***
#   Diam      -1.638e-04  1.939e-04  -0.845  0.40177    
# Time        -1.230e-04  3.776e-05  -3.256  0.00192 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for Gamma family taken to be 0.1514793)
# 
# Null deviance: 20.2679  on 58  degrees of freedom
# Residual deviance:  9.8449  on 56  degrees of freedom
# AIC: 577.3
# 
# Number of Fisher Scoring iterations: 6

# Calculating explicitly yields same result for Pearson estimator of phi
w <- weights(n.m1, type="working")
e <- residuals(n.m1, type="working")
sum(w * e^2)/df.residual(n.m1)
# 0.1514793

# Wald test ---------------------------------------------------------------
     
                                                                            
printCoefmat(coef(summary(n.m1)))
#                Estimate  Std. Error t value  Pr(>|t|)    
# (Intercept)  0.01932605  0.00136079 14.2021 < 2.2e-16 ***
#   Diam      -0.00016381  0.00019389 -0.8449  0.401767    
# Time        -0.00012296  0.00003776 -3.2563  0.001919 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Coefficient of Time is non-zero but that for Diam is not

# Score test --------------------------------------------------------------


library(statmod)  
# Consider Time, conditional on Diam
mA <- glm(Price ~ Diam, family=Gamma(link = "inverse"), data=nambeware)
t.Time <- glm.scoretest(mA, nambeware$Time)
p.Time <- 2 * pt(-abs(t.Time), df=df.residual(mA))  # two-tailed p-value
tbl <- data.frame(Score.stat=t.Time, P.Value=p.Time)
print(tbl, digits=3)
# Score.stat P.Value
#      -2.68 0.00948

# Now consider Diam, conditional on Time
mB <- glm(Price ~ Time, family=Gamma(link = "inverse"), data=nambeware)
t.Temp <- glm.scoretest(mB, log(nambeware$Diam))
p.Temp <- 2 * pt(-abs(t.Temp), df=df.residual(mB))
tbl <- data.frame(Score.test=t.Temp, P.Value=p.Temp)
print(tbl, digits=3)
# Score.test P.Value
#      -3.38 0.00131

# Score tests suggests both coefficients are non-zero but Wald
# test on both variables at the same time indicates the coefficient
# of Diam is not non-zero.
# So which is it for the coefficient of Diam?
# Text says score tests are good for deciding if new explanatory variables
# should enter the current model. We will accept the results of the score
# test and include both Time and Diam in our model



