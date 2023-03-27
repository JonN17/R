# GLM_onion_yield.R

library(GLMsData)
data("yieldden")
summary(yieldden)
# Yield             Dens             Var   
# Min.   : 16.30   Min.   : 2.140   Min.   :1  
# 1st Qu.: 25.95   1st Qu.: 4.647   1st Qu.:1  
# Median : 48.20   Median : 8.695   Median :2  
# Mean   : 54.71   Mean   :12.344   Mean   :2  
# 3rd Qu.: 72.58   3rd Qu.:18.483   3rd Qu.:3  
# Max.   :131.60   Max.   :31.080   Max.   :3  


# Data prep ---------------------------------------------------------------

#  Var (variety) is a factor, not a numerical value
yieldden$Var <- factor(yieldden$Var)
# let YD be the yield per unit area 
yieldden$YD <- with(yieldden, Yield*Dens)
summary(yieldden)
# Yield             Dens        Var          YD       
# Min.   : 16.30   Min.   : 2.140   1:10   Min.   :281.6  
# 1st Qu.: 25.95   1st Qu.: 4.647   2:10   1st Qu.:335.7  
# Median : 48.20   Median : 8.695   3:10   Median :418.6  
# Mean   : 54.71   Mean   :12.344          Mean   :423.2  
# 3rd Qu.: 72.58   3rd Qu.:18.483          3rd Qu.:509.6  
# Max.   :131.60   Max.   :31.080          Max.   :598.4

# Plots -------------------------------------------------------------------

plot(yieldden$Dens,yieldden$Yield)
# positive continuous data, skewed right
plot(yieldden$Dens,yieldden$YD)
# as density increase, yield per unit area is more variable
# these observations suggest a gamma GLM

# Model -------------------------------------------------------------------

# model with interactions
yd.glm.int <- glm(YD ~ (Dens + I(1/Dens)) * Var,
                  family = Gamma(link = inverse), data=yieldden)
# in R, the analysis of deviance table is returned by anova()
# test="F" is needed to obtain p-values
round(anova (yd.glm.int, test="F"), 2)
#               Df Deviance Resid. Df Resid. Dev      F Pr(>F)    
# NULL                             29       1.45                  
# Dens           1     1.00        28       0.45 191.67 <2e-16 ***
# I(1/Dens)      1     0.27        27       0.18  51.28 <2e-16 ***
# Var            2     0.06        25       0.12   5.48   0.01 ** 
# Dens:Var       2     0.01        23       0.12   0.57   0.57    
# I(1/Dens):Var  2     0.01        21       0.11   0.53   0.59    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# neither of the interaction terms are significant
# refit model with no interactions

# model without interactions
yd.glm <- update(yd.glm.int, . ~ Dens + I(1/Dens) + Var)
round(anova(yd.glm, test="F"), 2)
# the fitted model is:
printCoefmat(coef(summary(yd.glm)), 5)
#                Estimate  Std. Error t value  Pr(>|t|)    
# (Intercept)  1.9687e-03  1.3934e-04 14.1292 2.009e-13 ***
# Dens        -1.2609e-05  5.1637e-06 -2.4419  0.022026 *  
# I(1/Dens)    3.5744e-03  4.9364e-04  7.2409 1.376e-07 ***
# Var2         1.0015e-04  7.1727e-05  1.3963  0.174914    
# Var3         2.4503e-04  7.1187e-05  3.4420  0.002041 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Confidence intervals for individual coefficients ------------------------

confint(yd.glm)
#                     2.5 %        97.5 %
# (Intercept)  1.696107e-03  2.242322e-03
# Dens        -2.269013e-05 -2.447041e-06
# I(1/Dens)    2.615438e-03  4.550577e-03
# Var2        -4.011622e-05  2.411092e-04
# Var3         1.058228e-04  3.849394e-04

# Adequacy of model -------------------------------------------------------

library(statmod)
scatter.smooth( rstandard(yd.glm) - log(fitted(yd.glm)), las=1,
      xlab = "Log of fitted values", ylab="Standardized residuals")
# residuals appear equally on either side of mean line

plot(cooks.distance(yd.glm), type="h", las=1,
     ylab="Cook's distance, D")

# look for influential obsevations
im <- influence.measures(yd.glm)
colSums(im$is.inf)
# dfb.1_ dfb.Dens dfb.I(1/ dfb.Var2 dfb.Var3    dffit    cov.r   cook.d      hat 
#      0        0        0        0        0        0        2        0        0
# Two observations are identified as influenctial, but only by 
# the covariance ratio, cov.r. Cooks distance did not identify any
# influential observations so we should not be too concerned

qqnorm(qr <- qresid(yd.glm), las=1)
qqline(qr)
# residuals are approximately normally distributed

boxplot(rstandard(yd.glm) ~ yieldden$Var, las=1,
     xlab="Variety", ylab = "Standardized residuals")
# no great difference between varieties

# Measure of dispersion, phi ----------------------------------------------

summary(yd.glm)$dispersion
# 0.004789151
# since the estimate of phi is small, the saddlepoint approximation
# to the EDM density function will be very accurate, and the distributional
# assumptions used in inferences are accurate




