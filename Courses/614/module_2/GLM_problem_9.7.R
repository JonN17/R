# GLM_problem_9.7.R
# Challenger disaster

library(GLMsData)
data(shuttles)

library(statmod)
   ### Part 1
plot(Damaged/6~Temp,data = shuttles)
# Note that the lowest experienced temperature was 53 degrees
   ### Part 2
# There are 6 O-rings on a space shuttle rocket.
# length(Temp) = 23, the number of previously recovered launch equipments
length(shuttles$Temp) # returns 23
shuttle.m <- glm(Damaged/6~Temp, weights=rep(6, length(Temp)),
                 family=binomial, data = shuttles)
summary(shuttle.m)
# Temp is significant at the 0.05 level.
# As seen by
# Temp        -0.11560    0.04702  -2.458   0.0140 *
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# There is no overdispersion since residual deviance, 18.086, is less than
# the 21 degrees of freedom

# Question: Why does Residual deviance < Degrees of freedom mean there is no over dispersion?
# Answer: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxx

   ### Part 3 - perform a diagnostic analysis.
qqnorm(qresid(shuttle.m))
qqline(qresid(shuttle.m))
# residuals appear to be reasonably normally distributed
plot(cooks.distance(shuttle.m), type = "h", las=1, main = "Cook's D",
     ylab = "Cook's Distance, D")
colSums(influence.measures(shuttle.m)$is.inf)
# There are no outlier influencers according to Cooks distance, D.
# Question: Whty does this show no outliers?
# Answer: cook.d is 0
# Question: Why does the influence measures override the plot?  Can the plot override the influence.measures?
# Answer: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

   ### Part 4
data.frame(temp=31)

max(shuttles$Temp)

# day of the challenger launch the temperature was 31 F fahrenheit.

predict(shuttle.m, newdata=data.frame(Temp=31), type="response")
# 0.8177744  is the predicted probability of an O-ring failure at 31
# degrees Fahrenheit
   ### Part 5
# The temperature at which 50% of O-rings fail. Since we do not want
# O-rings to fail, probably a higher threshold would be more appropriate.
# By trial and error/ search, ED50 occurs at Temp=44.


# ED50 - temperature is 50%
# happens in medicine https://registries.ncats.nih.gov/glossary/median-effective-dose/
# ED50 - 50% effective dose
# in this case 50% failure rate.
# Brute force investigation.
