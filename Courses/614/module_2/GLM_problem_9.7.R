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
shuttle.m <- glm(Damaged/6~Temp, weights=rep(6, length(Temp)),
                 family=binomial, data = shuttles)
summary(shuttle.m)
# Temp is significant at the 0.05 level.
# There is no overdispersion since residual deviance, 18.086, is less than
# the 21 degrees of freedom
   ### Part 3
qqnorm(qresid(shuttle.m))
qqline(qresid(shuttle.m))
# residuals appear to be reasonably normally distributed
plot(cooks.distance(shuttle.m), type = "h", las=1, main = "Cook's D",
     ylab = "Cook's Distance, D")
colSums(influence.measures(shuttle.m)$is.inf)
# There are no outlier influencers according to Cooks distance, D.
   ### Part 4
predict(shuttle.m, newdata=data.frame(Temp=31), type="response")
# 0.8177744  is the predicted probability of an O-ring failure at 31
# degrees Fahrenheit
   ### Part 5
# The temperature at which 50% of O-rings fail. Since we do not want
# O-rings to fail, probably a higher threshold would be more appropriate.
# By trial and error/ search, ED50 occurs at Temp=44.




