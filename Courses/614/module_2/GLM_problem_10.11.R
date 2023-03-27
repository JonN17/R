# GLM_problem_10.11.R
# Cervical cancer in women of different ages in 4 European countries

library(MASS)
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
   ### Part 3
cc.m0 <- glm(Deaths~offset(log(Wyears)) + Age + Country,
             data=cervical, family = poisson)
plot(rstandard(cc.m0) ~ fitted(cc.m0), main="Poissn glm") 
   ### Part 4
cc.m0Q <- glm(Deaths~offset(log(Wyears)) + Age + Country,
             data=cervical, family = quasipoisson)
plot(rstandard(cc.m0Q) ~ fitted(cc.m0Q), main="Quasi-Poisson glm") 
   ### Part 5
cc.m0NB <- glm(Deaths~offset(log(Wyears)) + Age + Country,
              data=cervical)
cc.m0NB <- glm.convert(cc.m0NB)
plot(rstandard(cc.m0NB) ~ fitted(cc.m0NB), main="Neg. bi. glm")
   ### Part 6
# All models seem to have a large negative outlier, but clearly the 
# Poisson model does not accommodate the variation correctly
summary(cc.m0)
