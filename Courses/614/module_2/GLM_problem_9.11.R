# GLM_problem_9.11.R

# Data prep ---------------------------------------------------------------

# li: lymphocytic infiltration
li <-  factor( c(0,0,0,0,1,1,1,1), labels=c("Absent", "Present"))
m <-  c(3,2,4,1,5,5,9,17)   # group size
y <- c(3,2,4,1,5,3,5,6)     # number of successes
# gl(): function to generate factor levels
# 2 levels, repeated in groups of 2, up to length 8
# See second column Table 9.11, p. 366
gender <- gl(2,2,8,labels=c("Female", "Male"))

par(mfrow=c(1,3))           # 1 row with 3 plots

   ### Part 1
# Proportion of successes against presence or absence of
# lymphocytic infiltration
plot(y/m~li)

# Proportion of successes against gender
plot(y/m~gender)

interaction.plot(li, gender, y/m)

   ### Part 2
m1 <- glm(y/m~gender, weights=m, family=binomial)
m2 <- glm(y/m~li+gender,weights=m, family=binomial)
m3 <- glm(y/m~gender+li, weights=m, family=binomial)
# Wald test. Effect of lymphocytic infiltration not significant
summary(m2)

   ### Part 3
# likelihood ratio test shows effect of lymphocytic infiltration
# is significant
anova(m2, test = "Chisq")
anova(m3, test = "Chisq")

   ### Part 4
# score test shows effect of lymphocytic infiltration
# is significant
z.score <- glm.scoretest(m1,as.numeric(li))
p.score <- 2*(1-pnorm(abs(z.score)))
c(z.score, p.score)

   #### Part 5
# Wald test results show nothing greatly significant; the others do.
# This is the Hauck-Donner effect, since y/m is always 1 when li is Absent