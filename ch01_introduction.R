library(faraway)
data(gavote)
help(gavote)
head(gavote)

# Purpose:
# Determine what factors affect the undercount 

summary(gavote)

# Number of ballots cast ranges over orders of magnitudes. 
# This suggests that we should consider the relative, rather than the absolute, undercount.
gavote$undercount <- (gavote$ballots-gavote$votes)/gavote$ballots

# The mean across counties is 4.38%.
summary(gavote$undercount)

# Right skewed distribution
hist(gavote$undercount,main="Undercount",xlab="Percent Undercount")
plot(density (gavote$undercount), main="Undercount")
rug(gavote$undercount) # 2 outliers

# Types of voting equipment
# Lever is the major type
pie(table(gavote$equip), col=gray(0:4/4))
barplot(sort(table(gavote$equip), decreasing=TRUE),las=2)

# Proportion voting for Gore relates to the proportion of African Americans
# Strong correlation
gavote$pergore <- gavote$gore/gavote$votes
plot(pergore ~ perAA, gavote, xlab="Proportion African American", ylab="Proportion for Gore")

# Undercount behavior by equipment
# No major differences in undercount for the different types of equipment
plot(undercount ~ equip, gavote, xlab=" ", las=3)

# Rural are mostly outside of Atlanta 
xtabs(~ atlanta + rural, gavote)

# Correlations
nix <- c(3,10,11,12)
cor(gavote[,nix])

# Model the undercount as the response and the proportions of Gore voters and African Americans as predictors
lmod1 <- lm(undercount ~ pergore + perAA, gavote)

# Obtain the least squares estimates of β, called the regression coefficients
coef(lmod1)

# Important:
# The construction of the least squares estimates do not require any assumptions about ε. 
# If we are prepared to assume that the errors are at least independent and have equal
# variance, then the Gauss-Markov theorem tells us that the least squares estimates are the
# best linear unbiased estimates. Although it is not necessary, we might further assume that
# the errors are normally distributed, we might compute the maximum likelihood estimate (MLE) of β 
# 
# For the linear models, these MLEs are identical with the least squares estimates. 
# However, we shall find that, in some of the extension of linear models considered later in this book, 
# an equivalent notion to least squares is not suitable and that likelihood methods must be used. 
# This issue does not arise with the standard linear model.

# Predicted values
predict(lmod1)

# Residuals
residuals(lmod1)

# Residual Sum of Squares
# For linear models, the deviance is the RSS
deviance(lmod1)

# Degrees of freedom = number of cases - number of coefficients
df.residual(lmod1)
nrow(gavote) - length(coef(lmod1))

# Variance = σ2
# σ is estimated by the residual standard error
sqrt(deviance(lmod1) / df.residual(lmod1))

# Summary
lmod1sum <- summary(lmod1)
lmod1sum$sigma

# The deviance measures how well the model fits in an absolute sense, 
# but it does not tell us how well the model fits in a relative sense.
# The popular choice is R2, called the coefficient of determination 
# or percentage of variance explained.

lmod1sum$r.squared
cor(predict(lmod1), gavote$undercount)^2

# R2 suffers as a criterion for choosing models among those available because it can never
# decrease when you add a new predictor to the model. This means that it will favor the
# largest models. The adjusted R2 makes allowance for the fact a larger model also uses
# more parameters.

lmod1sum$adj.r.squared

# Qualitative variables
# Dummy variables
gavote$cpergore <- gavote$pergore - mean(gavote$pergore)
gavote$cperAA <- gavote$perAA - mean(gavote$perAA)
lmod2 <- lm(undercount ~ cperAA + cpergore * rural + equip, gavote)
summary(lmod2)

# INTERPRETING PRACTICE

# ESTABLISHING THE BASELINE

# Consider a rural county which has an average proportion of Gore voters and an average
# proportion of African Americans where lever machines are used for voting. Because rural
# and lever are the reference levels for the two qualitative variables, there is no contribution
# to the predicted undercount from these terms. Furthermore, because we have centered the
# two quantitative variables at their mean values, these terms also do not enter into the
# prediction. Notice the worth of the centering because otherwise we would need to set
# these variables to zero to get them to drop out of the prediction equation; zero is not a
# typical value for these predictors. Given that all the other terms are dropped, the predicted
# undercount is just given by the intercept, which is 4.33%

# Baselines: 
# Rural
# Lever
# average proportion of Gore voters 
# average proportion of African Americans

# COMPARED instead BECAUSE

# Given two counties with the same values of the predictors, except having different voting
# equipment, we would predict the undercount to be 1.56% higher for the OS-PC county
# compared to the lever county. However, we would not go so far as to say that if we went
# to a county with lever equipment and changed it to OS-PC that this would cause the
# undercount to increase by 1.56%.

#                         Estimate  Std. Error    t value     Pr(>|t|)
# equipOS-PC           0.015639603 0.005827389  2.6838095 8.096872e-03

# Practicing:
# With all other predictions unchanged, the undercount would be 1.56% higher for
# the OS-PC county compared to the lever county. 

# RESCALING OF INTERPRETATION

# With all other predictors held constant, we would predict the undercount to increase
# by 2.83% going from a county with no African Americans to all African American.

#                         Estimate  Std. Error    t value     Pr(>|t|)
# cperAA               0.028264080 0.031092148  0.9090424 3.647860e-01

# Sometimes a one-unit change in a predictor is too large or too small, prompting a
# rescaling of the interpretation. For example, we might predict a 0.283% increase in the
# undercount for a 10% increase in the proportion of African Americans. Of course, this
# interpretation should not be taken too literally. We already know that the proportion of
# African Americans and Gore voters is strongly correlated so that an increase in the
# proportion of one would lead to an increase in the proportion of the other. This is the
# problem of collinearity which makes interpretation of regression coefficients more
# difficult. Furthermore, the proportion of African Americans is likely to be associated with
# other socioeconomic variables which might also be related to the undercount. This further
# hinders the possibility of a causal conclusion.

# INTERPRETING INTERACTION
# The interpretation of the rural and pergore cannot be done separately as there is an
# interaction term between these two variables. 
# 
# We see that for an average number of Gore voters, we would predict a 1.86%-lower undercount 
# in an urban county compared to a rural county. 

#                      Estimate Std. Error t value Pr(>|t|) 
# ruralurban          -0.018637   0.004648  -4.009 9.56e-05 *** 

# In a rural county, we predict a 0.08% increase in the undercount as the
# proportion of Gore voters increases by 10%. 

#                      Estimate Std. Error t value Pr(>|t|) 
# cpergore             0.008237   0.051156   0.161   0.8723 
 
# In an urban county, we predict a (0.00824–0.00880)*10=−0.0056% increase in the undercount as the proportion of Gore voters
# increases by 10%. Since the increase is by a negative amount, this is actually a decrease.
# This illustrates the potential pitfalls in interpreting the effect of a predictor in the presence
# of an interaction. We cannot give a simple stand-alone interpretation of the effect of the
# proportion of Gore voters. The effect is to increase the undercount in rural counties and to
# decrease it, if only very slightly, in urban counties.

# Hypothesis testing
# Compare 2 linear models
anova(lmod1, lmod2)
# p-value indicates the null hypothesis of preferring the smaller model should be rejected

# Test specifc predictors
summary(lmod2)$coef

# Test qualitative predictors with >levels
drop1(lmod2, test="F")
# equipment is barely statistically significant in that the p-value is just less
# than the traditional 5% significance level.

# Confidence intervals
confint(lmod2)
