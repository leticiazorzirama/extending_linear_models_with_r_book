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

# INTERPRETATION WITH BASELINE
# With all other predictors held constant...

# COMPARED instead BECAUSE
# Given two counties with the same values of the predictors, except having different voting
# equipment, we would predict the undercount to be 1.56% higher for the OS-PC county
# compared to the lever county. However, we would not go so far as to say that if we went
# to a county with lever equipment and changed it to OS-PC that this would cause the
# undercount to increase by 1.56%.

