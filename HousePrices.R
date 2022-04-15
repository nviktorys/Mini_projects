#=================================================================================#
# 1.                            Initial Data Analysis                             #
#=================================================================================#

# Get housing data
housing <- read.csv("Data/housing.csv")
head(housing)
summary(housing)
# The initial observations from summary table are:
# a) There is skewness in the bath, sqft, precip and price variables (high max values)
#   which can potentially be an outlier values.
# b) There appear to be negative precip values (min=-110), which look like input data error

# 1) Test for outliers
library(car)
par(mfrow=c(2,2))
plot(lm(price~., data=housing))
outlierTest(lm(price~., data=housing))
housing[c(348),]
par(mfrow=c(1,1))
plot(housing$bath)
plot(housing$price)
plot(housing$sqft)
abline(plot(price~sqft, housing))
abline(lm(price~sqft, housing))
abline(lm(price~sqft, housing[-348,]))
# The graphs and outlier test indicate that observation 348 stands out from the data
# which can clearly be observed from the residual plots and the Cook's distance
# Looking closer at that observation and variables with unusually high max values
# confirmed that this observation is an outlier and therefore will be removed from 
# the data to remove it's influence on the model.
housing2 <- housing[-c(348),]
plot(lm(price~., data=housing2))
outlierTest(lm(price~., data=housing2))
# There are no other outliers identified by the outlier test.

# 2) Unusual values
# Check negative precipitation values
housing2[c(housing2$precip<0),]
plot(housing$precip)
# While this observation has not been identified as an outlier or influential point 
# in the above tests, the negative amount of percipitation doesn't seem to be 
# realistic. While it may not have a significant influence on the model this 
# observation considered unreliable, and therefore will be removed from data.
housing3 <- housing2[c(housing2$precip>0),]


#=================================================================================#
# 2.                            Exploration  Analysis                             #
#=================================================================================#

# 1) Plot all variables to identify dependencies
library(psych)
pairs.panels(housing3, method="pearson", ellipses=FALSE)
# The initial pairs plot accross all the variables revield following information:
# a) Price has a positive relationship with the number of bath and a high correlation
#   coefficient which suggest it will be a good predictor in the model. 
#   There is no other clear relationships with target variable (price).
# b) dist_am1, dist_am2 and dist_am3 have a strong positive relationship which suggests that 
#   only one of them needed in the model. However since neither of them shows a better
#   relationship with the target variable we will keep all of them for now.

library(ggplot2)
ggplot(housing3, aes(elevation, price)) + geom_point() + stat_smooth(method="lm")
ggplot(housing3, aes(dist_am1, price)) + geom_point() + stat_smooth(method="lm")
ggplot(housing3, aes(dist_am2, price)) + geom_point() + stat_smooth(method="lm")
ggplot(housing3, aes(dist_am3, price)) + geom_point() + stat_smooth(method="lm")
ggplot(housing3, aes(bath, price)) + geom_point(position=position_jitter(width=.2, height=0))
ggplot(housing3, aes(sqft, price)) + geom_point() + stat_smooth(method="lm")
ggplot(housing3, aes(parking, price)) + geom_point(position=position_jitter(width=.2, height=0))
ggplot(housing3, aes(precip, price)) + geom_point() + stat_smooth(method="lm")
# Closer look at the each characteristic against target variable didn't reveal 
# any trends and therefore no variable transformations will be considered.

# 2) Backward elimination using p-value
# Here we first construct general model with all variables and then remove
# variables on by one based on the highest p-value above critical value 0.05.
model0 <- lm(price~., housing3)
summary(model0)
# max p-value=0.869 (dist_am2), Radj=0.8583
model0 <- update(model0, .~.-dist_am2)
summary(model0)
# max p-value=0.7586 (dist_am3), Radj=0.8586
model0 <- update(model0, .~.-dist_am3)
summary(model0)
# max p-value=0.7523 (precip), Radj=0.8589
model0 <- update(model0, .~.-precip)
summary(model0)
# max p-value=0.4224 (dist_am1), Radj=0.8591
model0 <- update(model0, .~.-dist_am1)
summary(model0)
# max p-value=0.4016 (elevation), Radj=0.8592
model0 <- update(model0, .~.-elevation)
summary(model0)
# The last iteration suggest that parking "Not Provided" should be excluded next.
# However, since it's not an independent variable but a variation of the categorical
# variable "parking" it can't be excluded on its own. 

# One solution here can be to split categorical variable parking into four separate
# characteristics:

housing3$pOpen <- ifelse(housing3$parking=="Open",1,0)
housing3$pCovered <- ifelse(housing3$parking=="Covered",1,0)
housing3$pNoParking <- ifelse(housing3$parking=="No Parking",1,0)
housing3$pNotProvided <- ifelse(housing3$parking=="Not Provided",1,0)
str(housing3)
model0<-lm(price~bath+sqft+pOpen+pCovered+pNoParking+pNotProvided, housing3)
summary(model0)
# max p-value=0.5027 (pOpen), Radj=0.8593
model0 <- update(model0, .~.-pOpen)
summary(model0)
# max p-value=0.7496 (pNoParking), Radj=0.8596
model0 <- update(model0, .~.-pNoParking)
summary(model0)
# max p-value=0.4132 (pNotProvided), Radj=0.8596
model0 <- update(model0, .~.-pNotProvided)
summary(model0)
# max p-value=0.2135 (sqft), Radj=0.8597
model0 <- update(model0, .~.-sqft)
summary(model0)
# Rsq = 0.8601, Radj = 0.8595
# Here we don't have any p-values above 0.05 remaining and therefore will be 
# the final model with explanatory variables bath and pCovered 
model0 <- lm(price~bath+pCovered, housing3)
#############################################
# model0: lm(price~bath+pCovered, housing3) #
#         Rsq = 0.8601, Radj = 0.8595       #
#############################################

# Another solution can be to merge it with another value of "parking" variable.
ggplot(housing3, aes(price, colour=parking, fill=parking)) + geom_density(alpha=0.55)
# "Not provided" has a similar distribution with "Open" values of parking by price
# thus these two values can be merged:
library(plyr)
parking2 <- housing3$parking
parking2 <- revalue(parking2, c("Not Provided"="Open"))
housing3$parking2 <- parking2
summary(housing3)

# repeat the last model iteration with new parking variable:
model1 <- lm(price~bath+sqft+parking2, housing3)
summary(model1)
# max p-value=0.21003 (sqft), Radj=0.8595
model1 <- update(model1, .~.-sqft)
summary(model1)
# Rsq = 0.8602, Radj = 0.8593.
# Here the final model still has p-values slightly greater than 0.05. But given 
# that both parking parameters are very close to 0.05 we will stop here.
# We can additionaly check the confidence intervals for these parameters:
confint(model1)
# Both "parking" parameters include 0, thus we can conclude that parking in this
# case is not significant predictors in the model and we should remove this variable.
model1 <- update(model1, .~.-parking2)
summary(model1)
# Rsq = 0.8588, Radj = 0.8585
# The final model here only contains on variable: bath
model1 <- lm(price~bath, housing3)
#############################################
# model1: lm(price~bath, housing3)          #
#         Rsq = 0.8588, Radj = 0.8585       #
#############################################

# 3) Stepwise selection.
# Combine forevard and backward selection methods and use AIC for elimination.
# We will use the final dataset with additional parking variables and allow
# the model to select the one which contributes more to the model.
model2 <- lm(price~., housing3)
step(model2, direction="both")
summary(lm(price~bath+pCovered, housing3))
# The final model ended up to be the same as the initial model and includes 
# variables bath and pCovered with R = 0.8601 and	Radj = 0.8595.

# 4) Factors and interactions
# The first model, model0, has two parameters: bath and pCovered. The bath is a 
# categorical variable with 4 values and therefore we can attempt to fit it 
# as a factor with separate or parallel lines.
housing3$f.bath <- as.factor(housing3$bath)
ggplot(data=housing3, aes(x=pCovered, y=price, colour=f.bath, shape=f.bath)) +
  geom_point(size = 3, alpha = 0.8, position=position_jitter(width=.2, height=0)) + 
  geom_smooth(method = "lm", fill = NA, fullrange = TRUE)
# From the graph it seems that fitting bath as a factor with parallel lines is a 
# good idea. We should try fitting the interaction to the model0 first.
model3 <- lm(price~f.bath*pCovered, housing3)
summary(model3)
anova(model3)
confint(model3)
# Neither the confidence interval and p-value for the interaction terms provide 
# enough evidence to conclude that the two seperate regressions model is appropriate, 
# so we will instead try to fit the “parallel lines” model to the data.
model4 <- lm(price~f.bath+pCovered, housing3)
summary(model4)
anova(model4)
confint(model4)
# Baed on the CI and p-value all parameters appear to be significant. The Radj 
# has a slight improvement comparing to the model0: Radj=0.8619, up by 0.024.
#################################################
# model4: lm(price~f.bath+pCovered, housing3)   #
#         Rsq = 0.863, Radj = 0.8619            #
#################################################

#=================================================================================#
# 3.              Diagnostics and Final Model Selection                           #
#=================================================================================#
# The 3 final models are model0, model1, and model4. Given that model0 and model1
# only differ by one variable and model0 has a slightly better Radj, we will 
# reject model1 and check model assumptions for the remaining two models.

### I model4: price~f.bath+pCOvered 
# Based on the Radj the model4 looks as a better fit. We should check model 
# assumptions to make a final decision.
# 1) Significance of factors:
confint(model4)
anova(model4)
# both factors are significant
# 2) Check if errors are normally distributed and scale of variability.
qqnorm(residuals(model4))
qqline(residuals(model4))
hist(residuals(model4), xlab="residuals", main="")
# The Q-Q plot shows light tails, and thus we cannot confirm the normality of residuals:
shapiro.test(residuals(model4))

plot(jitter(fitted(model4)), residuals(model4), xlab="fitted", ylab="residuals")
abline(h=0)
# The reiduals vs fitted plot suggests that the variability is approximately constant with fewer observations in the first
# and last groups, which could be the reason for the lighter tails in the Q-Q plot.
# We can double check consistency of variation:
house<-housing3[(1:24)*2,]
house$price2<-sqrt(abs(residuals(model4)))[(1:24)*2]
vmod<-lm(price2~f.bath+pCovered, house)
anova(vmod)
# The p-values are above 0.05, which confirms that there are no strong evidence 
# against constant variation among f.bath and pCovered.

# 3) Test for correlation in errors
n<-length(residuals(model4))
plot(tail(residuals(model4),n-1)~head(residuals(model4),n-1), xlab="epsilon(i)",
     ylab="epsilon(i+1)")
abline(h=0, v=0, col=grey(0.75))
library(lmtest)
dwtest(price~f.bath+pCovered, data=housing3)
# the high p-value indicates that there is not enough evidence to reject H0: 
# the errors are uncorrelated.

# 4) Given the above we can conclude that the errors of the model4 are independent
# have a constant variablility and therefore capture all the non-random structure
# in data. 

### II model0: price~bath+pCOvered 
# Based on the Radj the model4 looks as a better fit. We should check model 
# assumptions to make a final decision.
# 1) Significance of factors:
confint(model0)
anova(model0)
# all variables are significant
# 2) Check if errors are normally distributed and scale of variability.
qqnorm(residuals(model0))
qqline(residuals(model0))
hist(residuals(model0), xlab="residuals", main="")
# The Q-Q plot shows light tails, and thus we cannot confirm the normality of residuals:
shapiro.test(residuals(model0))

plot(jitter(fitted(model0)), residuals(model0), xlab="fitted", ylab="residuals")
abline(h=0)
# The reiduals vs fitted plot suggests that the variability is approximately constant with fewer observations in the first
# and last groups, which could be the reason for the lighter tails in the Q-Q plot.
# We can double check consistency of variation:
house<-housing3[(1:24)*2,]
house$price2<-sqrt(abs(residuals(model0)))[(1:24)*2]
vmod<-lm(price2~bath+pCovered, house)
anova(vmod)
# The p-values are above 0.05, which confirms that there are no strong evidence 
# against constant variation among f.bath and pCovered.

# 3) Test for correlation in errors
n<-length(residuals(model0))
plot(tail(residuals(model0),n-1)~head(residuals(model0),n-1), xlab="epsilon(i)",
     ylab="epsilon(i+1)")
abline(h=0, v=0, col=grey(0.75))
library(lmtest)
dwtest(price~bath+pCovered, data=housing3)
# the high p-value indicates that there is not enough evidence to reject H0: 
# the errors are uncorrelated.

#------------------------#
# Final model selection. #
#------------------------#
# Both models passed the assumption check (except for normality of residuals that 
# has been failed fue to lighter tails). Based on the Radj the model4 slightly better 
# explains the variability in dependent variable (0.8619 VS 0.8595). However,
# there is a downside in using this model. Since the number of bath set up as a factor
# the model prediction capacity is limitted to the housing with up to 4 bathrooms.
# While this was the case in our development sample we don't know if the same applies 
# to the target data for which this model was developed.Thus, if the advertised house
# has 5 bathrooms model4 would fail to estimate its sale price. At the same time,
# model0 doesn't have this limitation and can be applied to houses with any number 
# of bath.
# Thus while having slightly lower R-squared model0 is preffered model for house 
# price estimation.
# Final model: lm(price~bath+pCovered, housing3)
