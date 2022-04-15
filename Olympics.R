library(ggplot2)
library(GGally)
library(gridExtra)

#==============================================================================#
##########                        1. Data exploration                 ##########
#==============================================================================#

#=======================================#
####  1.1 Download data              ####
#=======================================#
oldat <- read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/rioolympics.csv"))
summary(oldat)
str(oldat)

# specify missing values
oldat <- read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/rioolympics.csv"),
                  na.strings = "#N/A") 
head(oldat,1)
oldat$muslim <- ifelse(oldat$muslim==2,1,oldat$muslim)

#=======================================#
#### 1.2 Replace Missing values      ####
#=======================================#
summary(oldat)
str(oldat)
#
### GDP
#
# there are few variables with missing values: gdp00 (1), gdp16(2), bmi (27)
oldat[is.na(oldat$gdp00)==TRUE,]
oldat[is.na(oldat$gdp16)==TRUE,]
oldat[is.na(oldat$bmi)==TRUE,]
# we can't exclude those observations since we need to have a prediction for 
# them for 2016 Olympics. Another available options are to replace missings
# with an average value for that variable, approximate it with linear regression
# or take an average betwenn points just before and after the missing obs.

gdp01.dat <- data.frame(matrix(c(oldat[1,"gdp00"], oldat[1,"gdp04"], oldat[1,"gdp08"],
                                 oldat[1,"gdp12"], oldat[1,"gdp16"], oldat[1,"pop00"],
                                 oldat[1,"pop04"], oldat[1,"pop08"], oldat[1,"pop12"],
                                 oldat[1,"pop16"]), ncol=2), 
                        row.names = c("00","04","08","12","16"))
plot(X1~X2, data=gdp01.dat, xlab="Population, thousands", ylab="GDP, millions USD", 
     main="Afghanistan, 2000", ylim=c(0,20000))
abline(v=gdp01.dat$X2[1], col="blue", lty=2)
abline(lm(X1~X2, data=gdp01.dat), col="red")
# The linear approxiation will produce too low GDP values

# For gdp, since missing observations lie on the end points, most reliable option
# will be to assume that gdp per capita will remain the same as in the previous/
# following years
oldat.c <- oldat

oldat.c[1,"gdp00"] <- oldat.c[1,"pop00"]*(oldat.c[1,"gdp04"]/oldat.c[1,"pop04"])
oldat.c[21,"gdp16"] <- oldat.c[21,"pop16"]*(oldat.c[21,"gdp12"]/oldat.c[21,"pop12"])
oldat.c[92,"gdp16"] <- oldat.c[92,"pop16"]*(oldat.c[92,"gdp12"]/oldat.c[92,"pop12"])

#
### BMI
#
# Given a large number of missing values a simple average approximation has been selected
avg.bmi <- mean(oldat.c[is.na(oldat.c$bmi)==FALSE,"bmi"])
oldat.c[is.na(oldat.c$bmi)==TRUE, "bmi"] <- avg.bmi
plot(oldat.c[,"bmi"], xlab="", ylab="", main="BMI")

#=======================================#
#### 1.3 Bring data to long format   ####
#=======================================#
library(tidyr)

wide_long <- function(cname){
  c.columns <- c("country", paste(cname,"00",sep=""),paste(cname,"04",sep=""),
                 paste(cname,"08",sep=""),paste(cname,"12",sep=""),
                 paste(cname,"16",sep=""))
  wide <- oldat.c[,c.columns]
  df <- gather(data=wide, Year, var, c(paste(cname,"00",sep=""),paste(cname,"04",sep=""),
                                       paste(cname,"08",sep=""),paste(cname,"12",sep=""),
                                       paste(cname,"16",sep="")))
  df$Year <- as.numeric(paste("20",substr(df$Year, nchar(cname)+1, nchar(cname)+2),sep=""))
  colnames(df)[3] <- cname
  return(df)
}
gdp <- wide_long("gdp")
pop <- wide_long("pop")
gold <- wide_long("gold")
tot <- wide_long("tot")
totgold <- wide_long("totgold")
totmedals <- wide_long("totmedals")
athletes <- wide_long("athletes")

# merge into one long dataset
long <- merge(merge(merge(merge(merge(merge(gdp,pop,by=c("country","Year")), 
                                      gold,by=c("country","Year")),tot,by=c("country","Year")),
                          totgold, by=c("country","Year")), 
                    totmedals, by=c("country","Year")), 
              athletes, by=c("country","Year"))
oldat.l <- merge(oldat.c[,c("country", "country.code", "soviet", "comm", "muslim",
                            "oneparty", "bmi", "altitude", "host")],long, by="country")

# Plot approximated GDP values
ggplot(oldat.l[c(1:5,101:105,456:460),], aes(x=pop, y=gdp, colour=country))+geom_point()
ggplot(oldat.l[c(1:5,101:105,456:460),], aes(x=pop, y=gdp))+geom_point(aes(colour=country))+
  geom_point(data=oldat.l[c(1,105,460),], aes(x=pop, y=gdp), colour="red", size=3, pch=1) +
  xlab("Population, thousands") + ylab("GDP, millions USD")

# convert to factors
oldat.l$soviet <- as.factor(oldat.l$soviet)
oldat.l$comm <- as.factor(oldat.l$comm)
oldat.l$muslim <- as.factor(oldat.l$muslim)
oldat.l$oneparty <- as.factor(oldat.l$oneparty)
oldat.l$host <- as.factor(oldat.l$host)
oldat.l <- oldat.l[with(oldat.l,order(oldat.l$country, oldat.l$Year)),]

str(oldat.l)
dim(oldat.l)

#=======================================#
#### 1.4 Additional Variables        ####
#=======================================#
# There are some variables which should not be allowed to be predictors since 
# they carry information about the outcome, such as number of gold medals
# won by country for each year. 
# However, we can use this information from previous years.
# We will create 4 additional variables: prev.gold, prev.totgold,
# prev.tot, prev.totmedals

# The 1996 data has been taken from below website:
# https://web.archive.org/web/20111105151510/http://www.databaseolympics.com/games/gamesyear.htm?g=24
oldat1996 <- read.csv("C:/Users/Viktoria/Documents/Glasgow/Advanced Predictive Modelling/Project/Oldata_1996.csv")

for(i in 1:nrow(oldat.l)){
    if(oldat.l$Year[i]!=2000){
      oldat.l$prev.gold[i] = oldat.l$gold[i-1]
      oldat.l$prev.totgold[i] = oldat.l$totgold[i-1]
      oldat.l$prev.tot[i] = oldat.l$tot[i-1]
      oldat.l$prev.totmedals[i] = oldat.l$totmedals[i-1]
    }else{
      for(k in 1:nrow(oldat1996)){
        if(oldat.l$country[i]==oldat1996$Country[k]){
          oldat.l$prev.gold[i] = oldat1996$gold[k]
          oldat.l$prev.tot[i] = oldat1996$tot[k]
        }
      }
      oldat.l$prev.totgold[i] = sum(oldat1996$gold)
      oldat.l$prev.totmedals[i] = sum(oldat1996$tot)
    }
}


#=======================================#
#### 1.5 Split into train and test   ####
#=======================================#
train.dat <- oldat.l[oldat.l$Year<=2012,]
test.dat <- oldat.l[oldat.l$Year==2016,]


#=======================================#
#### 1.6 Data exploration            ####
#=======================================#
library(car)
outlierTest(lm(tot~bmi, data=train.dat))
outlierTest(lm(tot~altitude, data=train.dat))
outlierTest(lm(tot~gdp, data=train.dat))
train.dat[rownames(train.dat) %in% c(518,517,516,396,397,88,21),]

boxplot(train.dat$gdp, xlab="GDP")
boxplot(train.dat$tot, xlab="Total medals")
# Given the big difference in wealth, population, etc. a lot of real data may appear
# as outliers. Therefore no outlier removal/replacement will be performed

plot(tot~bmi, data=train.dat)
plot(tot~altitude, data=train.dat)
plot(tot~host, data=train.dat)
plot(tot~gdp, data=train.dat)
plot(log(tot)~log(gdp), data=train.dat)
plot(tot~pop, data=train.dat)
plot(log(tot)~log(pop), data=train.dat)
plot(tot~athletes, data=train.dat)
plot(log(tot)~log(athletes), data=train.dat)
plot(tot~totgold, data=train.dat)
plot(tot~totmedals, data=train.dat)
plot(tot~Year, data=train.dat)
plot(tot~prev.gold, data=train.dat)
plot(tot~prev.totgold, data=train.dat)
plot(tot~prev.totmedals, data=train.dat[train.dat$Year==2012,])
plot(tot~prev.tot, data=train.dat)

ggpairs(train.dat[train.dat$Year==2012,c(-1,-2,-3,-4,-5,-6,-9,-10,-13,-15,-16,-19,-21)], 
        upper=list(continuous=wrap("points", alpha=0.3, color="#d73027")), 
        lower="blank", axisLabels="none")
library(corrplot)
corrplot(cor(train.dat[,c(-1,-2,-3,-4,-5,-6,-9)]), method="color", type="upper", tl.pos="d", tl.cex = 0.8)

p1 <- ggplot(train.dat[train.dat$Year==2012,c(-1,-2)], aes(x=soviet, y=log(tot)))+
  geom_boxplot(fill=c("blue", "orange")) + ylab("log(total medals)") 
p2 <- ggplot(train.dat[train.dat$Year==2012,c(-1,-2)], aes(x=comm, y=log(tot)))+
  geom_boxplot(fill=c("blue", "orange")) + ylab("log(total medals)") 
p3 <- ggplot(train.dat[train.dat$Year==2012,c(-1,-2)], aes(x=muslim, y=log(tot)))+
  geom_boxplot(fill=c("blue", "orange")) + ylab("log(total medals)") 
p4 <- ggplot(train.dat[train.dat$Year==2012,c(-1,-2)], aes(x=oneparty, y=log(tot)))+
  geom_boxplot(fill=c("blue", "orange")) + ylab("log(total medals)") 
grid.arrange(p1,p2,p3,p4,nrow=2)

# Exclude gold from the dataset as it contains future information
train.dat <- train.dat[,-13]

#==============================================================================#
##########                        2. Analysis                         ##########
#==============================================================================#

#=======================================#
####  2.1 Linear Regression          ####
#=======================================#
library(MASS)
library(caret)
mod0 <- lm(tot~., data=train.dat[,c(-1,-2)])
summary(mod0)
stepAIC(mod0)

mod0 <- lm(formula = tot ~ soviet + oneparty + gdp + pop + totgold + 
             athletes + prev.gold + prev.tot, data = train.dat)
# mod0 <- lm(formula = tot ~ soviet + oneparty + log(gdp) + log(pop) + 
#             athletes + prev.gold + prev.tot, data = train.dat)
# mod0 <- lm(formula = tot ~ soviet + oneparty + log(gdp) + log(pop) + 
#             log(athletes) + prev.tot, data = train.dat[train.dat$athletes>0, c(-1,-2)])
# mod0 <- lm(formula = tot ~ soviet + oneparty + scale(gdp) + scale(pop) +
#             scale(athletes) + prev.tot, data = train.dat[, c(-1,-2)])
summary(mod0)
RMSE(fitted(mod0),train.dat$tot)
MAE(fitted(mod0),train.dat$tot)
AIC(mod0)
par(mfrow=c(2,2))
plot(mod0)
par(mfrow=c(1,1))


#=======================================#
####  2.2 Binomial model             ####
#=======================================#

train.bin <- cbind(train.dat$tot, train.dat$totmedals-train.dat$tot)

# GLM binomila logit link W3
mod1 <- glm(train.bin~ .,family=binomial, data=train.dat[,c(-1,-2,-13)])
summary(mod1)

qchisq(0.95, mod1$df.residual)
# the deviance is too high
# Stepwise variable selection:
stepAIC(mod1)

# Fit new model suggested using AIC
mod1 <- glm(formula = train.bin ~ soviet + comm + muslim + oneparty + 
               bmi + altitude + host + Year + gdp + pop + totgold + totmedals + athletes + 
               prev.gold + prev.tot, family = binomial(link="logit"), data = train.dat[,c(-1, -2)])
summary(mod1)
qchisq(0.95, mod1$df.residual)

anova(mod1)

library(sjPlot)
plot_model(mod1, prefix.labels="label", show.values=TRUE,
           show.p=FALSE, value.offset=0.45)


RMSE(fitted(mod1),train.dat$tot)
MAE(fitted(mod1),train.dat$tot)
AIC(mod1)
par(mfrow=c(2,2))
plot(mod1)
par(mfrow=c(1,1))


### Binomial with probit link
# mod1 <- glm(train.bin~.,family=binomial(link="probit"), data=train.dat[,c(-1,-2,-13)])
# summary(mod1)
# stepAIC(mod1)
# Deviance is slightly lower but not significantly
### Binomial with probit log-log link
# mod1 <- glm(train.bin~.,family=binomial(link="cloglog"), data=train.dat[,c(-1,-2,-13)])
# summary(mod1)
# stepAIC(mod1)
# Deviance is higher

#=======================================#
####  2.3 Poisson model              ####
#=======================================#

mod2 <- glm(tot~., family = poisson, data = train.dat[,c(-1,-2)])
summary(mod2)

stepAIC(mod2)
mod2 <- glm(formula = tot ~ soviet + comm + muslim + oneparty + bmi + 
              altitude + host + Year + gdp + pop + totgold + totmedals + 
              athletes + prev.gold + prev.tot, family = poisson, data = train.dat[,c(-1, -2)])
summary(mod2)
qchisq(0.95, mod2$df.residual)
# The deviance is large
RMSE(fitted(mod2),train.dat$tot)
MAE(fitted(mod2),train.dat$tot)
AIC(mod2)
par(mfrow=c(2,2))
plot(mod2)
par(mfrow=c(1,1))

# Check residual plots

resp <- resid(mod2, type = "pearson") 
resd <- resid(mod2, type = "deviance")
p1<- ggplot(mod2, aes(sample = resp)) + geom_point(stat = "qq", color = "#7fc97f") + 
  ylab("Pearson residuals") 
p2<- ggplot(mod2, aes(sample = resd)) + geom_point(stat = "qq", color = "#7fc97f") + 
  ylab("Deviance residuals") 
p3<- ggplot(mod2, aes(x = predict(mod2, type="link"), y =resd))+ 
  geom_point(col = "#7fc97f") + ylab("Deviance residuals") + xlab("Linear predictor") 
grid.arrange(p1, p2, p3, nrow = 1)

# Check for overdispersion
ggplot(mod2, aes(x=fitted(mod2), y=(train.dat$tot-fitted(mod2))^2))+ 
  geom_point(col="#f46d43") + geom_abline(slope=1, intercept=0, col="#a6d96a", size=1) + ylab(expression((y-hat(mu))^2)) + xlab(expression(hat(mu)))
# Many points lie above the equality line - sign of overdispersion.


#=======================================#
####  2.4 Quasi-Poisson model        ####
#=======================================#
X2 <- sum(resid(mod2, type = "pearson")^2) 
dp <- X2 / mod2$df.res 
dp
summary(mod2, dispersion = dp)
# Wald tests are not very reliable, so we turn to an F test
# to determine the signiﬁcance of the regression coefﬁcients: 
drop1(mod2, test = "F")
# Residual plots vs. predicted 
pred <- predict(mod1, type = "response") 
stand.resid <- rstandard(model = mod1, type = "pearson") 
# Standardised Pearson residuals
par(mfrow=c(1,2)) 
plot(x = pred, y = stand.resid, xlab = "Predicted count", 
     ylab = "Standardised Pearson residuals", main = "Regular likelihood", 
     ylim = c(-5,5)) 
abline(h = c(-3, -2, 0, 2, 3), lty = "dotted", col = "red")

mod3 <- glm(formula = tot ~ soviet + comm + muslim + bmi + 
              host + pop + athletes + prev.tot,
              family = quasipoisson(link = "log"), data = train.dat[,c(-1, -2)])
drop1(mod3, test = "F")

pred <- predict(mod3, type = "response") 
stand.resid <- rstandard(model = mod3, type = "pearson") # Standardised Pearson residuals
plot(x = pred, y = stand.resid, xlab = "Predicted count", 
     ylab = "Standardised Pearson residuals", main = "Quasi-likelihood", 
     ylim = c(-5,5)) 
abline(h = c(-3, -2, 0, 2, 3), lty = "dotted", col = "red")
summary(mod3)

valid.qpoi <- cbind(valid.dat[,c("country","tot","gdp")], 
                    expected=round(predict(mod2, newdata = valid.dat, type='response'),2))
plot(valid.qpoi$tot,valid.qpoi$expected,xlab="actual", ylab="predicted",
     main="Quasi Poisson")
abline(a=0,b=1, col="red")
par(mfrow=c(1,1)) 

qchisq(0.95, mod3$df.residual)
# The deviance is large
RMSE(fitted(mod3),train.dat$tot)
MAE(fitted(mod3),train.dat$tot)
# AIC(mod3)
par(mfrow=c(2,2))
plot(mod3)
par(mfrow=c(1,1))


#=======================================#
####  2.5 Negative Binomial model    ####
#=======================================#
library(MASS)
mod4 <- glm.nb(formula = tot ~ soviet + comm + muslim + oneparty + bmi + 
                 altitude + host + Year + gdp + pop + totgold + totmedals + 
                 athletes + prev.gold + prev.tot,data=train.dat)
summary(mod4)

# compare the Poisson and negative binomial models by looking at 
# their deviances and AIC scores: 
# Poisson model
c(mod2$deviance, mod2$aic)
# Negative Binomial
c(mod4$deviance, mod4$aic)
# both are much smaller for NB

# To chose QP vs NB plot (y-mu)^2 vs mu
# plot linear(QP) and quadratic(NB) to see which one is better
# Plot of squared residuals v predicted values 
res.sq <- residuals(mod2, type = "response")^2 
set1 <- data.frame(res.sq, mu.hat = mod2$fitted.values)

fit.lin <- lm(formula = res.sq ~ mu.hat, data = set1) 
fit.quad <- lm(formula = res.sq ~ mu.hat + I(mu.hat^2), data = set1) 
summary(fit.quad)

plot(set1$mu.hat, y = set1$res.sq, xlab = "Predicted count", 
     ylab = "Squared Residual") 
curve(expr = predict(fit.lin, newdata = data.frame(mu.hat = x), 
                     type = "response"), col = "blue", add = TRUE, lty = "solid") 
curve(expr = predict(fit.quad, newdata = data.frame(mu.hat = x), 
                     type = "response"), col = "red", add = TRUE, lty = "dashed") 
legend("topleft", legend = c("Linear", "Quadratic"), col = c("red", "blue"), lty = c("solid", "dashed"), bty = "n")
# The quadratic coefficient is significant (p-value <0.05) there are evidence
# that NB fit better

RMSE(fitted(mod4),train.dat$tot)
MAE(fitted(mod4),train.dat$tot)
AIC(mod4)
par(mfrow=c(2,2))
plot(mod4)
par(mfrow=c(1,1))


# Variables reduction and transformation of variales doesn't improve 
#  residuals distribution
# mod4 <- glm.nb(formula = tot ~ comm + bmi + altitude + log(gdp) + pop + 
#                  sqrt(athletes) + sqrt(prev.tot),data=train.dat)
# plot(mod4)


#=======================================#
####  2.6 Linear Mixed model         ####
#=======================================#
library(lme4)
# We have multiple observations per country and it would be reasonable 
# to expect that they are correlated
ggplot(train.dat, aes(x=Year, y=tot))+ geom_point(alpha=0.9, color="#7a0177") + 
  geom_path( aes(group=country), alpha=0.3, col="#ae017e")

mod5 <- lmer(tot ~ soviet + comm + muslim + oneparty + bmi + scale(gdp) + scale(pop) +  
               altitude + host + Year + totgold + totmedals + athletes + 
               prev.gold + prev.tot + (1|country) + (1|athletes),
             data=train.dat)
summary(mod5)
# Calculate CI since model doesn't have p-values
round(confint(mod5,method="boot",oldNames=FALSE),3)
# Used log transformation for gdp and pop to bring them to the same scale
# both were removed as insignificant
# mod5 <- lmer(tot ~ athletes + oneparty + prev.gold + prev.tot + 
#                scale(gdp) + (1|country) + (1|oneparty:country),
#              data=train.dat)
mod5 <- lmer(tot ~ scale(athletes) + oneparty + prev.gold + prev.tot + 
               scale(gdp) + (1|country),
             data=train.dat)
round(confint(mod5,method="boot",oldNames=FALSE),3)
summary(mod5)


RMSE(fitted(mod5),train.dat$tot)
MAE(fitted(mod5),train.dat$tot)
AIC(mod5)
par(mfrow=c(1,2)) 
qqnorm(resid(mod5), main="") 
qqline(resid(mod5))  
plot(fitted(mod5), resid(mod5), xlab="Fitted", ylab="Residuals") 
abline(0,0)
par(mfrow=c(1,1))

# Add random slope
mod6 <- lmer(tot ~ athletes + oneparty + prev.gold + prev.tot + 
               scale(gdp) + (1|country) + (0+Year|country),
             data=train.dat)
round(confint(mod5,method="boot",oldNames=FALSE),3)
summary(mod5)


mod7 <- glmer(tot~1+scale(athletes) + oneparty + prev.gold + prev.tot + 
                scale(gdp) + (1|country),
              data=train.dat, family=poisson)
RMSE(fitted(mod7),train.dat$tot)
MAE(fitted(mod7),train.dat$tot)
AIC(mod7)
par(mfrow=c(1,2)) 
qqnorm(resid(mod7), main="") 
qqline(resid(mod7)) 
plot(fitted(mod7), resid(mod7), xlab="Fitted", ylab="Residuals") 
abline(0,0)
par(mfrow=c(1,1))
