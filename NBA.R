#####################################
###     Project Assignment ML2    ###
###   NBA basketball and rookies  ###
###           2426557N            ###
#####################################

# Set-up libraries
library(ggplot2)
library(gridExtra)
#library("GGally")
library(corrplot)
library(dplyr)

library(glmnet)
library(caret)
set.seed(523)


###==============================###
### 1. Extract data ####
###==============================###
# load the data
data<-read.csv("Data/nba.csv")
head(data)


###==============================###
### 2. Exploratory Analysis ####
###==============================###
summary(data)
# there is no missing values and the values are mostly reasonable.
# There are potentially few outliers (such as REB=12.1, AST=10.6, etc.)

# check duplicated names
data[duplicated(data$Name),]
data[data$Name %in% c("Mark Davis","Charles Smith","Dee Brown")]

# drop the index column
# drop names as it can't be used for prediction
# remove variable "Yrs" from data as it was used to derive target variable
names <- data[,2]
yrs <- data[,23]
data2 <- data[,-c(1,2,23)]
head(data2)
str(data2)

### Check for outliers
plots <- list()
for (i in 1:length(names(data2[,!names(data2)=="Target"]))){
  p <- as.data.frame(cbind(Year_drafted=data2Year_drafted,y=data2[,i]))
  # points grid plot
  plots[[i]] <- ggplot(data=p, aes(x=Year_drafted,y=y)) + geom_point(size=0.7) + 
    ylab(names(data2)[i]) + xlab("Year")
  # histogram grid plot
  # plots[[i]] <- ggplot(data=p, aes(x=y)) + geom_histogram() +
  #   xlab(names(data2)[i])
}
do.call(grid.arrange,plots)
remove(p)


# check correlation between variables
pairs(data2[,10:20],col=c("red","purple")[target+1])
#ggpairs(data[,10:20])

# Drop insignificant correlations and diagonal
corr <- cor(data2[,-21])
corr[lower.tri(corr,diag=TRUE)] <- NA 
corr <- as.data.frame(as.table(corr))
corr <- na.omit(corr)
#select significant values  
corr <- subset(corr, abs(Freq) > 0.5) 
#sort by highest correlation
corr <- corr[order(-abs(corr$Freq)),]
#print table
print(corr)
#turn corr back into matrix in order to plot with corrplot
mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")

#plot correlations visually
corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ")
# Highly correlated groups of variables (>0.8):
# MIN, PTS, FG_made, FGA, FT_made, FTA, TOV
# TP_made, TPA
# OREB, DREB, REB
corrplot(cor(data2[,!colnames(data2) %in% c("FT_percent", "TP_made", "Target", "FG_percent","Year_drafted")], 
             yrs),tl.cex=0.6, cl.cex=0.6, pch.cex=0.6, method="pie")
# Selcet the one with max corr with target var
# MIN
# TPA
# REB


###==============================###
### 3. Model fit ####

### Split train - test
# 70/30 split on train/test
sample_size <- floor(0.7*nrow(data2))
training_index<-sample(seq_len(nrow(data2)), size=sample_size)

train<-data2[training_index,]
test<-data2[-training_index,]

### Standardise data
train[,1:(ncol(train)-1)] <- apply(train[,1:(ncol(train)-1)],2,scale)
test[,1:(ncol(test)-1)] <- apply(test[,1:(ncol(test)-1)],2,scale)

n_train <- nrow(train)
n_test <- nrow(test)

x.train <- train[,-ncol(train)]
y.train <- train$Target
x.test<- test[,-ncol(train)]
y.test <- test$Target

#### 3.1 Lasso ####
# use family=binomial for logistic regression
mod.l = glmnet(as.matrix(x.train),y.train,family='binomial',alpha=1)
# plot coefficients against log-lambda
plot(mod.l,xvar='lambda',label=T)
#legend("topright", lwd = 1, col = 1:6, legend = colnames(x.train), cex = .7)
plot(mod.l,xvar='dev',label=T)

# find the best lambda with cross-validation
cv.lasso <- cv.glmnet(as.matrix(x.train),y.train,family='binomial',alpha=1)
plot(cv.lasso)
# first vertical line - min of MSE, second line - one standard error of MSE
cv.lasso$lambda.min
cv.lasso$lambda.1se

# take min MSE lambda
mod.l.1se <- glmnet(as.matrix(x.train),y.train,family='binomial',alpha=1, lambda=cv.lasso$lambda.1se)
coef(mod.l.1se)

#### 3.2 Ridge ####
mod.r = glmnet(as.matrix(x.train),y.train,family='binomial',alpha=0) 
# plot coefficients against log-lambda
plot(mod.r,xvar='lambda',label=T)
#legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x.train), cex = .7)
plot(mod.r,xvar='dev',label=T)

# find the best lambda with cross-validation
cv.ridge <- cv.glmnet(as.matrix(x.train),y.train,family='binomial',alpha=0)
plot(cv.ridge)
# first vertical line - min of MSE, second line - one standard error of MSE
cv.ridge$lambda.min
cv.ridge$lambda.1se

# calculate final model with selected lambda
mod.r.1se <- glmnet(as.matrix(x.train),y.train,family='binomial',alpha=0, lambda=cv.ridge$lambda.1se)
coef(mod.r.1se)

#### 3.3 Elastic Net ####
mod.en <- train(
  as.factor(Target)~., data=train,family='binomial',method='glmnet',
  trControl=trainControl("cv",number=10),
  tuneLength=10
)
# print best combination
mod.en$results
mod.en$bestTune
# print coefficients
coef(mod.en$finalModel,mod.en$bestTune$lambda)

# plot alpha and lambda against Accuracy
ggplot(data=mod.en$results) + 
  geom_line(aes(x=lambda, y=Accuracy, group=alpha, col=as.factor(alpha))) + 
  labs(color='alpha') 

mod.en.f <- glmnet(as.matrix(x.train),y.train,family='binomial',alpha=mod.en$bestTune$alpha, lambda=mod.en$bestTune$lambda)
coef(mod.en.f)



# use cross validation to select lambda
cv.net <- cv.glmnet(as.matrix(x.train),y.train,type.measure='deviance',
                    alpha=mod.en$bestTune$alpha, family='binomial')
plot(cv.net)
cv.net$lambda.min
cv.net$lambda.1se



###==============================###
### 4. Accuracy ####
###==============================###

# Test the model Lasso
test.l <- mod.l.1se %>% predict(as.matrix(x.test), type='response') %>% as.vector()
summary.l <- assess.glmnet(mod.l.1se,newx = as.matrix(x.test), newy=y.test, family = "binomial")
results.l <- data.frame(
  model = 'lasso',
  Deviance = summary.l$deviance,
  AUC = summary.l$auc,
  Accuracy = mean(ifelse(test.l>0.5,1,0)==y.test)
)
table(pred=ifelse(test.l>0.5,1,0),true=y.test)

plot(roc.glmnet(mod.l.1se, newx = as.matrix(x.test), newy=y.test))


# Test the model Ridge
test.r <- mod.r.1se %>% predict(as.matrix(x.test), type='response') %>% as.vector()
summary.r <- assess.glmnet(mod.r.1se,newx = as.matrix(x.test), newy=y.test, family = "binomial")
results.r <- data.frame(
  model = 'ridge',
  Deviance = summary.r$deviance,
  AUC = summary.r$auc,
  Accuracy = mean(ifelse(test.r>0.5,1,0)==y.test)
)
table(pred=ifelse(test.r>0.5,1,0),true=y.test)

plot(roc.glmnet(mod.r.1se, newx = as.matrix(x.test), newy=y.test))

# Test the model El Net
test.en <- mod.en.f %>% predict(as.matrix(x.test), type='response') %>% as.vector()
summary.en <- assess.glmnet(mod.en.f, newx = as.matrix(x.test), newy=y.test, family = "binomial")
results.en <- data.frame(
  model = 'el.net',
  Deviance = summary.en$deviance,
  AUC = summary.en$auc,
  Accuracy = mean(ifelse(test.en>0.5,1,0)==y.test)
)
table(pred=ifelse(test.en>0.5,1,0),true=y.test)

plot(roc.glmnet(mod.en.f, newx = as.matrix(x.test), newy=y.test))


results <- c()
results <- rbind(results.r,results.l,results.en)
results

# Coefficients
plot(coef(mod.en$finalModel,mod.en$bestTune$lambda)[-1], 
     col="red", xaxt="n", xlab="", ylab="Coefficients", ylim=c(0,0.4))
points(coef(mod.l.1se)[-1], col="blue")
points(coef(mod.r.1se)[-1], col="green")
axis(1,at=1:20,labels=colnames(train[,-21]),lwd=0.5,las=2, cex.axis=0.8)
legend(x=17, y=0.35, legend=c("el.net","lasso","ridge"),
       pch=1, col=c("red","blue","green"))

