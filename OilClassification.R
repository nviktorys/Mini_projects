# 1. Download ' format data #
#===========================#
setwd("C:/Users/Viktoria/Documents/Glasgow/Data Mining and Machine Learning 1/Project")
load("oliveoils.RData")
head(oliveoils)
# Specify seed for reproductivity
seed <- 13
set.seed(seed)

# merge on class column and select assigned observations
a <- as.matrix(oliveoillabels)
colnames(a) <- c("class")

olive <- as.data.frame(cbind(a, as.data.frame(oliveoils[,c(525:625)])))
rownames(olive)<-c()
str(olive)
summary(olive)
# There are no missing observations, all values have correct format

# 2. Split data on train/validate/test samples #
#==============================================#
# Apply 50/25/25 split by class to ensure we have enough observations from each

crete <- olive[olive$class=="Crete",]
other <- olive[olive$class=="Other",]
pelop <- olive[olive$class=="Peloponese",]

ds.split <- function(data){
  set.seed(seed)
  n <- nrow(data)
  ind1 <- sample(c(1:n), round(n/2))
  ind2 <- sample(c(1:n)[-ind1], round(n/4))
  ind3 <- setdiff(c(1:n), c(ind1,ind2))
  return(list("ind1"=ind1, "ind2"=ind2, "ind3"=ind3))
}
crete.ind <- ds.split(crete)
other.ind <- ds.split(other)
pelop.ind <- ds.split(pelop)
train.data<-rbind(crete[crete.ind$ind1,], other[other.ind$ind1,], pelop[pelop.ind$ind1,])
valid.data<-rbind(crete[crete.ind$ind2,], other[other.ind$ind2,], pelop[pelop.ind$ind2,])
test.data<-rbind(crete[crete.ind$ind3,], other[other.ind$ind3,], pelop[pelop.ind$ind3,])
dim(train.data)
dim(valid.data)
dim(test.data)
cbind("training"=table(train.data[,1]), 
      "validation"=table(valid.data[,1]), 
      "test"=table(test.data[,1]))

# 3. Explaratory analysis #
#=========================#
library(skimr)
library(knitr)
kable(skim(train.data))

# we can see that the standard deviation is quite different between the variables. 
# There seem to be no outliers, but it is hard to see from the table.


# Check for outliers
#-------------------
library(grDevices)
outlier<-c()
outlier2 <- c()
check.out <- function(data){
  outlier<-c()
  outlier2 <- c()
  for(i in c(2:102)){
    if(length(boxplot.stats(data[,i])$out)>0){
      outlier[i-1] <- i
      print(boxplot.stats(data[,i])$out)
      print(which(data[,i] %in% boxplot.stats(data[,i])$out))
      outlier2 <- unique(c(outlier2, which(data[,i] %in% 
                                             boxplot.stats(data[,i])$out)))
    }
    else{
      outlier[i-1] <- 0
    }
  }
  outlier <<- outlier
  outlier2 <<- outlier2
  ifelse(length(which(outlier>0))>0,
         boxplot(data[,c(outlier[which(outlier>0)])], 
          xlab="spectra", ylab="Measurement"),
         print("No outliers detected"))
}
check.out(train.data)

outlier2
length(which(outlier>0))
# The test using boxplot identifies points that lie outside of 1.5*IQR
# which can be defined as outliers. The test revield 36 variables to have 
# outliers which can be seen on the boxplot. The outliers seem to come
# from the same observations that gives us confidence that there are indeed 
# outlier values that can be removed.

# Remove all outliers 
train.data2 <- train.data[-c(which(outlier2>0)),]
dim(train.data2)
table(train.data2[,1])

# check if there are more outliers remain
check.out(train.data2)
train.data[c(which(outlier2>0)),1]

# by removing extra outliers we eould further reduce observations
# for Crete, therefore no more outlier removal will be performed.

# Check correlation
#-------------------
cor(train.data2[,-1])
library(corrplot)
corrplot(cor(train.data2[,-1]), method="color", type="upper", tl.pos="d", tl.cex = 0.5)
# correlation tables shows very strong correlation between a number of variables
# (0.90+). Having strong correlation suggest that we can reduce dimencionality
# in data with PCA method.

# PCA
#-----
# We have seen in the beginning of analysis that variables have quite different
# variances, which suggest to use normalized values for PCA.
# We will define in advance that we want to retain at least 90% of variation.
plot(diag(var(train.data2[-1])), xaxt='n', xlab="spectra", ylab="Variance")
axis(1, at=1:ncol(train.data2[,-1]), labels=colnames(train.data2[,-1]))
max(round(diag(var(train.data2[-1])),2)) # maximum variance
min(round(diag(var(train.data2[-1])),2)) # minimum variance

# use prcomp function since we have more variables than observations
olive.pca <- prcomp(train.data2[-1],scale=T,retx=T)
olive.pca
olive.pca$sdev
varEx<-as.data.frame(olive.pca$sdev^2/sum(olive.pca$sdev^2))
varEx<-cbind(c(1:nrow(train.data2)),cumsum(varEx[,1]))
varEx
# standard deviation of the first two PC seem to be large comparing with the others
summary(olive.pca)
plot(olive.pca, xlab="Principal components", main=NULL)
# based on the summary we can conclude that to retain 90% of variation we need
# to select first two PCs.
olive.pca$rotation[,c(1,2)]
biplot(olive.pca, xlim=c(-0.4, 0.4), xlabs=rep("*",nrow(train.data2)))
# from biplot chart we can see that there is a whole range of correlation between variables
# from 0 (90 degrees angle) to nearly 1 (0 degrees). 
# We can also observe that loadings for all variables on PC1 are positive.

# Create new datasets with PCs as new explanatory variables (scale validation and 
# test datasets)
olive.pca$x[,c(1,2)]
train.pca <- data.frame(as.factor(train.data2$class),olive.pca$x[,c(1,2)])
colnames(train.pca)<-c("class", "PC1","PC2")
head(train.pca)


valid.pca <- data.frame(as.factor(valid.data$class),
                        predict(olive.pca,valid.data[,-1])[,c(1,2)])
colnames(valid.pca)<-c("class", "PC1","PC2")
head(valid.pca)

test.pca <- data.frame(as.factor(test.data$class),
                       predict(olive.pca,test.data[,-1])[,c(1,2)])
colnames(test.pca)<-c("class", "PC1","PC2")
head(test.pca)


# 4. Apply classification methods #
#=================================#

# 1) knn
#--------
library(class)
library(ggplot2)
ggplot(train.pca, aes(PC1,PC2,colour=class, fill=class))+
  geom_point(size=3)
pred.class <- numeric(nrow(valid.pca))
corr.class.rate<-numeric(25)
for(k in 1:25)
{
  pred.class<-knn(train.pca[,-1], valid.pca[,-1], train.pca[,1], k=k)
  corr.class.rate[k]<-sum((pred.class==valid.pca[,1]))/length(pred.class)
}

plot(c(1:25),corr.class.rate,type="l",
     main="Correct Classification Rates for the Test Data for a range of k",
     xlab="k",ylab="Correct Classification Rate",cex.main=0.7)


which.max(corr.class.rate)

valid.knn<-knn(train.pca[,-1], valid.pca[,-1], train.pca[,1], 
               k=which.max(corr.class.rate))
sum((valid.knn==valid.pca[,1]))/length(valid.knn)

table(valid.pca[,1],valid.knn)

# plot actuals vs predicted
library(plyr)
plot.df <- data.frame(valid.pca, predicted = valid.knn)
plot.df1 <- data.frame(x = plot.df$PC1, 
                       y = plot.df$PC2, 
                       predicted = plot.df$predicted)

find_hull = function(df) df[chull(df$x, df$y), ]
boundary = ddply(plot.df1, .variables = "predicted", .fun = find_hull)
colnames(plot.df)<- c("class","PC1","PC2","predicted")

ggplot(plot.df, aes(PC1, PC2, color=class, fill=class))+
  geom_point(size=6) +
  geom_point(aes(PC1, PC2, color=predicted, fill=predicted),size=3)


ggplot(plot.df, aes(PC1, PC2, color=predicted, fill=predicted))+
  geom_point(size=6) +
  geom_point(aes(PC1, PC2, color=class, fill=class),size=6) + 
  geom_polygon(data = boundary, aes(x,y), alpha = 0.5)


 # 2) Trees
#-----------
library(rpart)
library(rpart.plot)
# fit first tree using Gini impurity
first.tree <- rpart(class~., data=train.pca, method="class")
rpart.plot(first.tree,type=2,extra=4)
# there is no leafs for Create class. The reason for this could be 
# default min number of obs per node = 20 which is too high for a dataset with
# 33 observations. 
# We can reduce the min number of obs to increase number of splits
second.tree <- rpart(class~., data=train.pca, method="class",minsplit=7)
rpart.plot(second.tree,type=2,extra=4)

# Try growing full tree and then prunning
full.tree <- rpart(class~., data=train.pca, method="class",minsplit=2,
                   minbucket=1)
rpart.plot(full.tree,type=2,extra=4)
full.tree$variable.importance
printcp(full.tree)
# we want complexity of largest tree that is within 1 sd of the tree with 
# smallest xerror (1+0.171=1.171), cp = 0.0417
full.tree.pruned<-prune(full.tree, cp=0.042)
rpart.plot(full.tree.pruned,type=2,extra=4)


valid.first<-predict(first.tree, newdata = valid.pca,type="class")
valid.second<-predict(second.tree, newdata = valid.pca,type="class")
valid.prune<-predict(full.tree.pruned, newdata = valid.pca,type="class")

sum((valid.first==valid.pca[,1]))/length(valid.first)
sum((valid.second==valid.pca[,1]))/length(valid.second)
sum((valid.prune==valid.pca[,1]))/length(valid.prune)

# bagging
library(randomForest)
bag.tree <- randomForest(class~., data=train.pca, mtry=2,ntree=100)
bag.tree
valid.bag<-predict(bag.tree, newdata = valid.pca,type="class")
sum((valid.bag==valid.pca[,1]))/length(valid.bag)

# Random Forest
rf.tree <- randomForest(class~., data=train.pca, ntree=100)
valid.rf<-predict(rf.tree, newdata = valid.pca,type="class")
sum((valid.rf==valid.pca[,1]))/length(valid.rf)


# 3) SVM
#---------
# linear svm
library(e1071)
pred.error<-function(pred,truth)
{
  1-sum(diag(table(pred,truth)))/length(truth)
}
C.val<-c(0.01,0.05,0.1,0.2,0.5,1)
C.error<-numeric(length(C.val))
for(i in 1:length(C.val))
{
  model<-svm(class~.,data=train.pca,type="C-classification",kernel="linear",
             cost=C.val[i])
  pred.model<-predict(model, valid.pca)
  C.error[i]<-pred.error(pred.model,valid.pca$class)
}
C.sel<-C.val[min(which.min(C.error))]
C.sel
tune.svm(class~.,data=train.pca,type="C-classification",kernel="linear",
         cost=C.val)

plot(C.val,C.error,type="b", xlab="Cost value", ylab="Validation error")
abline(v=C.sel,lty=2)
train.svm1 <- svm(class~.,data=train.pca,type="C-classification",kernel="linear",
                  cost=C.sel)
summary(train.svm1)
plot(train.svm1,train.pca)

valid.svm1 <- predict(train.svm1,valid.pca)
sum((valid.svm1==valid.pca[,1]))/length(valid.svm1)

# Try radial basis kernel
train.tune <- tune(svm,class~., data=train.pca, type="C-classification", kernel="radial",
                   ranges=list(cost=c(0.01,0.05,0.1,0.2,1,2,5), 
                               gamma=c(0.01,0.05,0.1,0.2,0.3,0.4,0.5)))
summary(train.tune)$best.parameters
train.svm2 <- svm(class~., data=train.pca, type="C-classification", kernel="radial",
                  cost=train.tune$best.parameters[,"cost"], 
                  gamma=train.tune$best.parameters[,"gamma"])

plot(train.svm2,train.pca)

valid.svm2 <- predict(train.svm2,valid.pca)
sum((valid.svm2==valid.pca[,1]))/length(valid.svm2)
radial.error<-1-sum(diag(table(valid.svm2,valid.pca[,1])))/length(valid.svm2)


# Try a polynomial kernel with degree=2
train.tune<-tune(svm,class~.,data=train.pca,type="C-classification",kernel="polynomial",
                 degree=2,ranges=list(cost=c(0.01,0.05,0.1,0.2,1,2,5),
                                      gamma=c(0.01,0.05,0.1,0.2,0.3,0.4,0.5),
                                      coef0=c(0,1,2,3)))
summary(train.tune)$best.parameters
train.svm3<-svm(class~.,data=train.pca,type="C-classification",kernel="polynomial",
                degree=2,
                cost=train.tune$best.parameters[,"cost"], 
                gamma=train.tune$best.parameters[,"gamma"], 
                coef0=train.tune$best.parameters[,"coef0"])

plot(train.svm3,train.pca)

valid.svm3 <- predict(train.svm3,valid.pca)
sum((valid.svm3==valid.pca[,1]))/length(valid.svm3)


# Try different values
train.tune<-tune(svm,class~.,data=train.pca,type="C-classification",kernel="polynomial",
                 ranges=list(degree=c(1:5),cost=c(1:6),
                             gamma=c(0.2,0.3,0.4,0.5),
                             coef0=c(0,1,2,3,4,5)))
summary(train.tune)$best.parameters
train.svm4<-svm(class~.,data=train.pca,type="C-classification",kernel="polynomial",
                degree=train.tune$best.parameters[,"degree"],
                cost=train.tune$best.parameters[,"cost"], 
                gamma=train.tune$best.parameters[,"gamma"], 
                coef0=train.tune$best.parameters[,"coef0"])

plot(train.svm4,train.pca)

valid.svm4 <- predict(train.svm4,valid.pca)
sum((valid.svm4==valid.pca[,1]))/length(valid.svm4)



# 5. Compare the models #
#=======================#

# create summary datasets

sensitivity <- data.frame()
specificity <- data.frame()
accuracy <- data.frame()

summary <- function(model,name){
  cross.class.tab <- table(valid.data$class, model)
  
  a1 <- cross.class.tab["Crete","Crete"]/sum(cross.class.tab["Crete",])
  a2 <- cross.class.tab["Other","Other"]/sum(cross.class.tab["Other",])
  a3 <- cross.class.tab["Peloponese","Peloponese"]/sum(cross.class.tab["Peloponese",])
  a <- data.frame(Crete=a1, Other=a2, Peloponese=a3, row.names = name)
  sensitivity <<- rbind(sensitivity,a)
  
  b1 <- sum(cross.class.tab[-1,-1])/sum(cross.class.tab[-1,])
  b2 <- sum(cross.class.tab[-2,-2])/sum(cross.class.tab[-2,])
  b3 <- sum(cross.class.tab[-3,-3])/sum(cross.class.tab[-3,])
  b <- data.frame(Crete=b1, Other=b2, Peloponese=b3, row.names = name)
  print(b)
  specificity <<- rbind(specificity,b)  
  
  c <- data.frame(sum(diag(cross.class.tab))/sum(cross.class.tab),row.names = name)
  accuracy <<- rbind(accuracy,c)
}
summary(valid.knn, "KNN")
summary(valid.first, "first tree")
summary(valid.second, "second tree")
summary(valid.prune, "pruned tree")
summary(valid.bag, "bagging tree")
summary(valid.rf, "random forest")
summary(valid.svm1, "svm lin")
#summary(valid.svm2, "svm 2")
summary(valid.svm3, "svm radial")
summary(valid.svm4, "svm polynom")

sensitivity
specificity
colnames(accuracy) <- c("Accuracy")
accuracy

# Predict values on test data using selected model
test.knn<-knn(train.pca[,-1], test.pca[,-1], train.pca[,1], 
               k=which.max(corr.class.rate))
sum((test.knn==test.pca[,1]))/length(test.knn)

table(test.pca[,1],test.knn)

# plot actuals vs predicted
library(plyr)
plot.df <- data.frame(test.pca, predicted = test.knn)
plot.df1 <- data.frame(x = plot.df$PC1, 
                       y = plot.df$PC2, 
                       predicted = plot.df$predicted)

find_hull = function(df) df[chull(df$x, df$y), ]
boundary = ddply(plot.df1, .variables = "predicted", .fun = find_hull)
colnames(plot.df)<- c("class","PC1","PC2","predicted")

ggplot(plot.df, aes(PC1, PC2, color=class, fill=class))+
  geom_point(size=6) +
  geom_point(aes(PC1, PC2, color=predicted, fill=predicted),size=3)


ggplot(plot.df, aes(PC1, PC2, color=predicted, fill=predicted))+
  geom_point(size=6) +
  geom_point(aes(PC1, PC2, color=class, fill=class),size=6) + 
  geom_polygon(data = boundary, aes(x,y), alpha = 0.5)
