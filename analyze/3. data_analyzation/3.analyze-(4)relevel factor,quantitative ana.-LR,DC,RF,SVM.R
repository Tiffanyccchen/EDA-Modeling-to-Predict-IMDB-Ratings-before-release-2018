###3.analyze--
rmse <- function(error){
sqrt(mean(error^2))
}
print('a')

#We did this using 90/10 holdout
#cross validation, training on 90% of our data, and testing on the remaining 10%,randomly sample 90%/10%---

##Split training set into training and validation sat
tran<-sample(1:4567,4111)
traindat<-movieimpute[tran,]
dim(traindat)
testdat<-movieimpute[-tran,]
dim(testdat)

traindat<-traindat[,-c(2,6:9)]
testdat<-testdat[,-c(2,6:9)]

sd(traindat$imdbscore)
mean(traindat$imdbscore)
sd(testdat$imdbscore)
mean(testdat$imdbscore)

#covert unit
traindat$budget<-traindat$budget/1000000
traindat$pc1castfb<-traindat$pc1castfb/1000
traindat$pc2castfb<-traindat$pc2castfb/1000

testdat$budget<-testdat$budget/1000000
testdat$pc1castfb<-testdat$pc1castfb/1000
testdat$pc2castfb<-testdat$pc2castfb/1000

#change factor base level
levels(traindat$aspectratio)
levels(traindat$contentrating)
levels(traindat$color)
levels(traindat$language)
levels(traindat$genres)
levels(traindat$month)
levels(traindat$sequel)

traindat$aspectratio<-factor(traindat$aspectratio,levels=c("Others","1.85","2.35"))
traindat$contentrating<-factor(traindat$contentrating,levels=c("Others","G","PG","PG-13","R","NC-17"))
traindat$language<-factor(traindat$language, levels =c("Others","English","French","German","Italian","Spanish","Mandarin","Hindi"))
traindat$genres<-factor(traindat$genres, levels =c("Others","Action","Adventure","Animation","Biography","Comedy","Crime","Documentary", 
"Drama","Family","Fantasy","Horror","Mystery","Sci-Fi","Thriller","Western"))
traindat$month<-factor(traindat$month, levels =c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

traindat$sequel<-factor(traindat$sequel,ordered=TRUE)
testdat$sequel<-factor(testdat$sequel,ordered=TRUE)


##1. Linear Regression/quantitative(y=imdbscore)---(不用Lasso,Ridge原因:機器學習課本ridge那邊+網站資料(在word))
fit<-lm(imdbscore~.,data=traindat)
summary(fit)$r.squared
cor(predict(fit,testdat),testdat$imdbscore)
sqrt(mean((predict(fit,testdat)-testdat$imdbscore)^2))

#(1.1)check common variance and normality----
#package needed-----
library(car)

#common variance----
vif(fit)
par(mfrow=c(1,1))
yhat<-fit$fitted.values
plot(yhat, rstudent(fit), ylab="Fitted Values", xlab="R-Student ", main="Residual Plot of Original Model") 
abline(0, 0)  

#normality----
qqnorm(rstudent(fit))
qqline(rstudent(fit))
shapiro.test(fit$residuals)

#transform----
#package needed----
library(MASS)

boxcox(imdbscore~.,data=traindat,lambda=seq(0,4,length=16))
fit<-lm(imdbscore^2.5~.,data=traindat)

cor(predict(fit,testdat)^(1/2.5),testdat$imdbscore)
sqrt(mean((predict(fit,testdat)^(1/2.5)-testdat$imdbscore)^2))

#common variance & normality again----
vif(fit)
par(mfrow=c(1,1))
yhat<-fit$fitted.values
plot(yhat, rstudent(fit), 
ylab="Fitted Values", xlab="R-Student ", 
main="Residual Plot of Transformed Model") 
abline(0, 0)  
qqnorm(rstudent(fit))
qqline(rstudent(fit))
shapiro.test(fit$residuals)

#(1.2)variable selection---
step <- stepAIC(fit, direction="backward")
step$anova

step <- stepAIC(fit, direction="both")
step$anova

fit<-lm(imdbscore^2.5~runtime + budget + faceinposter + directorfblikes + 
    year + contentrating + color + language + genres + month + 
    pc1castfb + sequel,data=traindat)

summary(fit)
anova(fit)

#check common variance and normality----
vif(fit)
par(mfrow=c(1,1))
yhat<-fit$fitted.values
plot(yhat, rstudent(fit), 
ylab="Fitted Values", xlab="R-Student", 
main="Residual Plot of Selected Model") 
abline(0, 0)  

qqnorm(rstudent(fit))
qqline(rstudent(fit))
shapiro.test(rstudent(fit))

cor(predict(fit,testdat)^(1/2.5),testdat$imdbscore)
sqrt(mean((predict(fit,testdat)^(1/2.5)-testdat$imdbscore)^2))
hist((predict(fit,testdat)^(1/2.5)-testdat$imdbscore),col="lightblue",xlab="Error ",main="Histogram of Error - Linear Reg.")

##2.decision Tree/quantitative(y=imdbscore)----

#package needed----
library(rpart)
library(rattle)

### grow tree 
#Setting cp to a negative amount : that the tree will be fully grown.
#Notice the output shows only a root node. This is because rpart has some default 
#parameters that prevented our tree from growing. Namely: minsplit and minbucket. 
#minsplit : “the minimum number of observations that must exist in a node in order 
#for a split to be attempted”.
#minbucket : “the minimum number of observations in any terminal node”. See what happens when we override these parameters.
#The complexity measure : a combination of the size of a tree and the ability of the tree
#to separate the classes of the target variable. If the next best split in growing a tree 
#does not reduce the tree’s overall complexity by a certain amount, rpart will terminate 
#the growing process. This amount is specified by the complexity parameter, cp.
fit <- rpart(imdbscore~., method="anova", data=traindat,minsplit =2,minbucket =1,cp=-1)

### tree automatically verification
#When rpart grows a tree it performs 10-fold cross validation on the data. 
#To see the cross validation results use the printcp() function.
#The rel error of each iteration of the tree is the fraction of mislabeled elements 
#in the iteration relative to the fraction of mislabeled elements in the root. 
#In this example, 
#Labeling the cases at this point would produce an error rate of 20% which is 40% of 
#the root node error rate (i.e. it’s 60% better). The cross validation error rates and 
#standard deviations are displayed in the columns xerror and xstd respectively.

printcp(fit) # display the results 
plotcp(fit)
text(x= # visualize cross-validation results 
summary(fit) # detailed summary of splits
fit$cptable[which.min(fit$cptable[,"xerror"]),]

 #  CP            nsplit    rel error       xerror         xstd 
 #0.004723619 13.000000000  0.748361952  0.792405140  0.023354357 

#As a rule of thumb, it’s best to prune a decision tree using the cp of smallest 
#tree that is within one standard deviation of the tree with the smallest xerror. 
#In this example, the best xerror is 0.4 with standard deviation .25298. 
#So, we want the smallest tree with xerror less than 0.65298. 
#This is the tree with cp = 0.2, so we’ll want to prune our tree with a cp 
#slightly greater than than 0.2.

#xerror:0.792405140+0.023354357=0.8157595
#choose:cp=0.008435097(slightly larger=0.0085)

fit$cptable[8,]
  # CP      nsplit   rel error      xerror        xstd 
  #0.008435097 7.000000000 0.784874258 0.812566674 0.023614686 

#prune the tree 
pfit<- prune(fit, cp=0.0085)
fancyRpartPlot(pfit)

sqrt(mean((predict(pfit,testdat)-testdat$imdbscore)^2))
cor(predict(pfit,testdat),testdat$imdbscore)

hist((predict(pfit,testdat)-testdat$imdbscore),col="lightblue",xlab="Error ",main="Histogram of Error - Decision Tree")

##3.Random Forest/quantitative(y=imdbscore)---
#package needed
library(caret)
library(randomForest)
set.seed(13)

#tuning parameter 1---
#Both 10-fold cross-validation and 3 repeats slows down the search process, 
#but is intended to limit and reduce overfitting on the training set. 
#It won’t remove overfitting entirely. Holding back a validation set for final checking 
#is a great idea if you can spare the data.

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
tunegrid <- expand.grid(.mtry=c(1:14))
metric <- "MSE"

rf_gridsearch <- train(imdbscore~., data=traindat, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)

plot.new()
plot(rf_gridsearch,xlab="mtry (Randomly Selected Predictors)")
abline(v=13.5/18,col="darkred")
text(x=14/18,y=1/15,label="0.8992")

#tune parameter 2 ---
oob.err=double(14)
test.err=double(14)

for(mtry in 1:14) 
{
  rf=randomForest(imdbscore ~ . , data = traindat,mtry=mtry,ntree=2000) 
  oob.err[mtry] = rf$mse[2000] #Error of all Trees fitted
  
  pred<-predict(rf,testdat) #Predictions on Test Set for each Tree
  test.err[mtry]= with(testdat, mean( (imdbscore - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
}

#Plotting both Test Error and Out of Bag Error
matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))
text(x=10,y=1.05,label="1.052")

#fit model
rf=randomForest(imdbscore~.,data =traindat,importane = T,mtry=10,do.trace=1000,proximity =T,ntree=2000)
rf

#plot---------------------------------------------------------
plot(rf,main="IMDB score Random Forest")
varImpPlot(rf,type=2,main="Variable Importance for predict IMDB score")

sqrt(mean((predict(rf,testdat)-testdat$imdbscore)^2))
cor(predict(rf,testdat),testdat$imdbscore)
hist((predict(rf,testdat)-testdat$imdbscore),col="lightblue",xlab="Error ",main="Histogram of Error - Random Forest")

##4.SVM Reg./quantitative(y=imdbscore)----
#package needed----
library(e1071)
library(dummy)

traindat1<-traindat
testdat1<-testdat
traindat1$sequel<-as.integer(traindat1$sequel)
svmquantrain<-dummy.data.frame(traindat1, sep = ".") #efficiently creates dummy variables for a variety of structures.

testdat1$sequel<-as.integer(testdat1$sequel)
svmquantest<-dummy.data.frame(testdat1, sep = ".") 

svmquantrain<-scale(svmquantrain) #standardize data
svmquantest<-scale(svmquantest)

svmquantrain<-data.frame(svmquantrain)
svmquantest<-data.frame(svmquantest)

#tuning parameter----
sd(traindat$imdbscore)
mean(traindat$imdbscore)
svm_tune <- tune(svm,imdbscore~.,data = svmquantrain, kernel="radial", ranges=list(cost=10^(-1:3), gamma=c(0.5,1,2,3,4)))

summary(svm_tune)
print(svm_tune)
plot(svm_tune)

tunedModel <- svm_tune$best.model
tunedModelY <- predict(tunedModel,svmquantest) 
error <- testdat$imdbscore -(1.103664*tunedModelY+6.428874)

#this value can be different on your computer because the tune method  randomly shuffles the data
tunedModelRMSE <- rmse(error)  
tunedModelRMSE 
hist(error,col="lightblue",xlab="Error ",main="Histogram of Error - SVM")