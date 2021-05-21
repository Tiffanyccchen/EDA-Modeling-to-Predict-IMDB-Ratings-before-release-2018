#qualititative(index:accuracy,low/high,high/low error rate)
quantile(imdbscore,0.5)
quantile(imdbscore,0.25)
quantile(imdbscore,0.75)
quantile(imdbscore,0.34)
quantile(imdbscore,0.67)

movieimpute$category <- cut(movie$imdb_score,
                     breaks=c(-Inf, 5.8, 7.2, Inf),
                     labels=c("low","medium","high"))

movieimpute$category2 <- cut(movie$imdb_score,
                     breaks=c(-Inf, 6.5, Inf),
                     labels=c("<median",">median"))

write.csv(movieimpute,"movieimpute.csv")

traindat$category<-movieimpute[tran,"category"]
traindat$category2<-movieimpute[tran,"category2"]
str(traindat)
levels(traindat$category)
traindat$category<-factor(traindat$category,ordered=TRUE)

train<-traindat[,-1]
str(train)

train2<-train[,-16]
train2$category<-traindat$category2
str(train2)

testdat$category<-movieimpute[-tran,"category"]
testdat$category2<-movieimpute[-tran,"category2"]
str(testdat)
levels(testdat$category)
testdat$category<-factor(testdat$category,ordered=TRUE)

test<-testdat[,-1]
str(test)

test2<-test[,-16]
test2$category<-testdat$category2
str(test2)

table(train$category)
table(test$category)
table(train$category)
table(test$category)

##(1)Logistic Regression/qualititative---
###---(1)3 category-method1
library(VGAM)
fitlogit<-vglm(category~.,data=train,family=cumulative(parallel=T))
summary(fitlogit)

###---(1)3 category-method2
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
m <- polr(category~.,data=train, Hess=TRUE)
summary(m)

m2 <- stepAIC(m)
m2$anova
anova(m,m2)
summary(m2)
ctable <- coef(summary(m2))

p <- round(pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2,4)
ctable <- cbind(ctable, "p value" = p)
ctable

###confidence intervals
ci <- confint(m2)

###OR and CI
round(exp(cbind(OR = coef(m2), ci)),3)

###prediction
table(testdat$category,predict(m2,test))
#:accuracy:(38+183+40)/456
#:FP(low-high):(3)/120
#:FN(high-low):(2)/98
#:Ppred:(40)/76
#:Npred:(38)/72

###---(2)2 category
fit2<-glm(category~.,data=train2,family=binomial(link=logit))
nothing<-glm(category~1,data=train2,family=binomial(link=logit))
summary(nothing)

###variable selection---
forwards <- step(nothing,scope=list(lower=formula(nothing),upper=formula(fit2)), direction="forward")
backwards <-step(fit2,trace=0)
stepwise<-step(nothing, list(lower=formula(nothing),upper=formula(fit2)),direction="both",trace=0)

formula(forwards)
formula(backwards)
formula(stepwise)

summary(forwards)
summary(backwards)
summary(stepwise)

round(exp(coef(backwards)),3)

#The difference between the null deviance and the residual deviance shows how
#our model is doing against the null model (a model with only the intercept).
#The wider this gap, the better. Analyzing the table we can see the drop in 
#deviance when adding each variable one at a time. Again, adding Pclass, 
#Sex and Age significantly reduces the residual deviance. The other variables 
#seem to improve the model less even though SibSp has a low p-value. 
#A large p-value here indicates that the model without the variable explains 
#more or less the same amount of variation. Ultimately what you would like to 
#see is a significant drop in deviance and the AIC.

anova(backwards, test="Chisq")

###prediction
logistic.probs=predict(backwards,test2,type="response")
logistic.pred=rep("<median",456)
logistic.pred[logistic.probs>0.5]=">median"
table(test2$category,logistic.pred)

#:accuracy:(177+149)/456
#:FP:(62)/239
#:FN:(68)/217
#:Ppred:(149)/211
#:Npred:(177)/245
#:AUC:0.7645

library(ROCR)
pr <- prediction(logistic.probs, test2$category)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,main="Logistic-AUC:0.7645")
abline(a=0,b=1,col="blue")

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

##(2)LDA/qualititative---
###(1)3 category
lda.fit<-lda(category ~ runtime + faceinposter + directorfblikes + year + 
    contentrating + color + language + aspectratio + genres + 
    month + pc1castfb + pc2castfb + sequel,data=train)
lda.fit

lda.pred=predict(lda.fit,train)
str(lda.pred)
ldahist(lda.pred$x[,1], g=lda.pred$class)

names(lda.pred)
lda.pred=predict(lda.fit,test)
lda.class<-lda.pred$class

###prediction
table(test$category,lda.class)
#:accuracy:(35+188+38)/456
#:FP(low-high):(5)/120
#:FN(high-low):(4)/98
#:Ppred:(38)/69
#:Npred:(35)/63

###(2)2 category
lda.fit2<-lda(category ~ runtime + faceinposter + directorfblikes + year + 
    contentrating + color + language + aspectratio + genres + 
    month + pc1castfb + pc2castfb + sequel,data=train2)
lda.fit2
plot(lda.fit2)

lda.pred2=predict(lda.fit2,train2)
str(lda.pred2)
ldahist(lda.pred2$x[,1], g=lda.pred2$class)

names(lda.pred2)
lda.pred2=predict(lda.fit2,test2)
str(lda.pred2)
lda.class2<-lda.pred2$class

###prediction
table(test2$category,lda.class2)
#:accuracy:(179+145)/456
#:FP:(60)/239
#:FN:(72)/217
#:Ppred:(145)/205
#:Npred:(179)/251
#:AUC:

pr <- prediction(lda.pred2$posterior[,2], test2$category)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,main="LDA-AUC:0.7593")
abline(a=0,b=1,col="blue")

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

##(3)QDA/qualititative---

###(1)3 category
qda.fit<-qda(category ~ runtime + faceinposter + directorfblikes + year + 
    contentrating + color + language + aspectratio + genres + 
    month + pc1castfb + pc2castfb + sequel,data=train)
model.matrix(train)
###Error in qda.default(x, grouping, ...) : rank deficiency in group low

qda.fit

names(qda.pred)
qda.pred=predict(qda.fit,test)
qda.class<-qda.pred$class

###(2)2 category
qda.fit2<-qda(category ~ runtime + faceinposter + directorfblikes + year + 
    contentrating + color + language + aspectratio + genres + 
    month + pc1castfb + pc2castfb + sequel,data=train2)
qda.fit2

names(qda.pred2)
qda.pred2=predict(qda.fit2,test2)
qda.class2<-qda.pred2$class

###prediction
table(test2$category,qda.class2)
#:accuracy:(205+87)/456
#:FP:(34)/239
#:FN:(130)/217
#:Ppred:(87)/121
#:Npred:(205)/335
#:AUC:
names(qda.pred2)
str(qda.pred2)
pr <- prediction(qda.pred2$posterior[,2], test2$category)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,main="QDA-AUC:0.7120")
abline(a=0,b=1,col="blue")

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

##(4)KNN/qualititative---
a<-c("runtime","faceinposter","directorfblikes","year", 
    "contentrating","color","language","aspectratio","genres",
    "month","pc1castfb","pc2castfb","sequel")
str(movieimpute)
cat3knn<-movieimpute[,a]
str(cat3knn)
cat3knn$sequel<-as.integer(cat3knn$sequel)

#Leave the numerical features unchanged.
For the categorical features, do the following:
Assume for some categorical feature F, we have K levels. 
Convert this feature in a K - 1 dummy binary variables.
Don’t forget to scale your data. Also, Euclidean distance isn’t 
always the best metric to use.

library(dummies)
cat3knn[,c(1:4,13)]<-scale(cat3knn[,c(1:4,13)])
catknn<-dummy.data.frame(cat3knn, sep = ".")
names(catknn)
str(catknn)
catknn<-data.frame(catknn)
catknn<-scale(catknn)

#use cv to tune best k for knn, need about an hour to run, don't perform without consideration
#create folds
folds<-createFolds(1:4567,k=10)
length(folds)
folds[[1]]

###(1)3categories---
label<-movieimpute$category
str(label)
length(label)

library(class)
ks <- seq(10,60,by=10)
res <- sapply(ks, function(k) {
  ##try out each version of k
  res.k <- sapply(seq_along(folds), function(i) {
    ##loop over each of the 10 cross-validation folds
    ##predict the held-out samples using k nearest neighbors
    pred <- knn(train=catknn[ -folds[[i]],],
                test=catknn[ folds[[i]], ],
                cl=t(label[ -folds[[i]] ]), k = k)
    ##the ratio of misclassified samples
    mean(label[ folds[[i]]] == pred)
  })
  ##average over the 10 folds
  mean(res.k)
})
plot(ks, res, type="o",ylab="accuracy",main="k:10-60 (3 categories)")

pred <- knn(train=catknn[tran,],
                  catknn[-tran,],
                cl=t(label[tran]), k =30 )

###prediction
table(testdat$category,pred)
#:accuracy:(23+199+31)/456
#:FP(low-high):(2)/120
#:FN(high-low):(2)/98
#:Ppred:(31)/50
#:Npred:(23)/49

###(2)2categories---
label<-movieimpute$category2
str(label)
length(label)

ks <- seq(1,61,by=5)
res <- sapply(ks, function(k) {
  ##try out each version of k
  res.k <- sapply(seq_along(folds), function(i) {
    ##loop over each of the 10 cross-validation folds
    ##predict the held-out samples using k nearest neighbors
    pred <- knn(train=catknn[ -folds[[i]],],
                test=catknn[ folds[[i]], ],
                cl=t(label[ -folds[[i]] ]), k = k)
    ##the ratio of misclassified samples
    mean(label[ folds[[i]]] == pred)
  })
  ##average over the 10 folds
  mean(res.k)
})

plot(ks, res, type="o",ylab="accuracy",main="k:1-61(2 categories)")

pred <- knn(train=catknn[tran,],
                  catknn[-tran,],
                cl=t(label[tran]), k =26, prob = TRUE )

###prediction
table(test2$category,pred)
#:accuracy:(177+125)/456
#:FP:(62)/239
#:FN:(92)/217
#:Ppred:(125)/187
#:Npred:(177)/269
#:AUC:

prob <- attr(pred, "prob")
pr <- prediction(prob, test2$category)
str(pr)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
str(prf)
plot(prf,main="KNN-AUC:0.4767")
abline(a=0,b=1,col="blue")
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

##(5.1)Decision Tree/qualititative---

###(1)3 categories---
library(rpart)
library(rattle)

fit <- rpart(category~., method="class", data=train,minsplit =2,minbucket =1,cp=-1)

###When rpart grows a tree it performs 10-fold cross validation on the data. 
#To see the cross validation results use the printcp() function.
#The rel error of each iteration of the tree is the fraction of mislabeled elements 
#in the iteration relative to the fraction of mislabeled elements in the root. 
#In this example, 50% of training cases are fraudulent. The first splitting criteria is 
#“Is the claimant very active?”, which separates the data into a set of three cases, 
#all of which are fraudulent and a set of seven cases of which two are fraudulent. 
#Labeling the cases at this point would produce an error rate of 20% which is 40% of 
#the root node error rate (i.e. it’s 60% better). The cross validation error rates and 
#standard deviations are displayed in the columns xerror and xstd respectively.

str(train)
printcp(fit) # display the results 
plotcp(fit)
# visualize cross-validation results 
summary(fit) # detailed summary of splits
fit$cptable[which.min(fit$cptable[,"xerror"]),]
 #    CP       nsplit    rel error       xerror         xstd 
 0.002472799 38.000000000  0.752720079  0.900098912  0.015750464 

###As a rule of thumb, it’s best to prune a decision tree using the cp of smallest 
#tree that is within one standard deviation of the tree with the smallest xerror. 
#In this example, the best xerror is 0.4 with standard deviation .25298. 
#So, we want the smallest tree with xerror less than 0.65298. 
#This is the tree with cp = 0.2, so we’ll want to prune our tree with a cp 
#slightly greater than than 0.2.

#xerror:0.900098912+0.015750464=0.9158494
#choose:cp= 0.00568744     
fit$cptable[7,]
   #   CP       nsplit    rel error       xerror         xstd 
 #0.005687438 12.000000000  0.845697329  0.914441147  0.015774655 


# prune the tree 
pfit<- prune(fit, cp=0.00569)
fancyRpartPlot(pfit,type=0,cex=0.75)

###prediction
aaa<-predict(pfit,test,type="class")
table(test$category,aaa)
#:accuracy:(46+184+28)/456
#:FP(low-high):(3)/120
#:FN(high-low):(4)/98
#:Ppred:(28)/48
#:Npred:(46)/87

###(2)2 cateories---

fit <- rpart(category~., method="class", data=train2,minsplit =2,minbucket =1,cp=-1)

printcp(fit) # display the results 
plotcp(fit)
# visualize cross-validation results 
summary(fit) # detailed summary of splits
fit$cptable[which.min(fit$cptable[,"xerror"]),]
  #     CP       nsplit    rel error       xerror         xstd 
# 0.004880429 10.000000000  0.582235237  0.636896047  0.014565768 

#xerror:0.636896047+0.014565768=0.6514618
#choose:cp=0.00585652
fit$cptable[8,]
   # CP       nsplit    rel error      xerror        xstd 
 #0.005856515     9    0.588091752 0.649585163 0.014641842

# prune the tree 
pfit<- prune(fit, cp=0.00586)
fancyRpartPlot(pfit,cex=0.7)

###prediction
set.seed(200)
pred<-predict(pfit,test2,type="class")
table(test2$category,pred)
#:accuracy:(158+148)/456
#:FP:(81)/239
#:FN:(69)/217
#:Ppred:(148)/229
#:Npred:(158)/227
#:AUC:
pred<-predict(pfit,test2)
str(pred)
pr <- prediction(pred[,2], test2$category)
str(pr)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
str(prf)
plot(prf,main="Decision Tree-AUC:0.705")
abline(a=0,b=1,col="blue")
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

##(5.2)Random Forest/qualititative---
###(1)3 categories---
oob.err=double(14)
test.err=double(14)
str(train)
str(test)
for(mtry in 1:14) 
{
  rf=randomForest(category ~ . , data = train,mtry=mtry,ntree=2000) 
  oob.err[mtry] = rf$err.rate[2000,1] #Error of all Trees fitted
  
  pred<-predict(rf,test) #Predictions on Test Set for each Tree
  test.err[mtry]=mean(pred==test$category) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
}

#Plotting both Test Error and Out of Bag Error
matplot(1:mtry , cbind(oob.err,test.err), pch=19,ylim=c(0.2,0.7),col=c("red","blue"),type="b",ylab="Error Rate",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))
test.err
text(x=9,y=0.485,label="0.4934")
rf=randomForest(category~.,data =train,importane = T,mtry=9,do.trace=1000,proximity =T,ntree=5000)
rf

#plot---------------------------------------------------------
plot(rf,main="IMDB(3 category) Random Forest")
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(rf, log="y",main="IMDB(3 category) Random Forest")
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rf$err.rate),col=1:4,cex=0.8,fill=1:4)

varImpPlot(rf,type=2,main="Variable Importance for predict IMDB 3 category")

###prediction
aaa<-predict(rf,test,type="class")
table(test$category,aaa)
#:accuracy:(14+160+57)/456
#:FP(low-high):(8)/120
#:FN(high-low):(1)/98
#:Ppred:(57)/137
#:Npred:(14)/21

###(2)2 categories---
oob.err2=double(14)
test.err2=double(14)
str(train2)
str(test2)
for(mtry in 1:14) 
{
  rf=randomForest(category ~ . , data = train2,mtry=mtry,ntree=2000) 
  oob.err2[mtry] = rf$err.rate[2000,1] #Error of all Trees fitted
  
  pred<-predict(rf,test2) #Predictions on Test Set for each Tree
  test.err2[mtry]=mean(pred==test2$category) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
}

#Plotting both Test Error and Out of Bag Error
matplot(1:mtry , cbind(oob.err2,test.err2), pch=19,col=c("red","blue"),type="b",ylab="Error Rate",xlab="Number of Predictors Considered at each Split",ylim=c(0.2,0.7))
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))
test.err
text(x=3,y=0.49,label="0.4912")

rf2=randomForest(category~.,data =train2,importane = T,mtry=3,do.trace=1000,proximity =T,ntree=000)
rf2

#plot---------------------------------------------------------
plot(rf2,main="IMDB(2 category) Random Forest")
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(rf2, log="y",main="IMDB (2 category) Random Forest")
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rf$err.rate),col=1:4,cex=0.8,fill=1:4)

varImpPlot(rf2,type=2,main="Variable Importance for predict IMDB 2 category")

###prediction
pred2<-predict(rf2,test2,type="class")
table(test2$category,pred2)
str(test2$category)
#:accuracy:(26+210)/456
#:FP:(213)/239
#:FN:(7)/217
#:Ppred:(210)/423
#:Npred:(26)/33

pred2<-predict(rf2,test2,type="prob")
str(pred2)
pred2[(pred2[,2]<0.7)]="<median"
pred2[(pred2[,2]>0.7)]=">median"
table(test2$category,pred2[,2])

#:accuracy:(181+141)/456
#:FP:(58)/239
#:FN:(76)/217
#:Ppred:(141)/199
#:Npred:(181)/257

#:AUC:
predauc<-predict(rf2,test2,type="prob")
pr <- prediction(predauc[,2], test2$category)
str(pr)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
str(prf)
plot(prf,main="Random Forest-AUC:0.743")
abline(a=0,b=1,col="blue")
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

##(6)SVM/qualititative---
###(1)3category---
trainsvm1<-train[,-1]
testsvm1<-test[,-1]
library(e1071)
trainsvm1$sequel<-as.integer(trainsvm1$sequel)
svmqualtrain<-dummy.data.frame(trainsvm1, sep = ".")

testsvm1$sequel<-as.integer(testsvm1$sequel)
svmqualtest<-dummy.data.frame(testsvm1, sep = ".")

svmqualtrain<-scale(svmqualtrain)
svmqualtest<-scale(svmqualtest)

svmqualtrain<-data.frame(svmqualtrain)
svmqualtest<-data.frame(svmqualtest)

svmqualtrain<-cbind(movieimpute$category[tran],svmqualtrain)
svmqualtest<-cbind(movieimpute$category[-tran],svmqualtest)

names(train)[1]<-"category"
names(test)[1]<-"category"
str(svmqualtrain)
str(svmqualtest)

svm_tunecat3 <- tune(svm,category~.,data =svmqualtrain, kernel="radial", ranges=list(cost=10^(-1:3), gamma=c(0.5,1,2,3,4)))

summary(svm_tunecat3)
print(svm_tunecat3)
plot(svm_tunecat3)

tunedModel <- svm_tunecat3$best.model
tunedModel
tunedModelY <- predict(tunedModel,svmqualtest22,type="class") 
table(svmqualtest$category,tunedModelY)
str(svmqualtest22)

###(2)2 category---
trainsvm2<-train2[,-15]
testsvm2<-test2[,-15]
library(e1071)
trainsvm2$sequel<-as.integer(trainsvm2$sequel)
svmqualtrain2<-dummy.data.frame(trainsvm2, sep = ".")

testsvm2$sequel<-as.integer(testsvm2$sequel)
svmqualtest2<-dummy.data.frame(testsvm2, sep = ".")

svmqualtrain2<-scale(svmqualtrain2)
svmqualtest2<-scale(svmqualtest2)

svmqualtrain2<-data.frame(svmqualtrain2)
svmqualtest2<-data.frame(svmqualtest2)

svmqualtrain2<-cbind(movieimpute$category2[tran],svmqualtrain2)
svmqualtest2<-cbind(movieimpute$category2[-tran],svmqualtest2)

names(svmqualtrain2)[1]<-"category"
names(svmqualtest2)[1]<-"category"
str(svmqualtrain2)
str(svmqualtest2)

svm_tunecat2 <- tune(svm,category~.,data =svmqualtrain2 ,kernel="radial", ranges=list(cost=10^(-1:3), gamma=c(0.5,1,2,3,4)))

summary(svm_tunecat2)
print(svm_tunecat2)
plot(svm_tunecat2)

tunedModel2 <- svm_tunecat2$best.model
tunedModel2<-svm(category~.,data=svmqualtrain2 ,kernel="radial",cost=1,gamma=0.5,probability=TRUE)
tunedModel2 

###prediction
tunedModelY2 <- predict(tunedModel2,svmqualtest2,type="class") 
table(svmqualtest2$category,tunedModelY2)

#:accuracy:(111+168)/456
#:FP:(128)/239
#:FN:(49)/217
#:Ppred:(168)/296
#:Npred:(111)/160
prob <- attr (aucsvmqua, "probabilities")
str(prob)
prob[(prob[,1]<0.7)]="<median"
prob[(prob[,1]>0.7)]=">median" 
table(svmqualtest2$category,prob[,1])
table(test2$category,eee)

#:AUC:probability=TRUE, decision.values=TRUE
aucsvmqua<-predict(tunedModel2,svmqualtest2,probability=TRUE,decision.values=TRUE)
str(aucsvmqua)
prob <- attr (aucsvmqua, "probabilities")
pr <- prediction(prob, svmqualtest2$category)
str(pr)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
str(prf)
plot(prf,main="SVM-AUC:0.653")
abline(a=0,b=1,col="blue")
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc