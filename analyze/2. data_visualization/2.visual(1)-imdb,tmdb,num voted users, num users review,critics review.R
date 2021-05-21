library(stringi)
library(tidyverse)
library(jsonlite)

options(scipen = 999) #not to use exponential notation

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

movie<-read_csv(dir + "/data/data visualization/3moviecut.csv",guess_max=3000)
attach(movie)

###1.imdb score hist---------------------
summary(imdb_score)

h <- hist(imdb_score,col="lightblue",xlab="imdb score",main="Histogram & Normal Curve of imdb score",xaxt="n")
xfit <- seq(min(imdb_score),max(imdb_score),length=40) 
yfit <- dnorm(xfit,mean=mean(imdb_score),sd=sd(imdb_score)) 
yfit <- yfit*diff(h$mids[1:2])*length(imdb_score)
lines(xfit, yfit, col="yellow", lwd=2)

abline(v = mean(imdb_score),
 col = "royalblue",
 lwd = 2)
abline(v = median(imdb_score),
 col = "darkred",
 lwd = 2)

text(c(1,2,3,4,5,6,7,8,9,10), par("usr")[3] - 0.2, labels =c(1,2,3,4,5,6,7,8,9,10),
cex=1, xpd = TRUE)

legend(x = "topleft", # location of legend within plot area
 c("Mean=6.42", "Median=6.5"),
 col = c("royalblue", "darkred"),
 lwd = c( 2, 2))

###1.tmdb score hist----------------------------------------------------------------
is.na(vote_average)<-vote_average=="0"
summary(vote_average)
vote<-moviecut[!is.na(vote_average),]

hist(vote$vote_average,col="lightblue",xlab="tmdb score",main="Histogram & Normal Curve of tmdb score",xaxt="n")
xfit<-seq(min(vote$vote_average),max(vote$vote_average),length=40) 
yfit<-dnorm(xfit,mean=mean(vote$vote_average),sd=sd(vote$vote_average))
yfit <- yfit*diff(h$mids[1:2])*length(vote$vote_average) 
lines(xfit, yfit, col="yellow", lwd=2)

abline(v = mean(vote$vote_average),
 col = "royalblue",
 lwd = 2)
abline(v = median(vote$vote_average),
 col = "darkred",
 lwd = 2)

text(c(1,2,3,4,5,6,7,8,9,10), par("usr")[3] - 0.2, labels =c(1,2,3,4,5,6,7,8,9,10),
cex=1, xpd = TRUE)
legend(x = "topleft", # location of legend within plot area
 c("Mean=6.1", "Median=6.2"),
 col = c("royalblue", "darkred"),
 lwd = c( 2, 2))

###2.imdb and tmdb box plot -----------------------------------------------------------
library(data.table)
library(dplyr)

## create a dataset for each vector
df_A = data.frame(value=imdb_score, id="imdb")
df_B = data.frame(value=moviecut[!is.na(vote_average),"vote_average"], id="tmdb")

## combine datasets
df = rbind(df_A, df_B)

## create the box plot
fill <- c("gold1","#4271AE")
line <- "#1F3552"

df%>%
  group_by(id) %>%
  mutate(outlier = ifelse(is_outlier(value),value, as.numeric(NA))) %>%
  ggplot(., aes(x =id, y = value)) +
        geom_boxplot(fill = fill, colour = line, alpha = 0.7) +
        scale_y_continuous(name = "Score Distribution",
                           breaks = seq(0,10,1),
                           limits=c(0, 10)) +
        scale_x_discrete(name = "") +
        ggtitle("imdb & tmdb's Score Box Plot")+
        geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3,size=2.54)+
        theme(axis.text.x = element_text(size = 12, colour = "black"),
        plot.title = element_text(hjust = 0.5))

###2.imdb and tmdb scatter plot -----------------------------------------------------------
fit <- lm(moviecut[!is.na(vote_average),"imdb_score"]~moviecut[!is.na(vote_average),"vote_average"])
summary(fit)
rmse <- round(sqrt(mean(resid(fit)^2)), 2)
coefs <- coef(fit)
b0 <- round(coefs[1], 2)
b1 <- round(coefs[2],2)
r2 <- round(summary(fit)$r.squared, 2)
R<-round(sqrt(summary(fit)$r.squared),2)
eqn <- bquote(italic(y) == .(b0) + .(b1)*italic(x) * "," ~~ 
              R^2 == .(r2) * "," ~~ r== .(R) * ","~~RMSE == .(rmse))

ggplot(data=moviecut,aes(x=moviecut[!is.na(vote_average),"vote_average"], y=moviecut[!is.na(vote_average),"imdb_score"])) + 
       geom_point(shape=18, color="brown")+
       geom_smooth(method=lm,  linetype="dashed",
             color="darkred", fill="purple")+
       scale_y_continuous(name = "imdb score")+
       scale_x_continuous(name = "tmdb score")+
       theme(axis.text=element_text(size=12),
       axis.title=element_text(size=14,face="bold"))+
       annotate(geom="text", x=5, y=1, label="y=0.24+1x, r=0.87, pvalue<0.0001")

###3.number of voted users box plot------------------------------------------------------------------
summary(num_voted_users)
##outlier:value>98351+1.5*(98351-9511)

df_A = data.frame(value=num_voted_users, id="Number of Voted Users")
fill <- c("aquamarine2")
line <- "#1F3552"

p10 <- ggplot(df_A , aes(x ="", y = value)) +
        geom_boxplot(fill = fill, colour = line,
                     alpha = 0.7) +
        scale_y_continuous(name = "Number of Users",
                           breaks = seq(0,1700000,100000),
                           limits=c(0, 1700000)) +
        ggtitle("Number of Users Voted Box Plot")+
        theme(axis.text.x = element_text(size = 12, colour = "black"),
        plot.title = element_text(hjust = 0.5))+
        annotate("text",x=1.1,y=231611, label= "231611")+
        labs(x="",y="Number of  Voted Users") 

p10
###3.create number of voted users vs imdb score scatter plot-----------------------------------
dim(filter(movie,num_voted_users>=231611))[1]
divide<-(num_voted_users)/1000

fit <- lm(imdb_score~divide)
fit1 <- lm(imdb_score~log(divide))
summary(fit)
summary(fit1)
rmse <- round(sqrt(mean(resid(fit)^2)), 2)
coefs <- coef(fit)
b0 <- round(coefs[1], 2)
b1 <- round(coefs[2],7)
r2 <- round(summary(fit)$r.squared, 2)
R<-round(sqrt(summary(fit)$r.squared),2)
eqn <- bquote(italic(y) == .(b0) + .(b1)*italic(x) * "," ~~ 
              R^2 == .(r2) * "," ~~ r== .(R) * ","~~RMSE == .(rmse))

ggplot(movie,aes(x=(num_voted_users)/1000, y=imdb_score)) + 
  geom_point(shape=18, color="darkblue")+
  geom_smooth(method=lm,  linetype="dashed",
             color="darkred", fill="brown")+
  scale_y_continuous(name = "imdb score")+
  scale_x_continuous(name = "number of voted users(in thousands)")+
  theme(axis.text=element_text(size=12),
  axis.title=element_text(size=14,face="bold"))+
  annotate(geom="text", x=750, y=1, label="y= 6.13 + 0.0034x , r=0.43 , pvalue<0.0001")

###4.num users/critics for review box plot, vs imdb score scatter plot---------------------------------------
summary(num_user_for_reviews)
#outlier:>331+1.5*261=722.5

##(1-1). user for review box plot-----------------------
df_A = data.frame(value=num_user_for_reviews, id="Number of Users with Review")
fill <- c("burlywood1")
line <- "#1F3552"

p10 <- ggplot(df_A , aes(x ="", y = value)) +
        geom_boxplot(fill = fill, colour = line,
                     alpha = 0.7) +
        scale_y_continuous(name = "Number of Users",
                           breaks = seq(0,5060,200),
                           limits=c(0,5060)) +
        ggtitle("Number of Users with Review Box Plot")+
        theme(axis.text.x = element_text(size = 12, colour = "black"),
         plot.title = element_text(hjust = 0.5))+
        annotate("text",x=1.1,y=722.5, label= "722.5")+
        labs(x="",y="Number of Users") 
p10

##(1-2). user for review & imdb scatter plot--------------------------
vote<-movie[!is.na(num_user_for_reviews),c("num_user_for_reviews","imdb_score")]
dim(vote)

fit<- lm(vote$imdb_score~vote$num_user_for_reviews)
fit1<- lm(vote$imdb_score~log(vote$num_user_for_reviews))
summary(fit)
summary(fit1)
r<- round(sqrt(summary(fit)$r.squared), 2)
r1<- round(sqrt(summary(fit1)$r.squared), 2)
rmse <- round(sqrt(mean(resid(fit)^2)), 2)
coefs <- coef(fit)
b0 <- round(coefs[1], 2)
b1 <- round(coefs[2],7)
r2 <- round(summary(fit)$r.squared, 2)
R<-round(sqrt(summary(fit)$r.squared),2)
eqn <- bquote(italic(y) == .(b0) + .(b1)*italic(x) * "," ~~ 
              R^2 == .(r2) * "," ~~ r== .(R) * ","~~RMSE == .(rmse))

plot.new()
ggplot(data=vote,aes(x=num_user_for_reviews, y=imdb_score)) + 
  geom_point(shape=18, color="darkorchid4")+
  geom_smooth(method=lm,formula=y~log(x),linetype="dashed",
             color="darkred", fill="darkblue",size=1)+
  geom_smooth(method=lm,linetype="dashed",
             color="darkred", fill="darkkhaki",size=1)+
  scale_y_continuous(name = "imdb score")+
  scale_x_continuous(name = "Number of Users with Review")+
  theme(axis.text=element_text(size=12),
  axis.title=element_text(size=12,face="bold"))+
  annotate(geom="text", x=3500, y=1.5, label="y=6.17+0. 0.0009x , r=0.31, pvalue<0.0001")+
  annotate(geom="text", x=3500, y=1, label="y=5.1+0.26logx , r=0.33, pvalue<0.0001")


#----------------
summary(num_critic_for_reviews)
#outlier:>196+1.5*141=407.5

##(2-1).critics for review box plot-----------------------
df_A = data.frame(value=num_critic_for_reviews, id="Number of Critic with Review")
fill <- c("burlywood1")
line <- "#1F3552"

p10 <- ggplot(df_A , aes(x ="", y = value)) +
        geom_boxplot(fill = fill, colour = line,
                     alpha = 0.7) +
        scale_y_continuous(name = "Number of Critics",
                           breaks = seq(0,820,100),
                           limits=c(0,820)) +
        ggtitle("Number of Critics with Review Box Plot")+
        theme(axis.text.x = element_text(size = 12, colour = "black"),
         plot.title = element_text(hjust = 0.5))+
        annotate("text",x=1.1,y=407.5,label= "407.5")+
        labs(x="",y="Number of Critics") 
p10

##(2-2).critics for review &imdb score scatter plot--------------------------
dim(filter(movie,num_critic_for_reviews>=407.5))[1]
vote<-movie[!is.na(num_critic_for_reviews),c("num_critic_for_reviews","imdb_score")]
dim(vote)

fit <- lm(vote$imdb_score~vote$num_critic_for_reviews)
fit1<- lm(vote$imdb_score~log(vote$num_critic_for_reviews))
summary(fit)
summary(fit1)
rmse <- round(sqrt(mean(resid(fit)^2)), 2)
coefs <- coef(fit)
b0 <- round(coefs[1], 2)
b1 <- round(coefs[2],7)
r2 <- round(summary(fit)$r.squared, 2)
R<-round(sqrt(summary(fit)$r.squared),2)
eqn <- bquote(italic(y) == .(b0) + .(b1)*italic(x) * "," ~~ 
              R^2 == .(r2) * "," ~~ r== .(R) * ","~~RMSE == .(rmse))
eqn

plot.new()
ggplot(data=vote,aes(x=num_critic_for_reviews, y=imdb_score)) + 
  geom_point(shape=18, color="darkorchid4")+
  geom_smooth(method=lm,  linetype="dashed",
             color="darkred", fill="darkkhaki",size=1.5)+
  scale_y_continuous(name = "imdb score")+
  scale_x_continuous(name = "Number of Critics with Review")+
  theme(axis.text=element_text(size=12),
  axis.title=element_text(size=12,face="bold"))+
  annotate(geom="text", x=400, y=1, label="y=5.98 + 0.00309x , r=0.34 , pvalue<0.0001")







