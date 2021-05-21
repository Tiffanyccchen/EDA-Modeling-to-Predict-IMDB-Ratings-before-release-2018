###5.budget vs popularity, gross, profit, roe 檢查 數值outliers------------------------
options(scipen = 999)
attach(movie)

##budget----
#處理不同國家幣值問題
summary(as.factor(country))
movie[which(country=="Japan"),"budget"]<-as.vector(movie[which(country=="Japan"),"budget"])*0.0092
movie[which(country=="South Korea"),"budget"]<-as.vector(movie[which(country=="South Korea"),"budget"])*0.00094
movie[which(country=="Turkey"),"budget"]<-as.vector(movie[which(country=="Turkey"),"budget"])*0.27

#處理預算過大錯誤情形
summary(budget)
movie[which(budget<10000),c("title","budget")]
movie[which(title==" Escape from Tomorrow "),"budget"]<-650000
movie[which(title=="Stan Helsing"),"budget"]<-1000000
movie[which(title=="Oldboy"),"budget"]<-3000000
movie[which(title=="Jungle Shuffle"),"budget"]<-10000000

movie[which(budget/1000>300000),c("title","budget")]
europe<-c("The Messenger: The Story of Joan of Arc","Red Cliff","Kabhi Alvida Naa Kehna","Iceland","The Legend of Suriyothai","Kites","Tango")
movie[is.element(movie$title,europe),"budget"]<-c(85000000,553632000*0.16,700000000*0.016,400000000*0.032,600000000*0.016,700000000*0.0074)

#outlier
summary(budget) #outlier:43000000+(43000000-6000000)*1.5
dim(filter(movie,budget>98500000))[1]

##gross----
summary(gross)movie[which(gross<1000),c("title","gross","release_date","budget")]
movie[which(title=="A Farewell to Arms"),"gross"]<-NA
movie[which(title=="F.I.S.T."),"gross"]<-2038920
movie[which(title=="Out of the Blue"),"gross"]<-NA
movie[which(title=="Skin Trade"),"gross"]<-1242

#outlier: 60049282+(60049282-5480488)*1.5
dim(filter(movie,gross>141902473))[1]

##profit, roe recalculate----
profit<-movie$gross-movie$budget
sum(is.na(profit))#647 NAs
roe<-(profit/budget)*100
sum(is.na(roe))#647 NAs

movie$profit<-profit
movie$roe<-roe

write.csv(movie,"moviecut.csv")

##profit----
summary(profit)
#outlier : 23691278+(23583175-( -9264607))*1.5 / -9264607 -(23583175-( -9264607))*1.5
dim(filter(movie,profit>72962951))[1]
dim(filter(movie,profit<(-58536280)))[1]

##roe----
summary(roe)
#outlier : 133.8+(131.7-(-52.5))*1.5 / (-52.6)-(133.8-(-52.5))*1.5
dim(filter(movie,roe>410.26))[1]
dim(filter(movie,roe<(-332.3)))[1]

##popularity----
summary(popularity)
#outlier : 28.812 +(28.812-(4.912))*1.5
dim(filter(movie,popularity>64.662))[1]
summary(movie[which(popularity==0),c("title")])
movie[which(popularity==0),c("title")]

### 6.budget,profit,roe,gross,popularity box plot----------------------------------------

##budget/1000----
df_A = data.frame(value=budget/1000, id="Budget")
fill <- c("lightpink4")
line <- "#1F3552"

p10 <- ggplot(df_A , aes(x ="", y = value)) +
        geom_boxplot(fill = fill, colour = line,
                     alpha = 0.7) +
        scale_y_continuous(name = "Budget") +
        ggtitle("Budget Box Plot (in thousands)")+
        theme(axis.text.x = element_text(size = 12, colour = "black"),
        plot.title = element_text(hjust = 0.5))+
        labs(x="",y="")
p10

##gross/1000----

df_A = data.frame(value=gross/1000, id="gross")
fill <- c("magenta4")
line <- "#1F3552"

p10 <- ggplot(df_A , aes(x ="", y = value)) +
        geom_boxplot(fill = fill, colour = line,
                     alpha = 0.7) +
        scale_y_continuous(name = "Gross ") +
        ggtitle("Gross Box Plot (in thousands)")+
        theme(axis.text.x = element_text(size = 12, colour = "black"),
         plot.title = element_text(hjust = 0.5))+
        labs(x="",y="") 
p10

##profit/1000----
df_A = data.frame(value=profit/1000, id="Profit")
fill <- c("maroon")
line <- "#1F3552"

p10 <- ggplot(df_A , aes(x ="", y = value)) +
        geom_boxplot(fill = fill, colour = line,
                     alpha = 0.7) +
        scale_y_continuous(name = "Profit") +
        ggtitle("Profit Box Plot (in thousands)")+
        theme(axis.text.x = element_text(size = 12, colour = "black"),
        plot.title = element_text(hjust = 0.5))+
        labs(x="",y="") 
p10

##roe----
df_A = data.frame(value=roe, id="ROE")
fill <- c("maroon1")
line <- "#1F3552"

p10 <- ggplot(df_A , aes(x ="", y = value)) +
        geom_boxplot(fill = fill, colour = line,
                     alpha = 0.7) +
        scale_y_continuous(name = "ROI(%)") +
        ggtitle("ROI(%) Box Plot")+
        theme(axis.text.x = element_text(size = 12, colour = "black"),
         plot.title = element_text(hjust = 0.5))+
        labs(x="",y="") 
p10

movie[which(roe>600000),c("title","genres")]

##popularity---
df_A = data.frame(value=popularity, id="Popularity")
fill <- c("lightpink4")
line <- "#1F3552"

p10 <- ggplot(df_A , aes(x ="", y = value)) +
        geom_boxplot(fill = fill, colour = line,
                     alpha = 0.7) +
        scale_y_continuous(name = "Popularity") +
        ggtitle("Popularity Box Plot")+
        theme(axis.text.x = element_text(size = 12, colour = "black"),
        plot.title = element_text(hjust = 0.5))+
        labs(x="",y="") 
p10


### 7.budget profit gross roe popularity vs imdb score scatter plot--------------------------
## budget----
vote<-movie[!is.na(budget),c("budget","imdb_score")]
dim(vote)

fit <- lm(vote$imdb_score~(as.vector(vote$budget/1000)))
fit1 <- lm(vote$imdb_score~log((as.vector(vote$budget/1000))))
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
ggplot(data=vote,aes(x=as.vector(budget)/1000, y=imdb_score)) + 
  geom_point(shape=18, color="lightpink4")+
  geom_smooth(method=lm,  linetype="dashed",
             color="darkred", fill="darkkhaki",size=1.5)+
  scale_y_continuous(name = "imdb score")+
  scale_x_continuous(name = "budget(in thousands)")+
  theme(axis.text=element_text(size=12),
  axis.title=element_text(size=12,face="bold"))+
  annotate(geom="text", x=200000, y=1, label="y=6.39+0.0000012x, r=0.05, pvalue<0.001")

##gross----
vote<-movie[!is.na(gross),c("gross","imdb_score")]
dim(vote)

fit <- lm(vote$imdb_score~(as.vector(vote$gross/1000)))
fit1 <- lm(vote$imdb_score~log((as.vector(vote$gross/1000))))
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
ggplot(data=vote,aes(x=as.vector(gross)/1000, y=imdb_score)) + 
  geom_point(shape=18, color="magenta4")+
  geom_smooth(method=lm,  linetype="dashed",
             color="darkred", fill="darkkhaki",size=1.5)+
  scale_y_continuous(name = "imdb score")+
  scale_x_continuous(name = "gross(in thousands)")+
  theme(axis.text=element_text(size=12),
  axis.title=element_text(size=12,face="bold"))+
  annotate(geom="text", x=200000, y=1, label="y=6.35+0.0000029x, r=0.18, pvalue<0.001")

## gross vs budget----
vote<-movie[!is.na(gross)&!is.na(budget),c("gross","budget")]
dim(vote)

fit <- lm(as.vector(vote$gross/1000)~(as.vector(vote$budget/1000)))
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
ggplot(data=vote,aes(x=as.vector(gross)/1000, y=as.vector(profit)/1000)) + 
  geom_point(shape=18, color="magenta4")+
  geom_smooth(method=lm,  linetype="dashed",
             color="darkred", fill="darkkhaki")+
  scale_y_continuous(name = "gross (in thousands)")+
  scale_x_continuous(name = "budget (in thousands)")+
  theme(axis.text=element_text(size=12),
  axis.title=element_text(size=12,face="bold"))+
  annotate(geom="text", x=200000, y=-200000, label="y=10840.34 + 0.064x, r=0.05")


## profit----
vote<-movie[!is.na(profit),c("profit","imdb_score")]
dim(vote)

fit <- lm(vote$imdb_score~(as.vector(vote$profit/1000)))
summary(fit)
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
ggplot(data=vote,aes(x=as.vector(profit)/1000, y=imdb_score)) + 
  geom_point(shape=18, color="maroon")+
  geom_smooth(method=lm,  linetype="dashed",
             color="darkred", fill="darkkhaki",size=1.5)+
  scale_y_continuous(name = "imdb score")+
  scale_x_continuous(name = "profit (in thousands)")+
  theme(axis.text=element_text(size=12),
  axis.title=element_text(size=12,face="bold"))+
  annotate(geom="text", x=200000, y=1, label="y=6.43 + 0.0000049x, r=0.24, pvaule<0.0001")

##profit vs budget----
vote<-movie[!is.na(profit)&!is.na(budget),c("profit","budget")]
dim(vote)

fit <- lm(as.vector(vote$profit/1000)~(as.vector(vote$budget/1000)))
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
ggplot(data=vote,aes(x=as.vector(budget)/1000, y=as.vector(profit)/1000)) + 
  geom_point(shape=18, color="maroon")+
  geom_smooth(method=lm,  linetype="dashed",
             color="darkred", fill="darkkhaki")+
  scale_y_continuous(name = "profit (in thousands)")+
  scale_x_continuous(name = "budget (in thousands)")+
  theme(axis.text=element_text(size=12),
  axis.title=element_text(size=12,face="bold"))+
  annotate(geom="text", x=200000, y=-200000, label="y=10840.34 + 0.064x, r=0.05")

##roe----
vote<-movie[!is.na(roe),c("roe","imdb_score")]
dim(vote)
vote[,1]

fit <- lm(vote$imdb_score~(as.vector(vote$roe)))
summary(fit)
rmse <- round(sqrt(mean(resid(fit)^2)), 2)
coefs <- coef(fit)
b0 <- round(coefs[1], 2)
b1 <- round(coefs[2],7)
r2 <- round(summary(fit)$r.squared, 7)
R<-round(sqrt(summary(fit)$r.squared),7)
eqn <- bquote(italic(y) == .(b0) + .(b1)*italic(x) * "," ~~ 
              R^2 == .(r2) * "," ~~ r== .(R) * ","~~RMSE == .(rmse))
eqn

plot.new()
ggplot(data=vote,aes(x=as.vector(roe), y=imdb_score)) + 
  geom_point(shape=18, color="maroon1")+
  geom_smooth(method=lm,  linetype="dashed",
             color="darkred", fill="darkkhaki",size=1.5)+
  scale_y_continuous(name = "imdb score")+
  scale_x_continuous(name = "ROI(%)(return on investment)")+
  theme(axis.text=element_text(size=12),
  axis.title=element_text(size=12,face="bold"))+
  annotate(geom="text", x=400000, y=1, label="y=6.49 + 0.0000004x, r= 0.0049, pvalue<0.0001")

##popularity----
vote<-movie[!is.na(popularity),c("popularity","imdb_score")]
dim(vote)

fit <- lm(imdb_score~popularity)
summary(fit)
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
ggplot(data=vote,aes(x=popularity, y=imdb_score)) + 
  geom_point(shape=18, color="lightpink4")+
  geom_smooth(method=lm,  linetype="dashed",
             color="darkred", fill="darkkhaki",size=1.5)+
  scale_y_continuous(name = "imdb score")+
  scale_x_continuous(name = "popularity")+
  theme(axis.text=element_text(size=12),
  axis.title=element_text(size=12,face="bold"))+
  annotate(geom="text", x=400, y=1, label="y=6.22 + 0.0095x, r=0.28, pvalue<0.0001")
