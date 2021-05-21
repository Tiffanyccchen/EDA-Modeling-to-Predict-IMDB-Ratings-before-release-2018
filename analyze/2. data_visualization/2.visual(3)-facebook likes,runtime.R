options(scipen = 999)

### 8. Examine FB line and run time data----00000000
summary(movie_facebook_likes)
length(which(movie_facebook_likes==0))

summary(cast_total_facebook_likes)
length(which(cast_total_facebook_likes==0))

summary(actor_1_facebook_likes)
length(which(actor_1_facebook_likes==0))

summary(actor_2_facebook_likes)
length(which(actor_2_facebook_likes==0))

summary(actor_3_facebook_likes)
length(which(actor_3_facebook_likes==0))

summary(runtime)
length(which(runtime<30))
movie[which(runtime<30),c("title","imdb_score")]

### 9. facebook like, runtime box plot----------------------------------------

## movie facebook likes----
df_A = data.frame(value=movie_facebook_likes, id="Movie Facebook Likes")
fill <- c("palegreen")
line <- "#1F3552"

p10 <- ggplot(df_A , aes(x ="", y = value)) +
        geom_boxplot(fill = fill, colour = line,
                     alpha = 0.7) +
        scale_y_continuous(name = "Facebook Likes") +
        ggtitle("Movie Facebook Likes")+
        theme(axis.text.x = element_text(size = 12, colour = "black"),
        plot.title = element_text(hjust = 0.5))+
        labs(x="",y="")
p10

## cast facebook likes----
df_A = data.frame(value=cast_total_facebook_likes, id="Cast Total Facebook Likes")
fill <- c("palegreen1")
line <- "#1F3552"

p10 <- ggplot(df_A , aes(x ="", y = value)) +
        geom_boxplot(fill = fill, colour = line,
                     alpha = 0.7) +
        scale_y_continuous(name = "Facebook Likes") +
        ggtitle("Cast Total Facebook Likes")+
        theme(axis.text.x = element_text(size = 12, colour = "black"),
         plot.title = element_text(hjust = 0.5))+
        labs(x="",y="") 
p10
movie[which(cast_total_facebook_likes>600000),"title"]

## actor 1 facebook likes----
df_A = data.frame(value=actor_1_facebook_likes, id="Actor 1 Facebook Likes")
fill <- c("palegreen2")
line <- "#1F3552"

p10 <- ggplot(df_A , aes(x ="", y = value)) +
        geom_boxplot(fill = fill, colour = line,
                     alpha = 0.7) +
        scale_y_continuous(name = "Facebook Likes") +
        ggtitle("Actor 1 Facebook Likes")+
        theme(axis.text.x = element_text(size = 12, colour = "black"),
        plot.title = element_text(hjust = 0.5))+
        labs(x="",y="") 
p10
movie[which(actor_1_facebook_likes>600000),"actor_1_name"]

## actor 2 facebook likes----
df_A = data.frame(value=actor_2_facebook_likes, id="Actor 2 Facebook Likes")
fill <- c("palegreen3")
line <- "#1F3552"

p10 <- ggplot(df_A , aes(x ="", y = value)) +
        geom_boxplot(fill = fill, colour = line,
                     alpha = 0.7) +
        scale_y_continuous(name = "Facebook Likes") +
        ggtitle("Actor 2 Facebook Likes")+
        theme(axis.text.x = element_text(size = 12, colour = "black"),
        plot.title = element_text(hjust = 0.5))+
        labs(x="",y="") 
p10
movie[which(actor_2_facebook_likes>100000),"actor_2_name"]

## actor 3 facebook likes----
df_A = data.frame(value=actor_3_facebook_likes, id="Actor 3 Facebook Likes")
fill <- c("palegreen4")
line <- "#1F3552"

p10 <- ggplot(df_A , aes(x ="", y = value)) +
        geom_boxplot(fill = fill, colour = line,
                     alpha = 0.7) +
        scale_y_continuous(name = "Facebook Likes") +
        ggtitle("Actor 3 Facebook Likes")+
        theme(axis.text.x = element_text(size = 12, colour = "black"),
        plot.title = element_text(hjust = 0.5))+
        labs(x="",y="") 
p10
movie[which(actor_3_facebook_likes>20000),"actor_3_name"]

## runtime----
df_A = data.frame(value=runtime, id="Runtime")
fill <- c("paleturquoise4")
line <- "#1F3552"

p10 <- ggplot(df_A , aes(x ="", y = value)) +
        geom_boxplot(fill = fill, colour = line,
                     alpha = 0.7) +
        scale_y_continuous(name = "runtime") +
        ggtitle("Runtime")+
        theme(axis.text.x = element_text(size = 12, colour = "black"),
        plot.title = element_text(hjust = 0.5))+
        labs(x="",y="") 
p10

### 10. facebook like, runtime vs imdb scatter plot-----------------------------
## movie facebook likes----
vote<-movie[!is.na(movie_facebook_likes),c("movie_facebook_likes","imdb_score")]
dim(vote)

fit <- lm(vote$imdb_score~vote$movie_facebook_likes)
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
ggplot(data=vote,aes(x=movie_facebook_likes, y=imdb_score)) + 
  geom_point(shape=18, color="palegreen")+
  geom_smooth(method=lm,  linetype="dashed",
             color="darkred", fill="darkkhaki")+
  scale_y_continuous(name = "imdb score")+
  scale_x_continuous(name = "movie facebook likes")+
  theme(axis.text=element_text(size=12),
  axis.title=element_text(size=12,face="bold"))+
  annotate(geom="text", x=200000, y=1, label="y=6.31 + 0.000014x, r=0.26, pvalue<0.0001")

## cast facebook likes----
vote<-movie[!is.na(cast_total_facebook_likes),c("cast_total_facebook_likes","imdb_score")]
dim(vote)

fit <- lm(vote$imdb_score~vote$cast_total_facebook_likes)
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
ggplot(data=vote,aes(x=cast_total_facebook_likes, y=imdb_score)) + 
  geom_point(shape=18, color="palegreen1")+
  geom_smooth(method=lm,  linetype="dashed",
             color="darkred", fill="darkkhaki")+
  scale_y_continuous(name = "imdb score")+
  scale_x_continuous(name = "cast total facebook likes")+
  theme(axis.text=element_text(size=12),
  axis.title=element_text(size=12,face="bold"))+
  annotate(geom="text", x=200000, y=1, label="y=6.37 + 0.0000056x, r=0.09, pvalue<0.0001")

## actor 1 facebook likes----
vote<-movie[!is.na(actor_1_facebook_likes),c("actor_1_facebook_likes","imdb_score")]
dim(vote)

fit <- lm(vote$imdb_score~vote$actor_1_facebook_likes)
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
ggplot(data=vote,aes(x=actor_1_facebook_likes, y=imdb_score)) + 
  geom_point(shape=18, color="palegreen2")+
  geom_smooth(method=lm,  linetype="dashed",
             color="darkred", fill="darkkhaki")+
  scale_y_continuous(name = "imdb score")+
  scale_x_continuous(name = "actor 1 facebook likes")+
  theme(axis.text=element_text(size=12),
  axis.title=element_text(size=12,face="bold"))+
  annotate(geom="text", x=200000, y=1, label="y=6.38+ 0.0000059x, r=0.08, pvalue<0.0001")

## actor 2 facebook likes----
vote<-movie[!is.na(actor_2_facebook_likes),c("actor_2_facebook_likes","imdb_score")]
dim(vote)

fit <- lm(vote$imdb_score~vote$actor_2_facebook_likes)
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
ggplot(data=vote,aes(x=actor_2_facebook_likes, y=imdb_score)) + 
  geom_point(shape=18, color="palegreen2")+
  geom_smooth(method=lm,  linetype="dashed",
             color="darkred", fill="darkkhaki")+
  scale_y_continuous(name = "imdb score")+
  scale_x_continuous(name = "actor 2 facebook likes")+
  theme(axis.text=element_text(size=12),
  axis.title=element_text(size=12,face="bold"))+
  annotate(geom="text", x=100000, y=1, label="y=6.38 + 0.0000243x, r=0.09, pvalue<0.0001")


## actor 3 facebook likes----
vote<-movie[!is.na(actor_3_facebook_likes),c("actor_3_facebook_likes","imdb_score")]
dim(vote)

fit <- lm(vote$imdb_score~vote$actor_3_facebook_likes)
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
ggplot(data=vote,aes(x=actor_3_facebook_likes, y=imdb_score)) + 
  geom_point(shape=18, color="palegreen4")+
  geom_smooth(method=lm,  linetype="dashed",
             color="darkred", fill="darkkhaki")+
  scale_y_continuous(name = "imdb score")+
  scale_x_continuous(name = "actor 3 facebook likes")+
  theme(axis.text=element_text(size=12),
  axis.title=element_text(size=12,face="bold"))+
  annotate(geom="text", x=5000, y=1, label="y=6.4 + 0.0000407x, r=0.06, pvalue<0.0001")


## runtime----
vote<-movie[!is.na(runtime),c("runtime","imdb_score")]
dim(vote)

fit <- lm(vote$imdb_score~vote$runtime)
fit1 <- lm(vote$imdb_score~log(vote$runtime))
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
ggplot(data=vote,aes(x=runtime, y=imdb_score)) + 
  geom_point(shape=18, color="paleturquoise4")+
  geom_smooth(method=lm,  linetype="dashed",
             color="darkred", fill="darkkhaki")+
  scale_y_continuous(name = "imdb score")+
  scale_x_continuous(name = "runtime")+
  theme(axis.text=element_text(size=12),
  axis.title=element_text(size=12,face="bold"))+
  annotate(geom="text", x=150, y=1, label="y=4.62 + 0.017x, r=0.35, pvalue<0.0001")

### 11.face number------------------------------
summary(facenumber_in_poster)

##facenumber box plot-----
df_A = data.frame(value=facenumber_in_poster, id="Face Number in Poster")
fill <- c("pink4")
line <- "#1F3552"

p10 <- ggplot(df_A , aes(x ="", y = value)) +
        geom_boxplot(fill = fill, colour = line,
                     alpha = 0.7) +
        scale_y_continuous(name ="Face Number") +
        ggtitle("Face Number in Poster Box Plot")+
        theme(axis.text.x = element_text(size = 12, colour = "black"),
        plot.title = element_text(hjust = 0.5))+
        labs(x="",y="") 
p10

# examine movies with high face numbers
movie[which(facenumber_in_poster>15),c("title","genres","imdb_score")]
movie[which(title=="Wal-Mart:The High Cost of Low Price"),c("title","genres","imdb_score")]

##facenumber vs score scatter---
vote<-movie[!is.na(facenumber_in_poster),c("facenumber_in_poster","imdb_score")]
dim(vote)

fit <- lm(vote$imdb_score~vote$facenumber_in_poster)
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
ggplot(data=vote,aes(x=facenumber_in_poster,y=imdb_score)) + 
  geom_point(shape=18, color="lightpink4")+
  geom_smooth(method=lm,linetype="dashed",
             color="darkred", fill="darkkhaki")+
  scale_y_continuous(name = "imdb score")+
  scale_x_continuous(name = "face number in poster")+
  theme(axis.text=element_text(size=12),
  axis.title=element_text(size=12,face="bold"))+
  annotate(geom="text",x=5, y=1, label="y=6.49 + -0.049x, r=-0.09, pvaule<0.0001")



