library(scales)
'%ni%' <- Negate('%in%')

ggplotColours <- function(n = 6, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65) #Create a vector of colors from vectors specifying hue, chroma and luminance. 
}
options(scipen = 999)
### color,language,country,aspect ratio,content rating,year,month,genres bar chart------------------
##1.color----
#bar chart---
summary(as.factor(color))
vote<-movie[!is.na(color),c("color","imdb_score")]
dim(vote)

fill<-c("peachpuff3","peachpuff4")
ggplot(vote, aes(x=factor(color)))+
  theme_minimal()+
  geom_bar(aes(y = (..count..)/sum(..count..)),fill=fill) + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25)+ # Label on top of each bar
  scale_y_continuous(labels=percent)+ # Continuous position scales (y).
  labs(title = "Color of Movies Bar Plot", y = "Percent", x = "Color") # Title and  axis labels

#box plot---
fill<-c("peachpuff3","peachpuff4")
p10 <- ggplot(vote, aes(x=color, y =imdb_score)) +
        geom_boxplot(fill = fill, colour = line, alpha = 0.7,
                     outlier.colour = "#1F3552", outlier.shape = 20) +
        scale_y_continuous(name = "imdb score") +
        scale_x_discrete(name = "color") +
        ggtitle("Boxplot of IMDB score by Color")+
        annotate(geom="text",x=-Inf, y=-Inf,hjust = 0, vjust =-1, label="one way anova test pvalue<0.0001")
p10

#ond way anova test of difference in score among categorical variable---
movie_color_aov <- aov(imdb_score ~color,vote) # ANOVA vote_average on decade
summary(movie_color_aov)

##2.language-----
#bar chart---
#examine and combine movies with languages seldom occur ( 5 lang.s with least propotion)
lang_dataframe <- as.data.frame(table(as.factor(language)))
select_leastlang <-lang_dataframe[which(lang_dataframe)$Freq<10),]
movie_leastlang_name <-as.vector(select[,1])

vote<-movie
vote<-movie[!is.na(vote$language),c("language","imdb_score")]
vote[is.element(vote$language, movie_leastlang_name),"language"]<-"Others"
summary(as.factor(vote$language))
names(table(vote$language))[order(table(vote$language))]
vote$language<-factor(vote$language, levels =c("Italian","German","Mandarin","Hindi","Spanish","French","English","Others"))


ggplot(vote, aes(x=factor(language)))+
  theme_minimal()+
  geom_bar(aes(y = (..count..)/sum(..count..)),fill=ggplotColours(n =8)) + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25)+
  scale_y_continuous(labels=percent)+  
  labs(title = "Language of Movies Bar Chart", y = "Percent", x = "Language   ANOVA:pvalue<0.0001")+
  theme(plot.title = element_text(hjust = 0.5),
  axis.text.x = element_text(angle = 45,hjust=1))

#ond way anova test of difference in score among categorical variable---
movie_language_aov <- aov(imdb_score ~language,vote) # ANOVA vote_average on decade
summary(movie_language_aov)

###box plot---

p10 <- ggplot(vote, aes(x=language, y =imdb_score)) +
        geom_boxplot(fill=ggplotColours(n =8),colour = line, alpha = 0.7,
                     outlier.colour = "#1F3552", outlier.shape = 20) +
        scale_y_continuous(name = "imdb score") +
        scale_x_discrete(name = "language") +
        ggtitle("Boxplot of IMDB score by Language")+
        theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45,hjust=1))+
        annotate(geom="text",x=-Inf, y=-Inf,hjust = 0, vjust =-0.5, label="one way anova test pvalue<0.0001")
p10

##3.country-----
###bar chart---
summary(as.factor(country))
dim(table(as.factor(country)))
ss<-as.data.frame(table(as.factor(country)))
select<-ss[which(ss$Freq<10),]
namee<-as.vector(select[,1])

vote<-movie
vote[is.element(movie$country,namee),"country"]<-"Others"
vote<-vote[!is.na(vote$country),c("country","imdb_score")]
dim(vote)
summary(as.factor(vote$country))
cyl_levels <- names(table(vote$country))[order(table(vote$country))]

vote$country<-factor(vote$country, levels =c("South Korea","Ireland","Mexico","Japan","New Zealand"
,"Hong Kong","China","Italy","India","Spain","Australia","Germany","Canada","France","UK","USA","Others"))

ggplot(vote, aes(x=factor(country)))+
  theme_minimal()+
  geom_bar(aes(y = (..count..)/sum(..count..)),fill=ggplotColours(n = 17)) + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25)+
  scale_y_continuous(labels=percent)+  
  labs(title = "Country of Movies Bar Chart", y = "Percent", x = "Country")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45,hjust=1))

###ond way anova test of difference in score among categorical variable---
'%ni%' <- Negate('%in%')
movie_country_aov <- aov(imdb_score ~country,vote) # ANOVA vote_average on decade
summary(movie_country_aov)

###box plot---
p10 <- ggplot(vote, aes(x=country, y =imdb_score)) +
        geom_boxplot(fill=ggplotColours(n = 17),colour = line, alpha = 0.7,
                     outlier.colour = "#1F3552", outlier.shape = 20) +
        scale_y_continuous(name = "imdb score") +
        scale_x_discrete(name = "country") +
        ggtitle("Boxplot of IMDB score by Country")+
        theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45,hjust=1))+
        annotate(geom="text",x=-Inf, y=-Inf,hjust = 0, vjust =-0.5, label="one way anova test pvalue<0.0001")
p10

##4.aspect ratio-----
#bar chart---
summary(as.factor(aspect_ratio))
dim(table(as.factor(aspect_ratio)))
aspect_ratio_dataframe <-as.data.frame(table(as.factor(aspect_ratio)))
select<-aspect_ratio_dataframe[aspect_ratio_dataframe$Var1 %ni% c("1.85","2.35"),]
namee<-as.vector(select[,1])

vote<-movie
vote<-vote[!is.na(vote$aspect_ratio),c("aspect_ratio","imdb_score")]
vote[is.element(vote$aspect_ratio,namee),"aspect_ratio"]<-"Others"
dim(vote)
summary(as.factor(vote$aspect_ratio))

fill<-c("springgreen2","springgreen3","springgreen4")

ggplot(vote, aes(x=factor(aspect_ratio)))+
  theme_minimal()+
  geom_bar(aes(y = (..count..)/sum(..count..)),fill=fill) + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25)+
  scale_y_continuous(labels=percent)+  
  labs(title = "Aspect Ratio of Movies Bar Chart", y = "Percent", x = "Aspect Ratio")+
  theme(plot.title = element_text(hjust = 0.5),)

#ond way anova test of difference in score among categorical variable---
movie_aspect_ratio_aov <- aov(imdb_score ~aspect_ratio,vote) # ANOVA vote_average on decade
summary(movie_aspect_ratio_aov)

#box plot---

p10 <- ggplot(vote, aes(x=aspect_ratio, y =imdb_score)) +
        geom_boxplot(fill=fill,colour = line, alpha = 0.7,
                     outlier.colour = "#1F3552", outlier.shape = 20) +
        scale_y_continuous(name = "imdb score") +
        scale_x_discrete(name = "aspect_ratio") +
        ggtitle("Boxplot of IMDB score by Aspect Ratio")+
        theme(plot.title = element_text(hjust = 0.5))+
        annotate(geom="text",x=-Inf, y=-Inf,hjust = 0, vjust =-0.5, label="one way anova test pvalue<0.0001")

p10

##5.content rating-----
#bar chart---
summary(as.factor(content_rating))
dim(table(as.factor(content_rating)))
ss<-as.data.frame(table(as.factor(content_rating)))
select<-ss[ss$Var1 %ni% c("G","PG","PG-13","R","NC-17"),]
namee<-as.vector(select[,1])

vote<-movie
vote<-vote[!is.na(vote$content_rating),c("content_rating","imdb_score")]
vote[is.element(vote$content_rating,namee),"content_rating"]<-"Others"
vote$content_rating<-factor(vote$content_rating, levels =c("G","PG","PG-13","R","NC-17","Others"))
dim(vote)
summary(as.factor(vote$content_rating))

fill<-c("yellow","yellow1","yellow2","yellow3","yellow4","yellowgreen")

ggplot(vote, aes(x=factor(content_rating)))+
  theme_minimal()+
  geom_bar(aes(y = (..count..)/sum(..count..)),fill=fill) + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25)+
  scale_y_continuous(labels=percent)+  
  labs(title = "Content Rating of Movies Bar Chart", y = "Percent", x = "Content Rating")+
  theme(plot.title = element_text(hjust = 0.5))

#ond way anova test of difference in score among categorical variable---
movie_content_aov <- aov(imdb_score ~content_rating,vote) # ANOVA vote_average on decade
summary(movie_content_aov)

#box plot---

p10 <- ggplot(vote, aes(x=content_rating, y =imdb_score)) +
        geom_boxplot(fill=fill,colour = line, alpha = 0.7,
                     outlier.colour = "#1F3552", outlier.shape = 20) +
        scale_y_continuous(name = "imdb score") +
        scale_x_discrete(name = "content_rating") +
        ggtitle("Boxplot of IMDB score by Content Rating")+
        theme(plot.title = element_text(hjust = 0.5))+
        annotate(geom="text",x=-Inf, y=-Inf,hjust = 0, vjust =-0.5, label="one way anova test pvalue<0.0001")

p10

##6.year hist,vs imdb scatter plot (將year以continuous data 看待)---------------------------
#transfer release date field and add 3 fields : year, month day
class(movie$release_date)
movie$release_date<-as.Date(movie$release_date, "%Y/%m/%d") 
movie$year=as.factor(format(movie$release_date,"%Y"))
movie$date=as.factor(format(movie$release_date,"%d"))
movie$month=month.abb[(as.factor(format(movie$release_date,"%m")))]

#year's histogram---
movie$year<-as.vector(movie$year)
year<-movie$year
year<-as.numeric(year)

h<-hist(year,col="snow4",xlab="year",main="Histogram & Normal Curve of Release Year",xaxt="n")

#plot histogram's curve
xfit<-seq(min(year),max(year),length=40) 
yfit<-dnorm(xfit,mean=mean(year),sd=sd(year)) 
yfit <- yfit*diff(h$mids[1:2])*length(year) 
lines(xfit, yfit, col="yellow", lwd=2)

abline(v = mean(year), #plot mean
 col = "royalblue",
 lwd = 2)
abline(v = median(year), #plot median
 col = "darkred",
 lwd = 2)

text(seq(1930,2010,by=10), par("usr")[3] - 0.2, labels =seq(1930,2010,by=10),
cex=1, xpd = TRUE)
legend(x = "topleft", # location of legend within plot area
 c("Mean=2002", "Median=2005"),
 col = c("royalblue", "darkred"),
 lwd = c( 2, 2))

#year vs score scatter---
movie$year<-as.numeric(movie$year)
vote<-movie[!is.na(year),c("year","imdb_score")]
dim(vote)

fit <- lm(vote$imdb_score~vote$year)
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
ggplot(data=vote,aes(x=year,y=imdb_score)) + 
  geom_point(shape=18, color="tan")+
  geom_smooth(method=lm,linetype="dashed",
             color="darkred", fill="darkkhaki")+
  scale_y_continuous(name = "imdb score")+
  scale_x_continuous(name = "year")+
  theme(axis.text=element_text(size=12),
  axis.title=element_text(size=12,face="bold"))+
  annotate(geom="text",x=1990, y=1, label="y=42.51 + -0.08x, r=-0.2, pvalue<0.0001")

##7.decade bar plot,vs imdb box plot--------------------------------
movie$decade=(year(release_date) %/% 10 ) * 10  

#decade bar plot---
summary(as.factor(movie$decade))
ss<-as.data.frame(table(as.factor(movie$decade)))
'%ni%' <- Negate('%in%')
select<-ss[ss$Var1 %ni% c("1970","1980","1990","2000","2010"),]
namee<-as.vector(select[,1])
vote<-movie
vote[is.element(vote$decade,namee),"decade"]<-"Before 1970"
dim(vote)
summary(as.factor(vote$decade))
vote<-vote[!is.na(vote$decade),]
vote$decade<-factor(vote$decade, levels =c("Before 1970","1970","1980","1990","2000","2010"))

fill<-c("snow","snow1","snow2","snow3","snow4","whitesmoke")
ggplot(vote, aes(x=factor(vote$decade)))+
  theme_minimal()+
  geom_bar(aes(y = (..count..)/sum(..count..)),fill=fill) + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25)+
  scale_y_continuous(labels=percent)+  
  labs(title = "Decades of Movies Bar Plot", y = "Percent", x = "Decade")+
  theme(plot.title = element_text(hjust = 0.5))

#ond way anova test of difference in score among categorical variable---
movie_decade_aov <- aov(imdb_score ~decade,vote) # ANOVA vote_average on decade
summary(movie_decade_aov)

#vs imdb score box plot---
p10 <- ggplot(vote, aes(x=decade, y =imdb_score)) +
        geom_boxplot(fill = fill, colour = line, alpha = 0.7,
                     outlier.colour = "#1F3552", outlier.shape = 20) +
        scale_y_continuous(name = "imdb score") +
        scale_x_discrete(name = "decade") +
        ggtitle("Boxplot of IMDB score by Decade")+
        theme(plot.title = element_text(hjust = 0.5))+
        annotate(geom="text",x=-Inf, y=-Inf,hjust = 0, vjust =-0.5, label="one way anova test pvalue<0.0001")
p10

##8.month bar plot,vs imdb box plot--------------------------------

#month bar plot---
summary(as.factor(movie$month))
vote<-movie
vote$month<- factor(vote$month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
summary(as.factor(vote$month))

ggplot(vote, aes(x=factor(vote$month)))+
  theme_minimal()+
  geom_bar(aes(y = (..count..)/sum(..count..)),fill=ggplotColours(n =12)) + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25)+
  scale_y_continuous(labels=percent)+  
  labs(title = "Month of Movies Bar Plot", y = "Percent", x = "Month")+
  theme(plot.title = element_text(hjust = 0.5))

#ond way anova test of difference in score among categorical variable---
movie_month_aov <- aov(imdb_score ~month,vote) # ANOVA vote_average on decade
summary(movie_month_aov)

#box plot---
p10 <- ggplot(vote, aes(x=month, y =imdb_score)) +
        geom_boxplot(fill =ggplotColours(n =12), colour = line, alpha = 0.7,
                     outlier.colour = "#1F3552", outlier.shape = 20) +
        scale_y_continuous(name = "imdb score") +
        scale_x_discrete(name = "month") +
        ggtitle("Boxplot of IMDB score by Month")+
        theme(plot.title = element_text(hjust = 0.5))+
        annotate(geom="text",x=-Inf, y=-Inf,hjust = 0, vjust =-0.5, label="one way anova test pvalue<0.0001")
p10


##9.genres bar plot and vs imdb box plot---
library(dplyr)
library(reshape)
library(wordcloud)
library(RColorBrewer)

gensplit<- colsplit(movie$genres,split="\\|",names=c("g1","g2","g3","g4","g5","g6","g7","g8"))
gensplit <- data.frame(lapply(gensplit, as.character), stringsAsFactors=FALSE)
for (i in 1: nrow(gensplit)) {gensplit[i,duplicated(as.character(gensplit[i,]))]<-""}

vote<-cbind(movie[,c("imdb_score","title")],gensplit)
summary(as.factor(vote$g1))
vote<-melt(vote,id.vars=1:2)
dim(vote)
names(vote)
vote<-vote[-3]
vote<-vote %>% filter(value!="") %>% droplevels()
summary(vote$value)

#all genres bar plot----
summary(as.factor(vote$value))
cyl_levels <- names(table(vote$value))[order(table(vote$value))]
vote$value<-factor(vote$value, levels =cyl_levels )

ggplot(vote, aes(x=factor(vote$value)))+
  theme_minimal()+
  geom_bar(aes(y = (..count..)/sum(..count..)),fill=ggplotColours(n =24)) + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25,size=3)+
  scale_y_continuous(labels=percent)+  
  labs(title = "Genre of Movies Bar Plot", y = "Percent", x = "genre")+
  theme(plot.title = element_text(hjust = 0.5),
  axis.text.x = element_text(angle = 60,hjust=1,size=11))

#ond way anova test of difference in score among categorical variable---
movie_allgenre_aov <- aov(imdb_score ~value,vote) # ANOVA vote_average on decade
summary(movie_allgenre_aov)

#all genres vs imdb score box plot---
ggplot(vote, aes(x=value, y =imdb_score)) +
        geom_boxplot(fill =ggplotColours(n =24), colour = line,
                     outlier.colour = "#1F3552", outlier.shape = 20) +
        scale_y_continuous(name = "imdb score") +
        scale_x_discrete(name = "genres") +
        ggtitle("Boxplot of IMDB score by Genre")+
        theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60,hjust=1,size=12))+
        annotate(geom="text",x=-Inf, y=-Inf,hjust = 0, vjust =-0.5, label="one way anova test pvalue<0.0001")

ggplot(aes(y=imdb_score,x=value,fill=value),data=vote)+geom_boxplot()+theme(axis.text.x = element_text(angle=70,hjust=1))

#ond way anova test of difference in score among categorical variable---
movie_gengross_aov <- aov(gross ~value,vote) # ANOVA vote_average on decade
summary(movie_gengross_aov)

#all genres vs gross box plot---
vote<-cbind(movie[,c("gross","title")],gensplit)
summary(as.factor(vote$g1))
vote<-melt(vote,id.vars=1:2)
vote<-vote[-3]
vote<-vote %>% filter(value!="") %>% droplevels()
summary(vote$value)

summary(as.factor(vote$value))
cyl_levels <- names(table(vote$value))[order(table(vote$value))]
vote$value<-factor(vote$value, levels =cyl_levels )

ggplot(vote, aes(x=value, y =gross/1000)) +
        geom_boxplot(fill =ggplotColours(n =23), colour = line,
                     outlier.colour = "#1F3552", outlier.shape = 20) +
        scale_y_continuous(name = "gross(in thousands)") +
        scale_x_discrete(name = "genres") +
        ggtitle("Boxplot of Gross(In Thousands) by Genre")+
        theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60,hjust=1,size=12))+
        annotate(geom="text",x=-Inf, y=-Inf,hjust = 0, vjust =-20, label="one way anova test pvalue<0.0001")

#1st genre bar chart---
summary(as.factor(gensplit$g1))
cyl_levels <- names(table(gensplit$g1))[order(table(gensplit$g1))]
gensplit$g1<-factor(gensplit$g1, levels =cyl_levels )

ggplot(gensplit, aes(x=factor(gensplit$g1)))+
  theme_minimal()+
  geom_bar(aes(y = (..count..)/sum(..count..)),fill=ggplotColours(n =19)) + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25,size=3)+
  scale_y_continuous(labels=percent)+  
  labs(title = "Main Genre of Movies Bar Plot", y = "Percent", x = "main genre")+
  theme(plot.title = element_text(hjust = 0.5,size=12),
  axis.text.x = element_text(angle = 60,hjust=1,size=12))

vote<-cbind(as.character(gensplit$g1),movie$imdb_score)
dim(vote)
vote<-as.data.frame(vote)
names(vote)[1]<-"g1"
names(vote)[2]<-"imdb_score"
str(vote)

cyl_levels <- names(table(vote$g1))[order(table(vote$g1))]
vote$g1<-factor(vote$g1, levels =cyl_levels )
vote$g1<-as.factor(vote$g1)
vote$imdb_score<-as.numeric(vote$imdb_score)

#ond way anova test of difference in score among categorical variable---
'%ni%' <- Negate('%in%')
vote<-vote[vote$g1 %ni% c("Film-Noir","History","Romance","Musical"),]
movie_genre_aov <- aov(imdb_score ~g1,vote) # ANOVA vote_average on decade
summary(movie_genre_aov)

#1st genre vs imdb score box plot---
ggplot(vote, aes(x=as.factor(g1), y =imdb_score)) +
        geom_boxplot(fill =ggplotColours(n =19), colour = line,
                     outlier.colour = "#1F3552", outlier.shape = 20) +
        scale_y_continuous(name = "imdb score") +
        scale_x_discrete(name = "main genre") +
        ggtitle("Boxplot of IMDB Score by Main Genre")+
        theme(plot.title = element_text(hjust = 0.5,size=12),
        axis.text.x = element_text(angle = 60,hjust=1,size=12))+
        annotate(geom="text",x=-Inf, y=-Inf,hjust = 0, vjust =-1, label="one way anova test pvalue<0.0001")