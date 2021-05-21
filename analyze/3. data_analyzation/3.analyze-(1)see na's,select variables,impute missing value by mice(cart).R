library(Amelia)
library(mice)
library(ggplot2)
library(lattice)

###1.analyze-select candidate predictor, missing value impute
##choose applicable predict variable, and pick from language vs.country & year vs decade
cv.test = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
    (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
  print.noquote("Cramer V / Phi:")
  return(as.numeric(CV))
}

ss<-as.data.frame(table(as.factor(country)))
select<-ss[which(ss$Freq<10),]
namee<-as.vector(select[,1])
vote<-movie
vote[is.element(movie$country,namee),"country"]<-"Others"
vote<-vote[!is.na(vote$country),]

ss<-as.data.frame(table(as.factor(language)))
select<-ss[which(ss$Freq<10),]
namee<-as.vector(select[,1])
vote<-vote[!is.na(vote$language),]
vote[is.element(vote$language,namee),"language"]<-"Others"

cv.test(vote$language,vote$country)
#1/(1-0.637^2)

##choose language or country(language,df less, significance does nor differ much---
movie_language_aov <- aov(imdb_score ~language,vote) # ANOVA vote_average on decade
summary(movie_language_aov)

movie_country_aov <- aov(imdb_score ~country,vote) # ANOVA vote_average on decade
summary(movie_country_aov)

##choose year or decade(year,quantitative,pearson corlarger for year, spearman's rho larger for decade, but decade lose too much information,so year---
cor(year,movie$decade)
cor(year,imdb_score)
cor(movie$decade,imdb_score)


cor.test( ~ imdb_score+year ,
         data=movie,
         method = "spearman",
         continuity = FALSE,
         conf.level = 0.95)

cor.test( ~ imdb_score+decade ,
         data=movie,
         method = "spearman",
         continuity = FALSE,
         conf.level = 0.95)

###2. missing value impute---
##1. select predictors,see missing value distribution---
movieana<-movie[,c("imdb_score","vote_average","runtime","budget","facenumber_in_poster","cast_total_facebook_likes",
"actor_1_facebook_likes","actor_2_facebook_likes","actor_3_facebook_likes","director_facebook_likes",
"year","content_rating","color","language","aspect_ratio","genres","month")]

dim(movieana)

sum(is.na(movieana))
sum(is.na(movieana$imdb_score))
sum(is.na(movieana$vote_average))
sum(is.na(movieana$runtime))
sum(is.na(movieana$budget))
sum(is.na(movieana$facenumber_in_poster))
sum(is.na(movieana$cast_total_facebook_likes))
sum(is.na(movieana$actor_1_facebook_likes))
sum(is.na(movieana$actor_2_facebook_likes))
sum(is.na(movieana$actor_3_facebook_likes))
sum(is.na(movieana$director_facebook_likes))
sum(is.na(movieana$year))
sum(is.na(movieana$content_rating))
sum(is.na(movieana$color))
sum(is.na(movieana$language))
sum(is.na(movieana$aspect_ratio))
sum(is.na(movieana$genres))
sum(is.na(movieana$month))

##2. misssing value impute---(mice)
plot.new()
par(mfrow=c(1,1))
missmap(movieana,col=c('yellow', 'darkred'))
names(movieana)<-c("imdbscore","voteaverage","runtime","budget","faceinposter","castfblikes","actor1fblikes","actor2fblikes","actor3fblikes","directorfblikes","year","contentrating","color","language","aspectratio","genres","month")

attach(movieana)
movieana[,1:11][] <- lapply(movieana[,1:11], as.numeric) # the "[]" keeps the dataframe structure
col_names <- names(movieana[,1:11])
movieana[,12:17][] <- lapply(movieana[,12:17],factor) # the "[]" keeps the dataframe structure
col_names <- names(movieana[,12:17])

md.pattern(movieana)
timetep<-proc.time()
mice.data <- mice(movieana,
                  m = 1,           # 產生1個被填補好的資料表
                  method="cart",# 使用CART決策樹，進行遺漏值預測
                  seed = 188)      # set.seed()，令抽樣每次都一樣
proc.time()-timetep

#原始資料(有遺漏值)
movieimpute<- complete(mice.data, 1)
missmap(movieimpute, col=c('yellow', 'darkred'))

#Confirm no NAs
sum(sapply(movieimpute, function(x) { sum(is.na(x)) }))
fwrite(movieimpute,"../data/movieana.csv")


