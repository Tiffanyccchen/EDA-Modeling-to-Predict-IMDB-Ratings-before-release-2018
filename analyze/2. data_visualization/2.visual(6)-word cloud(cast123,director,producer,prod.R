###word cloud-------------------------------------
##categorize imdb score----
movie$category <- cut(movie$imdb_score,
                     breaks=c(-Inf, 5.5, 7.5, Inf),
                     labels=c("low","medium","high"))

dim(movie[movie$imdb_score>8,])[1]
dim(movie[movie$imdb_score>7,])[1]
dim(movie[movie$imdb_score>5,])[1]
dim(movie[movie$imdb_score>6,])[1]
dim(movie[movie$imdb_score>5.5,])[1]
dim(movie[movie$imdb_score>7.5,])[1]

##1.cast 1 word cloud---
compcast1<-table(movie$cast1,movie$category)

#comparison word cloud---
par(mfrow=c(1,1))
set.seed(12)
comparison.cloud(compcast1,max.words = 900, colors=brewer.pal(9,"Dark2"),random.order = F,title.size = 2,rot.per = 0.2)

#commonality word cloud---
par(mfrow=c(1,1))
set.seed(12)
commonality.cloud(compcast1, random.order=FALSE, scale=c(5, .5),colors = brewer.pal(4, "Dark2"), max.words=900)

##2.cast 2 word cloud---
compcast2<-table(movie$cast2,movie$category)

#comparison word cloud---
par(mfrow=c(1,1))
set.seed(12)
comparison.cloud(compcast2,max.words = 900, colors=brewer.pal(9,"Dark2"),random.order = F,title.size = 2,rot.per = 0.2)

#commonality word cloud---
par(mfrow=c(1,1))
set.seed(12)
commonality.cloud(compcast2, random.order=FALSE, scale=c(5, .5),colors = brewer.pal(4, "Dark2"), max.words=400)

##3.cast 3 word cloud---
compcast3<-table(movie$cast3,movie$category)

###comparison word cloud---
par(mfrow=c(1,1))
set.seed(12)
comparison.cloud(compcast3,max.words = 900, colors=brewer.pal(9,"Dark2"),random.order = F,title.size = 2,rot.per = 0.2)

#commonality word cloud---
par(mfrow=c(1,1))
set.seed(12)
commonality.cloud(compcast3, random.order=FALSE, scale=c(5, .5),colors = brewer.pal(4, "Dark2"), max.words=900)

##4.director word cloud---
directorcoun<-table(movie$director_name,movie$category)

#comparison word cloud---
par(mfrow=c(1,1))
set.seed(12)
comparison.cloud(directorcoun,max.words = 900, colors=brewer.pal(9,"Dark2"),random.order = F,title.size = 2,rot.per = 0.2)

#commonality word cloud---
par(mfrow=c(1,1))
set.seed(13)
commonality.cloud(directorcoun, random.order=FALSE, scale=c(5, .5),colors = brewer.pal(4, "Dark2"), max.words=400)

##5.producer word cloud---
producerrcoun<-table(movie$producer,movie$category)

#comparison word cloud---
par(mfrow=c(1,1))
set.seed(12)
comparison.cloud(producerrcoun,max.words = 900, colors=brewer.pal(9,"Dark2"),random.order = F,title.size = 2,rot.per = 0.2)

#commonality word cloud---
par(mfrow=c(1,1))
set.seed(12)
commonality.cloud(producerrcoun, random.order=FALSE, scale=c(5, .5),colors = brewer.pal(4, "Dark2"), max.words=400)

##6.editor word cloud---
editorcoun<-table(movie$editor,movie$category)

#comparison word cloud---
par(mfrow=c(1,1))
set.seed(12)
comparison.cloud(editorcoun,max.words = 900, colors=brewer.pal(9,"Dark2"),random.order = F,title.size = 2,rot.per = 0.2)

#commonality word cloud---
par(mfrow=c(1,1))
set.seed(12)
commonality.cloud(editorcoun, random.order=FALSE, scale=c(5, .5),colors = brewer.pal(4, "Dark2"), max.words=400)

##7.screenplay word cloud---
screenplaycoun<-table(movie$screen,movie$category)

#comparison word cloud---
par(mfrow=c(1,1))
set.seed(12)
comparison.cloud(screenplaycoun,max.words = 900, colors=brewer.pal(9,"Dark2"),random.order = F,title.size = 2,rot.per = 0.2)

#commonality word cloud---
par(mfrow=c(1,1))
set.seed(12)
commonality.cloud(screenplaycoun, random.order=FALSE, scale=c(5, .5),colors = brewer.pal(4, "Dark2"), max.words=400)


##8.production companies word cloud---
gensplit<- colsplit(movie$production_companies,split="\\,",names=c("g1","g2","g3","g4","g5","g6","g7","g8","g9","g10","g11","g12","g13","g14","g15","g16","g17","g18","g19","g20","g21","g22","g23","g24","g25"))
gensplit <- data.frame(lapply(gensplit, as.character), stringsAsFactors=FALSE)
for (i in 1: nrow(gensplit)) { gensplit[i,duplicated(as.character(gensplit[i,]))]<-""}

vote<-cbind(movie[,c("imdb_score","category")],gensplit)
vote<-melt(vote,id.vars=1:2)
dim(vote)
names(vote)
vote<-vote[-3]
vote<-vote %>% filter(value!="") %>% droplevels()
summary(vote$value)

prodcomp<-data.frame(table(vote$value,vote$category))
prodcomp[2,]
dim(prodcomp)
names(prodcomp)<-c("prodcomp","category","times") #renaming our variables properly

prodcomp[1]<-as.character(prodcomp[,1]) 
prodcomp[2]<-as.character(prodcomp[,2]) #assigning non factor class to our variables

prodcomp<-data.frame(cast(prodcomp,prodcomp~category,value="times")) #casting into wide format
wordmatrix<-data.matrix(prodcomp[,-1])
rownames(wordmatrix)<-prodcomp[,1]

#comparison word cloud---
par(mfrow=c(1,1))
set.seed(12)
comparison.cloud(wordmatrix,max.words = 900, colors=brewer.pal(9,"Dark2"),random.order = F,title.size = 2,rot.per = 0.2)

#commonality word cloud---
par(mfrow=c(1,1))
set.seed(12)
commonality.cloud(wordmatrix, random.order=FALSE, scale=c(5, .5),colors = brewer.pal(4, "Dark2"), max.words=400)

write.csv(movie,"4movie.csv")

