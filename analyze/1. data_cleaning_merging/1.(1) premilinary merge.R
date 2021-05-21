dir <- get_wd()

movie<-read.csv(dir + "data/data cleaning/movie_metadata.csv",header=T)
movie1<-read.csv(dir + "data/data cleaning/tmdb_5000_movies.csv",header=T,sep=",")
movie2<-read.csv(dir + "data/data cleaning/tmdb_5000_credits.csv",header=T,sep=",")
colnames(movie)
colnames(movie1)

###刪除重複電影
movie<-movie[!duplicated(movie),] #(45)(完全一樣)(5043-4998)
movie1<-movie1[!duplicated(movie1),]# (4)(完全一樣)(4802-4798)

movie[duplicated(movie$movie_title),"movie_title"] #(82)(名字一樣)(4998-4998)
movie1[duplicated(movie1$title),"title"] #(0)(4798-4798)(名字一樣)(原本有4個已檢查消去或替代（參考imdb data有的））

movie$movie_title<- gsub('\xc2\xa0','', movie$movie_title)

names(movie)[12]<-"title"
str(movie1)
str(movie)
names(movie1)
movie1$title

###將IMDB及TMDB合併
imtmovie<-merge(movie,movie1,by="title",all=F) #(4647)
dim(imtmovie)
imtmovie$title
str(imtmovie)
imtmovie<-imtmovie[,-c(48:54)] #刪除重複變數

##刪除重複名稱之電影
imtmovie[duplicated(imtmovie),1]
imtmovie[duplicated(imtmovie$title),1]
imtmovie<-imtmovie[!duplicated(imtmovie$title),] #(4567)
dim(imtmovie)
names(imtmovie)

imtmovie<-imtmovie[-1,]
write.csv(imtmovie,"1mermovie.csv")(4567)

