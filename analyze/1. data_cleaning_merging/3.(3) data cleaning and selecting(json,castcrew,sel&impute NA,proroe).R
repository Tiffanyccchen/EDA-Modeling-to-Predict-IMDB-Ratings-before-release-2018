dir <- get_wd()

castcrew<-read_csv(dir + "data/data cleaning/tmdb_5000_credits.csv",guess_max=3000)
movie<-read_csv(dir + "data/data visualization/1mermovie.csv",guess_max=3000)

###json format convert-production companies--------------------------------------------
install.packages("tidyverse")
install.packages("reshape2")
install.packages("stringi")
library(tidyverse)
library(stringi)
library(jsonlite)

productioncompanies<- movie%>%    
  filter(nchar(production_companies)>2) %>%     # fiter out blank keywords field
  mutate(                           # create a new field 
    js = lapply(production_companies, fromJSON) #   containing a LIST of keyword and value pairs
  ) %>%                             #   called id and name
  unnest(js) %>%                    # turn each keyword/value pairs in the LIST into a row
  select(id,title, production_companies=name)   # select the columns we want

pro<-slice(productioncompanies)
names(pro)

pro1<-aggregate(production_companies~id, data =pro, FUN =toString) 
names(pro1)
str(pro1)

###each company has different column(後來沒用到)--------------------------------
prodcomps<-pro1[,2]
n.obs <- sapply(prodcomps, length)
seq.max <- seq_len(max(n.obs))

mat <- t(sapply(prodcomps, "[", i = seq.max))
mat<-data.frame(mat)
str(mat)

pro1<-pro1[,-c(2)]
pro1<-cbind(pro1,mat)
dim(pro1)
pro1<-pro1[,-c(28:53)]

pro[1:10,]
names(pro)[1]="id"
names(pro)
colnames(pro)[2:27] <- paste("procomp",1:26, sep = "")
attach(pro)
str(pro)

###merge convert json format to movie dataset---------------------
movie<-merge(movie,pro1,by="id",all.x="T")
dim(movie)

write.csv(movie,"2movieori.csv")

###json format convert-first 3 cast--------------------------------------------
## 把castcrew刪到跟合併資料集電影一模一樣 ->credits.csv--------------------------
names(movie)
dim(castcrew)
castcrew<-castcrew[castcrew$movie_id %in% movie$id,]
dim(castcrew)
write.csv(castcrew,"mercredits.csv")

## 得到所有人員名單-------------------------------------------------------------
credits<-read_csv(dir + "data/data cleaning/mercredits.csv",guess_max=3000)
dim(credits)

all_crew <-credits%>%      # start with the raw table 
  filter(nchar(cast)>2) %>%        # filter out movies with empty crew  
  mutate(                          #       
    js  =  lapply(cast, fromJSON)  # turn the JSON into a list
  )  %>%                           #
  unnest(js)                       # and then make each row a single crew member 


## 1st cast----------------------------------------------       
top1cast <- all_crew %>%         # start with the crew information
  filter(order==0) %>%            # get the directors
  mutate(cast1=name) %>%            # rename the "name" for convienance

dim(top1cast)
names(top1cast)
names(top1cast)[13]="cast1"

top1cast<-top1cast[,c(2,13)]
names(top1cast)[1]="id"
top1cast<-as.data.frame(top1cast)

mmmtry<-merge(movie,top1cast,by="id",all.x=T)
mmmtry[duplicated(mmmtry$id),"title"] #remove movie with duplicate id
which(mmmtry$title== "A Beautiful Mind")
mmmtry<-mmmtry[-c(189),]
str(mmmtry)

## 2nd cast----------------------------------------------                  
top2cast <- all_crew %>%        
  filter(order==1) %>%        
  mutate(cast2=name)                 

dim(top2cast)
names(top2cast)
names(top2cast)[13]="cast2"

top2cast<-top2cast[,c(2,13)]
names(top2cast)[1]="id"
top2cast<-as.data.frame(top2cast)

mmmtry<-merge(mmmtry,top2cast,by="id",all.x=T)
mmmtry[duplicated(mmmtry$id),"title"] 
mmmtry<-mmmtry[!duplicated(mmmtry$id),]
str(mmmtry)

## 3rd cast----------------------------------------------
top3cast <- all_crew %>%         
  filter(order==2) %>%               
  mutate(cast3=name) 

dim(top3cast)
names(top3cast)
names(top3cast)[13]="cast3"

top3cast<-top3cast[,c(2,13)]
names(top3cast)[1]="id"
top3cast<-as.data.frame(top3cast)

mmmtry<-merge(mmmtry,top3cast,by="id",all.x=T)
mmmtry[duplicated(mmmtry$id),"title"]
mmmtry<-mmmtry[!duplicated(mmmtry$id),]
str(mmmtry)

###screenplay,executive producer,sound editing,editing -----------------------
screenplay <- all_crew %>%         
  filter(job=="Screenplay") %>%             
  mutate(screen=name)               

dim(screenplay)
names(screenplay)
names(screenplay)[12]="screen"

screenplay<-screenplay[,c(2,12)]
names(screenplay)[1]="id"
screenplay<-as.data.frame(screenplay)

mmmtry<-merge(mmmtry,screenplay,by="id",all.x=T)
mmmtry[duplicated(mmmtry$id),"title"]
mmmtry<-mmmtry[!duplicated(mmmtry$id),]
str(mmmtry)

##-------------------------------------------------------------
edit <- all_crew %>%        
  filter(job=="Editor") %>%               
  mutate(editor=name)      

dim(edit)
names(edit )
edit<-edit[,c(2,12)]

names(edit)[1]="id"
edit<-as.data.frame(edit)

mmmtry<-merge(mmmtry,edit,by="id",all.x=T)
mmmtry[duplicated(mmmtry$id),"title"]
mmmtry<-mmmtry[!duplicated(mmmtry$id),]
str(mmmtry)     

##-------------------------------------------------------------------------
produce<- all_crew %>%        
  filter(job=="Executive Producer") %>%               
  mutate(producer=name)      

dim(produce)
names(produce)
produce<-produce[,c(2,12)]

names(produce)[1]="id"
produce<-as.data.frame(produce)

mmmtry<-merge(mmmtry,produce,by="id",all.x=T)
mmmtry[duplicated(mmmtry$id),"title"]
mmmtry<-mmmtry[!duplicated(mmmtry$id),]
str(mmmtry)

###decide budget gross runtime genres what to keep-----------------------------------------------------------
##(兩個資料集中有重複的欄位選遺失值較少的保留，保留的欄位的遺失值由另一個資料及做插補，幸運地都可以互相配合--------------------------------------------------------------------------

#----
movie<-mmmtry
names(movie)
is.na(movie$runtime.y) <- movie$runtime.y=="0"
sum(is.na(movie$runtime.x))#(6)>sum(is.na(movie$runtime.y))#(33)#runtime取imdb(x)

movie[which(is.na(movie$runtime.x)),"runtime.x"]<-movie[which(is.na(movie$runtime.x)),"runtime.y"]
sum(is.na(movie$runtime.x))#4(6-4)
movie<-subset(movie,select=-c(runtime.y)) #把其中較多遺失值的重複因子刪掉
dim(movie)

#----
is.na(movie$budget.y) <- movie$budget.y=="0"
sum(is.na(movie$budget.x))#(361)>sum(is.na(movie$budget.y))#(956)#budget取imdb(x)

movie[which(is.na(movie$budget.x)),"budget.x"]<-movie[which(is.na(movie$budget.x)),"budget.y"]
sum(is.na(movie$budget.x))#269(361-269)
movie<-subset(movie,select=-c(budget.y))
dim(movie)

#----
is.na(movie$gross.y) <- movie$gross.y=="0"
sum(is.na(movie$gross))#(670)>sum(is.na(movie$gross.y))#(1315)#gross取imdb(x)

movie[which(is.na(movie$gross)),"gross"]<-movie[which(is.na(movie$gross)),"gross.y"]
sum(is.na(movie$gross))#465(670-465)
movie<-subset(movie,select=-c(gross.y))
dim(movie)

#----
sum(is.na(movie$genres.x))(0) #genres取imdb(x)
sum(is.na(movie$genres.y))(0)
is.na(movie$genres.y) <- movie$genres.y=="[]"
sum(is.na(movie$genres.y))(25)
movie<-subset(movie,select=-c(genres.y))

dim(movie)
names(movie)
movie<-movie[,-c(2)]

###impute missing director name, language and keywords-------------------------------
##(把兩筆資料集中有重疊的變數(合併資料集取imdb(x)變數)叫出來，看可不可以從tmdb資料集中去插補我們在合併資料集中第一個資料集變數的遺失值)------------------------------------
write.csv(movie,"3moviecut.csv")
moviecut<-read_csv(dir + "/data/data visualization/3moviecut.csv",guess_max=3000)

##director name impute------------------
sum(is.na(movie$director_name))#(8)(手動補值在原資料上)
movie[which(is.na(movie$director_name)),"title"](看title是甚麼，去castcrew資料查)

moviecut[which(is.na(moviecut$director_name)),"director_name"]<-movie[which(is.na(moviecut$director_name)),"director_name"](補值到新資料)
sum(is.na(moviecut$director_name))#(0)

##language impute-----------------------
sum(is.na(moviecut$language))#(6)
moviecut[which(is.na(moviecut$language)),"id"]
langaid<-as.vector(langaid)

tmdb<-read_csv(dir + "data/data cleaning/tmdb_5000_movies.csv",guess_max=3000)
tmdb[tmdb$id %in% c(3060,10970,14877,22488,248402,308529),"original_language"] (從tmdb 5000 movie資料去查)
moviecut[which(is.na(moviecut$language)),"language"]<-"English"
sum(is.na(moviecut$language))#(0)
names(moviecut)

##tmdb keywords unjson & impute-----------------------
sum(is.na(moviecut$plot_keywords))#(121)
is.na(tmdb$keywords) <- tmdb$keywords=="[]"
sum(is.na(tmdb$keywords))#(410)

keyword<- tmdb%>%    
  filter(nchar(keywords)>2) %>%     # fiter out blank keywords field
  mutate(                           # create a new field 
    js = lapply(keywords, fromJSON) #   containing a LIST of keyword and value pairs
  ) %>%                             #   called id and name
  unnest(js) %>%                    # turn each keyword/value pairs in the LIST into a row
  select(id,title, keywords=name)   # select the columns we want

key<-slice(keyword)
names(key)
key1<-aggregate(keywords~id, data =key, FUN =toString) 
names(key1)
str(key1)

moviecut<-merge(moviecut,key1,by="id",all.x="T")
str(moviecut)
moviecut[which(is.na(moviecut$plot_keywords)),"plot_keywords"]<-moviecut[which(is.na(moviecut$plot_keywords)),"keywords"]
sum(is.na(moviecut$plot_keywords))#(51)(從121-51)
names(moviecut)
moviecut<-subset(moviecut,select=-c(keywords))

###moviecut revise(之前0沒有算進NA，選錯變數(選成y)，補錯值))---------------------------------------------
moviecut<-subset(moviecut,select=-c(budget.y))
moviecut<-subset(moviecut,select=-c(runtime.y))
moviecut<-subset(moviecut,select=-c(gross.y))

moviecut<-cbind(moviecut,movie$budget.x)
moviecut<-cbind(moviecut,movie$runtime.x)
moviecut<-cbind(moviecut,movie$gross)

names(moviecut)[names(moviecut) == 'movie$budget.x'] <- 'budget'
names(moviecut)[names(moviecut) == 'movie$runtime.x'] <- 'runtime'
names(moviecut)[names(moviecut) == 'movie$gross'] <- 'gross'
names(moviecut)[names(moviecut) == 'production_companies.x'] <- 'prodcomp.json'
names(moviecut)[names(moviecut) == 'production_companies.y'] <- 'production_companies'
names(moviecut)[names(moviecut) == 'genres.x'] <- 'genres'

summary(moviecut$budget)
summary(moviecut$gross)
summary(moviecut$runtime)
summary(moviecut$plot_keywords)

###calculate profit,ROE----------------------------------------------------------------------------
##profit calculate------------------------------
profit<-moviecut$gross-moviecut$budget #645 NAs
sum(is.na(profit))#645 NAs

##看一下 NA是否是缺少budget or gross那欄(抽10個值出來看看)
sample<-sample(which(is.na(profit)),10)
moviecut$gross[sample]
moviecut$budget[sample]

##ROE calculate------------------------------
roe<-(moviecut$profit/moviecut$budget)*100#645 NAs
sum(is.na(roe))#645 NAs

sample<-sample(which(is.na(roe)),10)
moviecut$gross[sample]
moviecut$budget[sample]

moviecut<-subset(moviecut,select=-roe)
moviecut<-cbind(moviecut,roe)
dim(moviecut)
str(moviecut)
moviecut<-moviecut[,-2]

write.csv(moviecut,"3moviecut.csv")

#excel:把年份補齊、把發行日期唯一缺的一個上網找出年月日補齊 
#excel:把keywords中的,變成|(跟imdb檔案一樣)
#excel:刪掉production company json format, homepage link, imdb link,title_year(跟release data重疊)
#excel:行順序調換，更清楚

