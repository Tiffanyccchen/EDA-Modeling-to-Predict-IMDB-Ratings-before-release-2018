###2.analyze-correlation of candidate var. analysis,pca of fb likes
attach(movieimpute)
library(RColorBrewer)
library(corrplot)
library(stats)
library(devtools)
install_github("ggbiplot", "vqv")
library(ggbiplot)

##(1)屬量變數相關係數(Pearson corr coef)
cor(movieimpute[,3:11])
dim(cor(movieimpute[,3:11]))
aa<-as.matrix(cor(movieimpute[,3:11]))
aaa<-matrix(,15,15)
aaa[1:9,1:9]<-aa


##(2)屬質變數相關係數(cramer's V)

#資料前處理(genres取第一個、contentrating,aspectratio,genres,language類別水準個數少的轉others)
dir<- get_wd()
movieimpute<-read_csv(dir + "../data/movieana.csv",guess_max=3000)

#genres取第一個
gensplit<- colsplit(movie$genres,split="\\|",names=c("g1","g2","g3","g4","g5","g6","g7","g8"))
gensplit <- data.frame(lapply(gensplit, as.character), stringsAsFactors=FALSE)
for (i in 1: nrow(gensplit)) { gensplit[i,duplicated(as.character(gensplit[i,]))]<-""}
movieimpute$genres<-gensplit$g1
movieimpute$genres

#contentrating, aspectratio, genres,language 類別水準個數少的轉others
ss<-as.data.frame(table(as.factor(contentrating)))
'%ni%' <- Negate('%in%')
select<-ss[ss$Var1 %ni% c("G","PG","PG-13","R","NC-17"),]
namee<-as.vector(select[,1])
movieimpute[is.element(contentrating,namee),"contentrating"]<-"Others"

ss<-as.data.frame(table(as.factor(aspectratio)))
'%ni%' <- Negate('%in%')
select<-ss[ss$Var1 %ni% c("1.85","2.35"),]
namee<-as.vector(select[,1])
movieimpute[is.element(aspectratio,namee),"aspectratio"]<-"Others"

ss<-as.data.frame(table(as.factor(language)))
select<-ss[which(ss$Freq<10),]
namee<-as.vector(select[,1])
movieimpute[is.element(language,namee),"language"]<-"Others"
movieimpute[,"language"]

ss<-as.data.frame(table(as.factor(genres)))
select<-ss[ss$Var1 %in% c("Film-Noir","History","Romance","Musical"),]
namee<-as.vector(select[,1])
movieimpute[is.element(genres,namee),"genres"]<-"Others"

movieimpute<-data.frame(movieimpute)
movieimpute[,12:17][] <- lapply(movieimpute[,12:17],factor) # the "[]" keeps the dataframe structure
col_names <- names(movieimpute[,12:17])

#calculate cv between categorical var.s
qcv<-matrix(,6,6)

cv.test = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
    (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
  print.noquote("Cramer V / Phi:")
  return(as.numeric(CV))
}

for(i in 1:6){
  for(j in i:6)
    qcv[j,i]<-cv.test(movieimpute[,i+11],movieimpute[,j+11])
}
aaa[10:15,10:15]<-qcv
aaa

##(3)屬質屬量變數相關係數(aov's sqrt(eta square)==lm's sqrt(rsquare))
for(i in 1:9){
  for(j in 10 :15){
    aaa[i,j]<- sqrt(summary(lm(movieimpute[,i+2]~movieimpute[,j+2]))$r.squared)
  }
}
aaa

##Correlation plot of all variables
names(movieimpute)
colnames(aaa) <-names(movieimpute)[3:17]
rownames(aaa) <-names(movieimpute)[3:17]
diag(aaa) = NA
corrplot(aaa, type = "upper",col = brewer.pal(n = 8, name = "PuOr"),na.label = "o")

##PCA of cast,actor123 fb likes----
moviepca<-movieimpute[,6:9]
names(moviepca)

pca <- prcomp(moviepca,
                 center = TRUE,
                 scale. = TRUE) 
print(pca)

#pca plot
plot(pca ,main="PCA of Cast & Actors'(1,2,3) FB Likes", type = "l",col="blue",lwd=5)
text(x=c(1+0.1,2-0.2,3-0.2,4-0.2),y=c(1.6225^2-0.2,0.9643^2,0.6601^2,0.0403^2), labels=c("65.8%","89.7%","99.9%","100%"),col="darkred",cex=1.1)
legend(2.5,2.5, legend=c("Cumulative Pro. of Var."),
       col=c("darkred"), lty=1, cex=0.8)
summary(pca)

g <- ggbiplot(pca, obs.scale = 1, var.scale = 1, 
              ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

#add these two pc's to our variable dataset 
#0.5915718/sd(castfblikes)
#0.5145252/sd(actor1fblikes)
#0.4750651/sd(actor2fblikes)
#0.3995244/sd(actor3fblikes)
#3.177623|3.313103|11.57236|24.24813

movieimpute$pc1castfb<-.3177623*castfblikes+.3313103*actor1fblikes+
1.157236*actor2fblikes+2.424813*actor3fblikes

#-0.2871048/sd(castfblikes)
#-0.5591687/sd(actor1fblikes)
#0.4044386/sd(actor2fblikes)
#0.6643272/sd(actor3fblikes)
#1.542181|3.600569|9.85913|40.31967

movieimpute$pc2castfb<-(-.1542181)*castfblikes+(-.3600569)*actor1fblikes+
0.985193*actor2fblikes+4.031967*actor3fblikes
