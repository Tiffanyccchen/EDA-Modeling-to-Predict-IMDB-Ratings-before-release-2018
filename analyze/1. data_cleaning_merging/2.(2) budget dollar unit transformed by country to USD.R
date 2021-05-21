
summary(as.factor(country))

movie[which(country=="Afghanistan"),"budget"]<-as.numeric(movie[which(country=="Afghanistan"),"budget"])/69
movie[which(country=="Argentina"),"budget"]<-as.vector(movie[which(country=="Argentina"),"budget"])*0.051
movie[which(country=="Australia"),"budget"]<-as.vector(movie[which(country=="Australia"),"budget"])*0.81
movie[which(country=="Bahamas"),"budget"]<-as.vector(movie[which(country=="Bahamas"),"budget"])*1
movie[which(country=="Belgium"),"budget"]<-as.vector(movie[which(country=="Belgium"),"budget"])*0.0308054
movie[which(country=="Brazil"),"budget"]<-as.vector(movie[which(country=="Brazil"),"budget"])*0.32
movie[which(country=="Bulgaria"),"budget"]<-as.vector(movie[which(country=="Bulgaria"),"budget"])/0.63*1.24
movie[which(country=="Cambodia"),"budget"]<-as.vector(movie[which(country=="Cambodia"),"budget"])*0.00025
movie[which(country=="Cameroon"),"budget"]<-as.vector(movie[which(country=="Cameroon"),"budget"])*1.2
movie[which(country=="Canada"),"budget"]<-as.vector(movie[which(country=="Canada"),"budget"])*0.81
movie[which(country=="Chile"),"budget"]<-as.vector(movie[which(country=="Chile"),"budget"])*0.0017
movie[which(country=="China"),"budget"]<-as.vector(movie[which(country=="China"),"budget"])*0.16
movie[which(country=="Colombia"),"budget"]<-as.vector(movie[which(country=="Colombia"),"budget"])*0.00036
movie[which(country=="Czech Republic"),"budget"]<-as.vector(movie[which(country=="Czech Republic"),"budget"])/0.049*1.24
movie[which(country=="Denmark"),"budget"]<-as.vector(movie[which(country=="Denmark"),"budget"])/0.17*1.24
movie[which(country=="Egypt"),"budget"]<-as.vector(movie[which(country=="Egypt"),"budget"])*0.057
movie[which(country=="Finland"),"budget"]<-as.vector(movie[which(country=="Finland"),"budget"])/1.33*1.24
movie[which(country=="France"),"budget"]<-as.vector(movie[which(country=="France"),"budget"])*1.24

europe<-c("Germany","Greece","Hungary","Iceland","Ireland","Italy","Netherlands","Poland","Slovakia","Spain","Sweden","UK","West Germany")
movie[is.element(movie$country,europe),"budget"]<-as.vector(movie[is.element(movie$country,europe),"budget"])*1.24

movie[which(country=="Georgia"),"budget"]<-as.vector(movie[which(country=="Georgia"),"budget"])*0.41
movie[which(country=="Hong Kong"),"budget"]<-as.vector(movie[which(country=="Hong Kong"),"budget"])*0.16
movie[which(country=="India"),"budget"]<-as.vector(movie[which(country=="India"),"budget"])*0.016
movie[which(country=="Iran"),"budget"]<-as.vector(movie[which(country=="Iran"),"budget"])*0.00003
movie[which(country=="Israel"),"budget"]<-as.vector(movie[which(country=="Israel"),"budget"])*0.29
movie[which(country=="Japan"),"budget"]<-as.vector(movie[which(country=="Japan"),"budget"])*0.0092
movie[which(country=="Kenya"),"budget"]<-as.vector(movie[which(country=="Kenya"),"budget"])*0.0098
movie[which(country=="Kyrgyzstan"),"budget"]<-as.vector(movie[which(country=="Kyrgyzstan"),"budget"])*0.015
movie[which(country=="Libya"),"budget"]<-as.vector(movie[which(country=="Libya"),"budget"])*0.75
movie[which(country=="Mexico"),"budget"]<-as.vector(movie[which(country=="Mexico"),"budget"])*0.054
movie[which(country=="New Zealand"),"budget"]<-as.vector(movie[which(country=="New Zealand"),"budget"])*0.73
movie[which(country=="Nigeria"),"budget"]<-as.vector(movie[which(country=="Nigeria"),"budget"])*0.0028
movie[which(country=="Norway"),"budget"]<-as.vector(movie[which(country=="Norway"),"budget"])*0.13
movie[which(country=="Pakistan"),"budget"]<-as.vector(movie[which(country=="Pakistan"),"budget"])*0.0091
movie[which(country=="Peru"),"budget"]<-as.vector(movie[which(country=="Peru"),"budget"])*0.31
movie[which(country=="Philippines"),"budget"]<-as.vector(movie[which(country=="Philippines"),"budget"])*0.02
movie[which(country=="Romania"),"budget"]<-as.vector(movie[which(country=="Romania"),"budget"])*0.27
movie[which(country=="Russia"),"budget"]<-as.vector(movie[which(country=="Russia"),"budget"])*0.018
movie[which(country=="South Africa"),"budget"]<-as.vector(movie[which(country=="South Africa"),"budget"])*0.084
movie[which(country=="South Korea"),"budget"]<-as.vector(movie[which(country=="South Korea"),"budget"])*0.00094
movie[which(country=="Soviet Union"),"budget"]<-as.vector(movie[which(country=="Soviet Union"),"budget"])*0.018
movie[which(country=="Switzerland"),"budget"]<-as.vector(movie[which(country=="Switzerland"),"budget"])*1.07
movie[which(country=="Taiwan"),"budget"]<-as.vector(movie[which(country=="Taiwan"),"budget"])*(1/32)
movie[which(country=="Thailand"),"budget"]<-as.vector(movie[which(country=="Thailand"),"budget"])*0.032
movie[which(country=="Turkey"),"budget"]<-as.vector(movie[which(country=="Turkey"),"budget"])*0.27
