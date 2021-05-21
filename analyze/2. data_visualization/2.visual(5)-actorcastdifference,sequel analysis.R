f <- function(x) 
  ans <- boxplot.stats(x)
  data.frame(ymin = ans$conf[1], ymax = ans$conf[2], y = ans$stats[3])
}
### 1.actor x = cast x bar plot--------------------------------
vote<-subset(movie,select=c("actor_1_name","cast1"))
sum(apply(vote, 1, function(x) length(unique(x))==1))###1178

vote<-subset(movie,select=c("actor_2_name","cast2"))
sum(apply(vote, 1, function(x) length(unique(x))==1))###512

vote<-subset(movie,select=c("actor_3_name","cast3"))
sum(apply(vote, 1, function(x) length(unique(x))==1))###396

aa<-c("actor 1","actor 2","actor 3")
bb<-c(1178,512,396)
mydata <-data.frame(aa,bb)

fill<-c("thistle","thistle1","thistle2")

p<-ggplot(mydata,aes(reorder(aa,bb),bb))
p+geom_bar(stat='identity',aes(fill =fill))+ 
  labs(title = "Number of Actor(by FB likes) equals to Actor(by Importance) Bar Plot", y = "Count", x = "Actor")+
  theme(plot.title = element_text(hjust = 0.5,size=12),
  axis.text.x = element_text(angle = 60,hjust=1,size=12))+
  scale_y_continuous(limits = c(0,4567),breaks=c(200,400,600,800,1000,1200,2000,3000,4000,4567))+
  coord_flip()+
  guides(fill=FALSE)+
  geom_label(aes(label=bb))

### 2.sequel trend analysis------------------------------
sfind<-movie[movie$title %like% "2","title"] #find title containing number 2
sfind<-as.data.frame(sfind)

##70 sequels---
#1-2:67
#1-3:21
#2-3:16

##episode 1 vs 2 box plot---
seq11<-data.frame(value=c(7.8,8.1,8.1,7.3,7,8,7.5,7.9,8.3,7.2,7.2,5.9,7,7.2,5.5,6.5,6.4,
6.8,5.8,6.2,7.9,6.6,6.2,5.9,4.9,6.2,6.5,5.1,7.8,7.8,5,5.9,6.2,6.3,7.2,7.6,7.7,6.6,5.8,6.5,5.5,7.9,8.2,6.8,7.7,6,7.2,7,7.3,7.6,7.1,7.1,7,5.1,6.9,7,6.9,6.9,6.4,7.2,5.2,7.5,6.8,7.3,6.6,6.2,6.6),id="Episode 1")
seq12<-data.frame(value=c(6.2,8.5,8.0,7.3,6.7,5.7,6.6,7.2,7.9,6.3,3.7,5,6.4,6.1,5.7,6.2,5.5,
6.3,5.4,5,7,6.2,4.6,5.4,4.9,5.7,5.7,4.6,6.4,5.4,4.5,5.5,3.8,5.7,6.3,7.3,6.6,5.8,5.8,6.7,5.4,6.3,7.9,6.6,7.5,5.4,6.3,6.5,7.6,7,6.7,6.7,6.4,4.7,6.2,6.4,6.3,6.4,5.1,6.5,4.4,7.8,5.2,6.9,6.1,5.9,4.8),id="Episode 2")

#combine datasets and compute descriptive statistics of original movies and sequals
df = rbind(seq11,seq12)
length(seq11)
length(seq12)
mean(seq11$value)
mean(seq12$value)
sd(seq11$value)
sd(seq12$value)

fill <- c("gold1","#4271AE")
line <- "#1F3552"

df%>%
  group_by(id) %>%
  mutate(outlier = ifelse(is_outlier(value),value, as.numeric(NA))) %>%
  ggplot(., aes(x =reorder(id,value), y = value)) +
        geom_boxplot(fill = fill, colour = line) +
        scale_y_continuous(name = "Score Distribution") +
        scale_x_discrete(name = "") +
        ggtitle("Episode 1 v.s Episode 2 Score Box Plot")+
        geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3,size=2.54)+
        theme(axis.text.x = element_text(size = 12, colour = "black"),
        plot.title = element_text(hjust = 0.5))+
        stat_summary(fun.y="mean", geom="point", shape=23, size=4, fill="white") +
        guides(fill=FALSE) + 
        coord_flip()+
        stat_summary(fun.data = f, geom = "crossbar", colour = NA, fill = "skyblue", width = 0.8, alpha = 0.5)

##episode 2 vs 3 box plot---

seq22<-data.frame(value=c(8.5,7.3,5.7,7.2,7.9,6.1,5.2,6.6,6.2,6.3,6.2,6.1,7.0,5.7,7.3,4.6,6.7,6.3,6.6,7.6,7.0),id="Episode2")
seq23<-data.frame(value=c(6.4,6.2,2.8,6.1,8.3,5.5,5.5,6.2,6.2,5.1,5.8,5.7,7.2,5.8,7.2,4.9,6.1,6.0,6.1,6.7,5.4),id="Episode3")

#combine datasets and compute descriptive statistics of sequals and the third
df = rbind(seq22,seq23)

length(seq22)
length(seq23)
median(seq22$value)
median(seq23$value)
sd(seq22$value)
sd(seq23$value)

fill <- c("pink4","plum")
line <- "#1F3552"

df%>%
  group_by(id) %>%
  mutate(outlier = ifelse(is_outlier(value),value, as.numeric(NA))) %>%
  ggplot(., aes(x =reorder(id,value), y = value)) +
        geom_boxplot(fill = fill, colour = line) +
        scale_y_continuous(name = "Score Distribution") +
        scale_x_discrete(name = "") +
        ggtitle("Episode 2 v.s Episode 3 Score Box Plot")+
        geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3,size=2.54)+
        theme(axis.text.x = element_text(size = 12, colour = "black"),
        plot.title = element_text(hjust = 0.5))+
        stat_summary(fun.y="mean", geom="point", shape=23, size=4, fill="white") +
        guides(fill=FALSE) + 
        coord_flip()+
        stat_summary(fun.data = f, geom = "crossbar", colour = NA, fill = "skyblue", width = 0.8, alpha = 0.5)

#combine datasets and compute descriptive statistics of sequals and the third
seq31<-data.frame(value=c(8.1,7.3,8.0,7.9,8.3,7.2,6.5,6.8,7.9,6.3,7.6,6.5,7.9,6.8,7.3,7.6),id="Episode1")
seq33<-data.frame(value=c(6.4,6.2,2.8,6.1,8.3,5.5,6.2,5.1,7.2,5.8,7.2,6.1,6.0,6.1,6.7,5.4),id="Episode3")
df<-rbind(seq31,seq33)

mean(seq31$value)
mean(seq33$value)
median(seq31$value)
median(seq33$value)
sd(seq31$value)
sd(seq33$value)

fill <- c("orchid4","palegoldenrod")
line <- "#1F3552"

df%>%
  group_by(id) %>%
  mutate(outlier = ifelse(is_outlier(value),value, as.numeric(NA))) %>%
  ggplot(., aes(x =reorder(id,value), y = value)) +
        geom_boxplot(fill = fill, colour = line) +
        scale_y_continuous(name = "Score Distribution") +
        scale_x_discrete(name = "") +
        ggtitle("Episode 1 v.s Episode 3 Score Box Plot")+
        geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3,size=2.54)+
        theme(axis.text.x = element_text(size = 12, colour = "black"),
        plot.title = element_text(hjust = 0.5))+
        stat_summary(fun.y="mean", geom="point", shape=23, size=4, fill="white") +
        guides(fill=FALSE) + 
        coord_flip()+
        stat_summary(fun.data = f, geom = "crossbar", 
        colour = NA, fill = "skyblue", width = 0.8, alpha = 0.5)


## Examine and Inspect of movies with sequals
#Friday the 13th
trend6<-c(6.1,5.7,5.9,4.7,5.3,4.5)

#Terminator
trend5<-c(8.1,8.5,6.4,6.6,6.6)

#Scary Movie
trend4<-c(5.2,5.5,5.1,3.5)

movie[movie$title %like% "Predator",c("title","imdb_score")]#2
movie[movie$title %like% "Terminator",c("title","imdb_score")]#5
movie[movie$title %like% "Kill Bill",c("title","imdb_score")]#2
movie[movie$title %like% "Spider-Man",c("title","imdb_score")]#3 #2
movie[movie$title %like% "Jaws",c("title","imdb_score")]#3
movie[movie$title %like% "Home Alone",c("title","imdb_score")]#2
movie[movie$title %like% "Shrek",c("title","imdb_score")]#4
movie[movie$title %like% "Toy Story",c("title","imdb_score")]#3
movie[movie$title %like% "Gremlins",c("title","imdb_score")]#2
movie[movie$title %like% "Speed",c("title","imdb_score")]#2
movie[movie$title %like% "The Grudge",c("title","imdb_score")]#2
movie[movie$title %like% "American Pie",c("title","imdb_score")]#2
movie[movie$title %like% "Scream",c("title","imdb_score")]#4
movie[movie$title %like% "Scary Movie",c("title","imdb_score")]#4(2-5)
movie[movie$title %like% "Rush Hour",c("title","imdb_score")]#2(2-3)
movie[movie$title %like% "White Noise",c("title","imdb_score")]#2
movie[movie$title %like% "Step Up",c("title","imdb_score")]#4
movie[movie$title %like% "The Santa Clause",c("title","imdb_score")]#2
movie[movie$title %like% "Transporter",c("title","imdb_score")]#3(1,2,4)
movie[movie$title %like% "Final Destination ",c("title","imdb_score")]#3(2,3,5)
movie[movie$title %like% "Cheaper by the Dozen",c("title","imdb_score")]#2
movie[movie$title %like% "Friday the 13th",c("title","imdb_score")]#6(2,3,4,5,7,8)
movie[movie$title %like% "Miss Congeniality",c("title","imdb_score")]#2
movie[movie$title %like% "Iron Man",c("title","imdb_score")]#3
movie[movie$title %like% "The Sisterhood of the Traveling Pants",c("title","imdb_score")]#2
movie[movie$title %like% "Legally Blonde",c("title","imdb_score")]#2
movie[movie$title %like% "Stuart Little",c("title","imdb_score")]#2
movie[movie$title %like% "Scooby-Doo",c("title","imdb_score")]#2
movie[movie$title %like% "The Princess Diaries",c("title","imdb_score")]#2
movie[movie$title %like% "Child's Play",c("title","imdb_score")]#2
movie[movie$title %like% "Big Momma's House",c("title","imdb_score")]#2
movie[movie$title %like% "Hamlet",c("title","imdb_score")]#2
movie[movie$title %like% "The Jungle Book",c("title","imdb_score")]#2
movie[movie$title %like% "Agent Cody Banks",c("title","imdb_score")]#2
movie[movie$title %like% "The Texas Chainsaw Massacre",c("title","imdb_score")]#2
movie[movie$title %like% "Beastmaster",c("title","imdb_score")]#2
movie[movie$title %like% "Paranormal Activity",c("title","imdb_score")]#5
movie[movie$title %like% "Cars",c("title","imdb_score")]#2
movie[movie$title %like% "Kung Fu Panda",c("title","imdb_score")]#3
movie[movie$title %like% "The Twilight Saga",c("title","imdb_score")]#3(2,3,5)
movie[movie$title %like% "Kick-Ass",c("title","imdb_score")]#2
movie[movie$title %like% "Never Back Down",c("title","imdb_score")]#2
movie[movie$title %like% "Journey ",c("title","imdb_score")]#2
movie[movie$title %like% "The Expendables",c("title","imdb_score")]#3
movie[movie$title %like% "The Smurfs",c("title","imdb_score")]#2
movie[movie$title %like% "Taken",c("title","imdb_score")]#3
movie[movie$title %like% "How to Train Your Dragon",c("title","imdb_score")]#2
movie[movie$title %like% "Insidious",c("title","imdb_score")]#3
movie[movie$title %like% "Despicable Me",c("title","imdb_score")]#2
movie[movie$title %like% "Grown Ups",c("title","imdb_score")]#2
movie[movie$title %like% "Anchorman",c("title","imdb_score")]#2
movie[movie$title %like% "Cloudy with a Chance of Meatballs",c("title","imdb_score")]#2
movie[movie$title %like% "The Hunger Games",c("title","imdb_score")]#4
movie[movie$title %like% "Batman",c("title","imdb_score")]#4
movie[movie$title %like% "RED",c("title","imdb_score")]#2
movie[movie$title %like% "Hotel Transylvania",c("title","imdb_score")]#2
movie[movie$title %like% "Rio",c("title","imdb_score")]#2
movie[movie$title %like% "A Haunted House",c("title","imdb_score")]#2
movie[movie$title %like% "Ted",c("title","imdb_score")]#2 #2
movie[movie$title %like% "Horrible Bosses",c("title","imdb_score")]#2
movie[movie$title %like% "Dolphin Tale",c("title","imdb_score")]#2
movie[movie$title %like% "Hot Tub Time Machine",c("title","imdb_score")]#2
movie[movie$title %like% "Pitch Perfect",c("title","imdb_score")]#2
movie[movie$title %like% "Paul Blart",c("title","imdb_score")]#2
movie[movie$title %like% "The Conjuring",c("title","imdb_score")]#2
movie[movie$title %like% "Sinister",c("title","imdb_score")]#2
movie[movie$title %like% "Now You See Me",c("title","imdb_score")]#2
movie[movie$title %like% "My Big Fat Greek Wedding",c("title","imdb_score")]#2
movie[movie$title %like% "Ride Along",c("title","imdb_score")]#2
movie[movie$title %like% "Zoolander",c("title","imdb_score")]#2
