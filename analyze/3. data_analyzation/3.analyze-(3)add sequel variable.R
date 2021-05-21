### 3. +sequel variable(0:not seq.,1:sequel,2:third installment and after---
movieimpute$sequel<-rep(0,4567)
str(movieimpute)
#1#07 sequels and after---
movieimpute[which(title=="Predator2"),"sequel"]<-1
movieimpute[which(title=="Terminator 2"),"sequel"]<-1
movieimpute[which(title=="Terminator 3: Rise of the Machines"),"sequel"]<-2
movieimpute[which(title=="Terminator Salvation"),"sequel"]<-2
movieimpute[which(title=="Terminator Genisys"),"sequel"]<-2
movieimpute[which(title=="Kill Bill: Vol. 2"),"sequel"]<-1
movieimpute[which(title=="Spider-Man 2"),"sequel"]<-1
movieimpute[which(title=="Spider-Man 3"),"sequel"]<-2
movieimpute[which(title=="The Amazing Spider-Man 2"),"sequel"]<-1
movieimpute[which(title=="Jaws 2"),"sequel"]<-1
movieimpute[which(title=="Jaws: The Revenge"),"sequel"]<-2
movieimpute[which(title=="Home Alone 2: Lost in New York"),"sequel"]<-1
movieimpute[which(title=="Shrek 2"),"sequel"]<-1
movieimpute[which(title=="Shrek the Third"),"sequel"]<-2
movieimpute[which(title=="Shrek Forever After"),"sequel"]<-2
movieimpute[which(title=="Toy Story 2"),"sequel"]<-1
movieimpute[which(title=="Toy Story 3"),"sequel"]<-2
movieimpute[which(title=="Gremlins 2: The New Batch"),"sequel"]<-1
movieimpute[which(title=="Speed 2: Cruise Control"),"sequel"]<-1
movieimpute[which(title=="The Grudge 2"),"sequel"]<-1
movieimpute[which(title=="American Pie 2"),"sequel"]<-1
movieimpute[which(title=="Scream 2"),"sequel"]<-1
movieimpute[which(title=="Scream 3"),"sequel"]<-2
movieimpute[which(title=="Scream 4"),"sequel"]<-2
movieimpute[which(title=="Scary Movie 2"),"sequel"]<-1
movieimpute[which(title=="Scary Movie 3"),"sequel"]<-2
movieimpute[which(title=="Scary Movie 4"),"sequel"]<-2
movieimpute[which(title=="Scary Movie 5"),"sequel"]<-2
movieimpute[which(title=="Rush Hour 2"),"sequel"]<-1
movieimpute[which(title=="Rush Hour 3"),"sequel"]<-2
movieimpute[which(title=="White Noise 2: The Light"),"sequel"]<-1
movieimpute[which(title=="Step Up 2: The Streets"),"sequel"]<-1
movieimpute[which(title=="Step Up 3D"),"sequel"]<-2
movieimpute[which(title=="Step Up Revolution"),"sequel"]<-2
movieimpute[which(title=="The Santa Clause 2"),"sequel"]<-1
movieimpute[which(title=="Transporter 2"),"sequel"]<-1
movieimpute[which(title=="The Transporter Refueled"),"sequel"]<-2
movieimpute[which(title=="Final Destination 2"),"sequel"]<-1
movieimpute[which(title=="Final Destination 3"),"sequel"]<-2
movieimpute[which(title=="Final Destination 5"),"sequel"]<-2
movieimpute[which(title=="Cheaper by the Dozen 2"),"sequel"]<-1
movieimpute[which(title=="Friday the 13th Part 2"),"sequel"]<-1
movieimpute[which(title=="Friday the 13th Part III"),"sequel"]<-2
movieimpute[which(title=="Friday the 13th: The Final Chapter"),"sequel"]<-2
movieimpute[which(title=="Friday the 13th: A New Beginning"),"sequel"]<-2
movieimpute[which(title=="Friday the 13th Part VIII: Jason Takes Manhattan"),"sequel"]<-2
movieimpute[which(title=="Friday the 13th Part VII: The New Blood"),"sequel"]<-2
movieimpute[which(title=="Miss Congeniality 2: Armed and Fabulous"),"sequel"]<-1
movieimpute[which(title=="Iron Man 2"),"sequel"]<-1
movieimpute[which(title=="Iron Man 3 "),"sequel"]<-2
movieimpute[which(title=="The Sisterhood of the Traveling Pants 2"),"sequel"]<-1
movieimpute[which(title=="Legally Blonde 2: Red, White & Blonde"),"sequel"]<-1
movieimpute[which(title=="Scooby-Doo 2: Monsters Unleashed"),"sequel"]<-1
movieimpute[which(title=="The Princess Diaries 2: Royal Engagement"),"sequel"]<-1
movieimpute[which(title=="Big Momma's House 2"),"sequel"]<-1
movieimpute[which(title=="Child's Play 2"),"sequel"]<-1
movieimpute[which(title=="Hamlet 2"),"sequel"]<-1
movieimpute[which(title=="The Jungle Book 2"),"sequel"]<-1
movieimpute[which(title=="Agent Cody Banks 2: Destination London"),"sequel"]<-1
movieimpute[which(title=="The Texas Chainsaw Massacre 2"),"sequel"]<-1
movieimpute[which(title=="Beastmaster 2: Through the Portal of Time"),"sequel"]<-1
movieimpute[which(title=="Paranormal Activity 2"),"sequel"]<-1
movieimpute[which(title=="Paranormal Activity 3"),"sequel"]<-2
movieimpute[which(title=="aranormal Activity 4"),"sequel"]<-2
movieimpute[which(title=="Paranormal Activity: The Marked Ones "),"sequel"]<-2
movieimpute[which(title=="Cars 2"),"sequel"]<-1
movieimpute[which(title=="Kung Fu Panda 2"),"sequel"]<-1
movieimpute[which(title=="Kung Fu Panda 3"),"sequel"]<-2
movieimpute[which(title=="The Twilight Saga: Eclipse"),"sequel"]<-1
movieimpute[which(title=="The Twilight Saga: Breaking Dawn - Part 2"),"sequel"]<-2
movieimpute[which(title=="Kick-Ass 2"),"sequel"]<-1
movieimpute[which(title=="Never Back Down 2: The Beatdown"),"sequel"]<-1
movieimpute[which(title=="Journey 2: The Mysterious Island"),"sequel"]<-1
movieimpute[which(title=="The Expendables 2"),"sequel"]<-1
movieimpute[which(title=="The Expendables 3"),"sequel"]<-2
movieimpute[which(title=="The Smurfs 2"),"sequel"]<-1
movieimpute[which(title=="Taken 2"),"sequel"]<-1
movieimpute[which(title=="Taken 3"),"sequel"]<-2
movieimpute[which(title=="Insidious: Chapter 2"),"sequel"]<-1
movieimpute[which(title=="Insidious: Chapter 3"),"sequel"]<-2
movieimpute[which(title=="Despicable Me 2"),"sequel"]<-1
movieimpute[which(title=="Grown Ups 2"),"sequel"]<-1
movieimpute[which(title=="Anchorman 2: The Legend Continues"),"sequel"]<-1
movieimpute[which(title=="Cloudy with a Chance of Meatballs 2"),"sequel"]<-1
movieimpute[which(title=="The Hunger Games: Catching Fire"),"sequel"]<-1
movieimpute[which(title=="The Hunger Games: Mockingjay - Part 1"),"sequel"]<-2
movieimpute[which(title=="The Hunger Games: Mockingjay - Part 2"),"sequel"]<-2
movieimpute[which(title=="Batman Returns"),"sequel"]<-1
movieimpute[which(title=="Batman Forever"),"sequel"]<-2
movieimpute[which(title=="Batman & Robin"),"sequel"]<-2
movieimpute[which(title=="RED 2"),"sequel"]<-1
movieimpute[which(title=="Hotel Transylvania 2"),"sequel"]<-1
movieimpute[which(title=="Rio 2"),"sequel"]<-1
movieimpute[which(title=="A Haunted House 2"),"sequel"]<-1
movieimpute[which(title=="Bill & Ted's Bogus Journey"),"sequel"]<-1
movieimpute[which(title=="Ted 2"),"sequel"]<-1
movieimpute[which(title=="Horrible Bosses 2"),"sequel"]<-1
movieimpute[which(title=="Dolphin Tale 2"),"sequel"]<-1
movieimpute[which(title=="Hot Tub Time Machine 2"),"sequel"]<-1
movieimpute[which(title=="Pitch Perfect 2"),"sequel"]<-1
movieimpute[which(title=="Paul Blart: Mall Cop 2"),"sequel"]<-1
movieimpute[which(title=="The Conjuring 2"),"sequel"]<-1
movieimpute[which(title=="Sinister 2"),"sequel"]<-1
movieimpute[which(title=="Now You See Me 2"),"sequel"]<-1
movieimpute[which(title=="My Big Fat Greek Wedding 2"),"sequel"]<-1
movieimpute[which(title=="Ride Along 2"),"sequel"]<-1
movieimpute[which(title=="Zoolander 2"),"sequel"]<-1

movieimpute$sequel<-as.factor(movieimpute$sequel)
write.csv(movieimpute,"../data/movieana.csv")

