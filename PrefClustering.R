library('dplyr')
library('tidyr')
setwd('/Users/hraja/Documents/DataProjects/Nike')

DCSubStats = read.csv('DCSubStats.csv',header = TRUE,sep=',',stringsAsFactors = FALSE)
DCClicks = read.csv('DCClicks.csv',header = TRUE,sep=',',stringsAsFactors = FALSE)
DCClicksPivot = read.csv('DCClicksPivot.csv',header = TRUE,sep=',',stringsAsFactors = FALSE)

DCClicksPivot = DCClicksPivot[,c(-37,-38)]

DCClicksPivot[is.na(DCClicksPivot)] = 0

View(DCClicksPivot)

DCClicks = mutate(DCClicks,Num = 1)
pivot1 = spread(DCClicks,SubscriberID,Num)


DCClicksJoin = DCClicks %>% inner_join(DCClicks,by='SubscriberID')


NikeWomenExpress = filter(DCClicks,trimws(EmailSubject) == 'All aboard the Nikewomen Express')
AllyouneedRaceDay = filter(DCClicks,trimws(EmailSubject) == 'All you need to know for race day.')
BlackHistoryMonth = filter(DCClicks,trimws(EmailSubject) == 'Black History Month Collection Launches Today')
BrillianceRedefined = filter(DCClicks,trimws(EmailSubject) == 'Brilliance Redefined: the new Metal Flash Pack ')
Congratulations = filter(DCClicks,trimws(EmailSubject) == 'Congratulations you smashed it!')
NikeWomenHalfMarathon = filter(DCClicks,trimws(EmailSubject) == "Don't miss your chance to be part of the Nike Women's Half Marathon")
FindYourRhythm = filter(DCClicks,trimws(EmailSubject) == 'Find your rhythm, show your style')
NeymarnewHypervenom = filter(DCClicks,trimws(EmailSubject) == "Get Neymar's New Hypervenom")
NikeAirforce1UltraFlyknit = filter(DCClicks,trimws(EmailSubject) == 'All you need to know for race day.') 
NikeAirforce1UltraFlyknitLow = filter(DCClicks,trimws(EmailSubject) == 'All you need to know for race day.')
  


#use graph clustering

D = filter(DCClicksPivot,All.aboard.the.Nikewomen.Express == All.you.need.to.know.for.race.day.) %>%
    filter(.,All.aboard.the.Nikewomen.Express == 1)

a = DCClicksPivot[1:35,]
a = mutate(a,EmailSubject = trimws(unique(DCClicks$EmailSubject)))
a = a[,-1]

nikegraph = read.csv('NikeGraph.csv',header = TRUE,sep=',',stringsAsFactors = FALSE)
interestsgraph = read.csv('nikeinterestsgraph.csv',header = TRUE,sep=',',stringsAsFactors = FALSE)

for(i in 2:3) {
  #print(colnames(DCClicksPivot)[i])
  j = i+1
  for(k in j:ncol(DCClicksPivot)) {
    firstcolumn = colnames(DCClicksPivot)[i]
    secondcolumn = colnames(DCClicksPivot)[k]
    print(paste("FirstCol:",firstcolumn," second:",secondcolumn))
    i=2
    k=4
    D = select(DCClicksPivot,i,k) 
    E = select(DCClicksPivot,i,k)
    F = inner_join(D,E,by=c(colnames(D)[1] ,colnames(E)[2]))
    if(nrow(D) > 0) {
      print(nrow(D))
    }
  }
}

InterestsMatrix = select(DCClicksPivot,Running,Soccer,AirMax,Basketball,Gym_Training,Special_Collection,Style,Skateboarding,Tennis)

for (i in 2:ncol(DCClicksPivot)) {
  g = DCClicksPivot[,i] == 1
  j = i+1
  diagonal = sum(DCClicksPivot[,i])
  nikegraph[i-1,j-1] = diagonal
  for ( k in j:ncol(DCClicksPivot)){
    l = DCClicksPivot[,k] == 1
    t = ifelse(g==TRUE & l == TRUE ,1,0 )
    overlap = sum(t)
    nikegraph[i-1,k] = overlap
    nikegraph[k-1,i] = overlap
    #firstindex = 1 for i = 2, secondindex = 1 for k= 3
    #first index = 1 for i = 2, secondindex = 2 for k = 4
   
    print(overlap)
  }
}


#Mapping overlaps to interests graph
for (i in 2:ncol(InterestsMatrix)) {
  g = InterestsMatrix[,i] == 1
  j = i+1
  diagonal = sum(InterestsMatrix[,i])
  interestsgraph[i-1,j-1] = diagonal
  for ( k in j:ncol(InterestsMatrix)){
    l = InterestsMatrix[,k] == 1
    t = ifelse(g==TRUE & l == TRUE ,1,0 )
    overlap = sum(t)
    interestsgraph[i-1,k] = overlap
    interestsgraph[k-1,i] = overlap
    #firstindex = 1 for i = 2, secondindex = 1 for k= 3
    #first index = 1 for i = 2, secondindex = 2 for k = 4
    
    print(overlap)
  }
}
interestsgraph = interestsgraph[-9,-10]
write.csv(interestsgraph,"InterestsGraphFinal.csv",row.names=FALSE)



lookupTbl = group_by(DCClicks,EmailSubject) %>% summarise(TotalClicks = n())

#write nike graph and lookupTbl to files. Do the cosine similarity in excel
write.csv(nikegraph,"NikeGraphFinal.csv",row.names=FALSE)
write.csv(lookupTbl,"LookupTbl.csv",row.names=FALSE)


# use kmeans clustering


DCClicksPivot = mutate(DCClicksPivot,Running = All.aboard.the.Nikewomen.Express+
                         All.you.need.to.know.for.race.day.+Don.t.miss.your.chance.to.be.part.of.the.Nike.Women.s.Half.Marathon
                       +Congratulations.you.smashed.it.+It.s.time.to.gear.up.for.race.day
                       +Nike.Air.Presto.Ultra.Flyknit+NRC.Women.s.Half.Marathon.Sydney.2016
                       +The.Running.Revolution.Is.here+Updated.race.time....Lunar.Epic.Run)

DCClicksPivot = mutate(DCClicksPivot,Soccer = Brilliance.Redefined..the.new.Metal.Flash.Pack+
                         Get.Neymar.s.New.Hypervenom)


DCClicksPivot = mutate(DCClicksPivot,AirMax = The.Return.of.Nike.Air.Max.Zero+
                         What.s.Your.Air.Max.)


DCClicksPivot = mutate(DCClicksPivot,Apparel = Introducing.Nike.Tech.Hypermesh+
                         Introducing.Nike.Tech.Knit)


DCClicksPivot = mutate(DCClicksPivot,Basketball = Introducing.Nike.Air.Force.1.Ultra.Flyknit+
                         Introducing.Nike.Air.Force.1.Ultra.Flyknit.Low)

DCClicksPivot = mutate(DCClicksPivot,Gym_Training = Tights.to.Support.Your.Workout+
                         Your.workout..improved..................................................................................................................................................................................)


DCClicksPivot = mutate(DCClicksPivot,Special_Collection = Black.History.Month.Collection.Launches.Today+
                         The.Nike.Free.Revolution.is.Here)


DCClicksPivot = mutate(DCClicksPivot,Style=Find.your.rhythm..show.your.style....................................................................................................................................................................... +
                         January.Style.Guide..Run.in.Rain.or.Shine+
                         Premium.Fused.Leather..Now.in.Black.and.White+
                         Subtlety.Is.Overrated..New.March.Style.Guide.+
                         Sweat.the.Details..All.New.Style.Guide+
                         Your.Favourite.Boots.in.a.New.Radiant.Colourway)

DCClicksPivot = mutate(DCClicksPivot,Skateboarding = Nike.SB.Bruin.Hyperfeel..Any.Style..Any.Spot............................................................................................................................................................)

DCClicksPivot = mutate(DCClicksPivot,Tennis = The.NikeCourt.Collection.from.Down.Under.)

DCClicksPivot2 = select(DCClicksPivot,SubscriberID,Running,Soccer,AirMax,Apparel,Basketball,Gym_Training,Special_Collection,Style,Skateboarding,Tennis)

DCClicksPivotFinal = left_join(DCClicksPivot2,DCSubStats,by='SubscriberID')
DCClicksPivotFinal = na.omit(DCClicksPivotFinal)
DCClicksPivotFinal = mutate(DCClicksPivotFinal,RunningPerc = Running/TotalClicks)
DCClicksPivotFinal = mutate(DCClicksPivotFinal,FootballPerc = Soccer/TotalClicks)
DCClicksPivotFinal = mutate(DCClicksPivotFinal,AirMaxPerc = AirMax/TotalClicks)
DCClicksPivotFinal = mutate(DCClicksPivotFinal,ApparelPerc = Apparel/TotalClicks)
DCClicksPivotFinal = mutate(DCClicksPivotFinal,BasketballPerc = Basketball/TotalClicks)
DCClicksPivotFinal = mutate(DCClicksPivotFinal,Gym_TrainingPerc = Gym_Training/TotalClicks)
DCClicksPivotFinal = mutate(DCClicksPivotFinal,Special_CollectionPerc = Special_Collection/TotalClicks)
DCClicksPivotFinal = mutate(DCClicksPivotFinal,StylePerc = Style/TotalClicks)
DCClicksPivotFinal = mutate(DCClicksPivotFinal,SkateboardingPerc = Skateboarding/TotalClicks)
DCClicksPivotFinal = mutate(DCClicksPivotFinal,TennisPerc = Tennis/TotalClicks)
DCClicksPivotFinal = mutate(DCClicksPivotFinal,ClothingPerc = (Apparel+Gym_Training)/TotalClicks)
DCClicksPivotFinal = mutate(DCClicksPivotFinal,SpecificCollection = (Special_Collection+AirMax)/TotalClicks)
DCClicksPivotFinal = mutate(DCClicksPivotFinal,OtherSports = (Tennis+Skateboarding)/TotalClicks)
DCClicksPivotFinal = filter(DCClicksPivotFinal,TotalSends > 2)

# Have to look at customers who have got more than two emails.


ClusterSet = select(DCClicksPivotFinal,RunningPerc,
                            BasketballPerc,
                            StylePerc,SpecificCollection,ClothingPerc,OtherSports)

ClusterSet = data.frame(scale(ClusterSet))
wss <- (nrow(ClusterSet)-1)*sum(apply(ClusterSet,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(ClusterSet, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
set.seed(10)
seg.k6 = kmeans(ClusterSet, 6,nstart=10)
seg.k6

#(New Six Clusters)

#Cluster means:
 # RunningPerc BasketballPerc   StylePerc SpecificCollection ClothingPerc  OtherSports
#1 0.018670387   0.0049828179 0.007827559        0.022933623  0.459293853 0.3664547968 (Apparel and Other Sports)
#2 0.006404552   0.0092820066 0.001947319        0.962385144  0.003594803 0.005840345 (Specific Collections)
#3 0.027611194   0.7924787606 0.003373313        0.060344828  0.024112944 0.0183658171 (basketball)
#4 0.032776158   0.0322796057 0.752033412        0.038631589  0.046037762 0.0190513212 (style guides)
#5 0.051841134   0.0377984813 0.036973466        0.114051354  0.027602016 0.0400936731 (a bit of everything)
#6 0.958752721   0.0002438685 0.001599910        0.001878285  0.001183177 0.0002845132 (running)

#Within cluster sum of squares by cluster:
 # [1] 717.88970  28.50782 157.13010 219.98514 509.77428  45.83118
#(between_SS / total_SS =  74.3 %)

DCClicksPivotFinal$cluster = seg.k6$cluster


write.csv(DCClicksPivotFinal,"BehaviouralSegments2.csv",row.names = FALSE)


Basketball = filter(DCClicksPivotFinal,cluster == 3)








#K-means clustering with 5 clusters of sizes 1348, 1488, 10295, 2769, 1737

#Cluster means:
#  RunningPerc   AirMaxPerc ClothingPerc BasketballPerc   StylePerc
#1  0.02806627 0.0213897132   0.02430762   0.7902571711 0.003338279 (Basketball)
#2  0.01842518 0.0092405914   0.87193100   0.0016801075 0.003214606 (Clothing)
#3  0.02511343 0.1041148027   0.01893097   0.0228101038 0.021174158 (AirMax)
#4  0.94089437 0.0009795955   0.00164656   0.0003912363 0.002164554 (Running)
#5  0.03300870 0.0181479654   0.04705225   0.0329933931 0.748294359 (Style)



DCClicksPivotFinal$cluster = seg.k5$cluster


write.csv(DCClicksPivotFinal,"BehaviouralSegments.csv",row.names = FALSE)
#Visible clusters
#Running,Basketball,AirMax,Clothing,Style 



