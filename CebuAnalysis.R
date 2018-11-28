setwd('/Users/hraja/Documents/DataProjects/Cebu/Kmeans')
library('dplyr')
library('tidyr')

getgobehaviour = read.csv('GetGoBehaviour1.csv',sep=',',stringsAsFactors = FALSE)

# reading accrual frequency 

accrualfrequency = read.csv('AccrualFrequency by Account.csv',sep=',',stringsAsFactors = FALSE)

# reading points balance
pointsbalance = read.csv('PointsBalance.csv',sep=',',stringsAsFactors = FALSE)


#joining into a finaldataset

finalData = select(getgobehaviour,c(Subscriber.ID,Account.No,Total.Sends,Total.Bounces,Total.Opens,Total.Clicks))

finalData$Account.No = as.character(finalData$Account.No)
finalDataset = finalData %>% 
            left_join(.,accrualfrequency,by=c("Account.No"="ACCOUNT_NO")) %>%
            left_join(.,pointsbalance,by=c("Account.No"="Account_No"))

finalDataset[is.na(finalDataset)] = 0


finalDataset = mutate(finalDataset,OpenRate = Total.Opens/(Total.Sends - Total.Bounces))
finalDataset = mutate(finalDataset,ClickRate = Total.Clicks/(Total.Sends - Total.Bounces))
finalDataset = mutate(finalDataset,ClickstoOpen = Total.Clicks/Total.Opens)

finalDataset = finalDataset[is.finite(finalDataset$OpenRate),]
finalDataset = finalDataset[is.finite(finalDataset$ClickRate),]


ClusterSet = select(finalDataset,c(Subscriber.ID,OpenRate,ClickRate,AccrualFrequency))

finalClusterSet = data.frame(scale(ClusterSet[,-1]))

#ClusterSet = cbind(PointsScaled = scale(ClusterSet$PointsBalance),ClusterSet)
#ClusterSet = cbind(AccrualsScaled = scale(ClusterSet$AccrualFrequency),ClusterSet)

#finalClusterSet = select(ClusterSet,Subscriber.ID,OpenRate,ClickRate,AccrualsScaled,PointsScaled)
#data[complete.cases(data), ]
#finalClusterSet = finalClusterSet[complete.cases(finalClusterSet),]

#removing infinites
#finalClusterSet = finalClusterSet[is.finite(finalClusterSet$OpenRate),]
#finalClusterSet = finalClusterSet[is.finite(finalClusterSet$ClickRate),]


wss <- (nrow(finalClusterSet)-1)*sum(apply(finalClusterSet,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(finalClusterSet, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
set.seed(10)
seg.k4 = kmeans(finalClusterSet, 4,nstart=10)
seg.k4$centers

#169185 800847  75776   2741
#OpenRate   ClickRate AccrualFrequency PointsBalance
#1  1.7725167  1.13509694     -0.002800589   -0.01292077 (Engaged,No transactions)
#2 -0.4100836 -0.24995713     -0.274876449   -0.07014959 (Low engaged,No transactions)
#3  0.3429133  0.08764804      2.841021315    0.32850853 (Medium engagement, high transactions)
#4  0.9290063  0.54521094      1.943292794   12.21161100 ()

ClusterSet$cluster = seg.k4$cluster


# The Clusters do not clear boundaries and that could be due to outliers. Need to remove
#outliers from accruals and remove point balance from clustering

ClusterSet = filter(ClusterSet,AccrualFrequency <= 6)
ClusterSet = filter(ClusterSet,OpenRate <= 1)
ClusterSet = filter(ClusterSet,ClickRate <= 1)

finalClusterSet = data.frame(scale(ClusterSet[,-1]))

wss <- (nrow(finalClusterSet)-1)*sum(apply(finalClusterSet,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(finalClusterSet, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
set.seed(10)
seg.k4 = kmeans(finalClusterSet, 4,nstart=10)
seg.k4$centers
ClusterSet$cluster = seg.k4$cluster
write.csv(ClusterSet,"KMeansResults2.csv",row.names = FALSE)

set.seed(10)
seg.k3 = kmeans(finalClusterSet, 3,nstart=10)
seg.k3$centers
ClusterSet$cluster = seg.k3$cluster
write.csv(ClusterSet,"KMeansResults3.csv",row.names = FALSE)

seg.k3$size


#68154 810581 169814
#OpenRate  ClickRate AccrualFrequency PointsBalance
#1  0.4864343  0.1661846      2.995841260    0.89507448
#2 -0.4116232 -0.2506492     -0.252355429   -0.07105970
#3  1.7695918  1.1297380      0.002213895   -0.02004113

ClusterSet$cluster = seg.k3$cluster

write.csv(ClusterSet,"KMeansResults.csv",row.names = FALSE)


#888944 checking this subscriber

View(filter(finalDataset,Subscriber.ID == 888944))




# checking email frequency by subscriber
setwd('/Users/hraja/Documents/DataProjects/Cebu')
emailfrequency = read.csv('EmailFrequencybySubscriber.csv',sep=',',stringsAsFactors = FALSE)
fiveormore = filter(emailfrequency,Send.Frequency.per.Month >=5)

# read mutual get go and cebu subscribers
mutualSubs = read.csv('MutualSubscribers.csv',sep=',',stringsAsFactors = FALSE)


#287,552/401146= 72% of subscribers that receive five or more emails per month are subscribed to both get go and cebu pacific