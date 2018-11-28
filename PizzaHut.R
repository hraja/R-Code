# Idea is three sets of gold customers
#1. High Value Gold
#2. Gold customers that use coupons
#3. Gold customers that have lapsed
#Make coupon hunters to aspire towards high value gold members.


library('dplyr')
library('tidyr')
setwd('/Users/hraja/Documents/DataProjects/Pizza Hut Hong Kong/CustomerData')
# data ends at Nov 28,2016

# reading RFM Data: CLV is money spent in 2016
RFMPZH  = read.csv("RFMbyCust.csv",header = TRUE,sep=',',stringsAsFactors = FALSE)
RFMPZH = mutate(RFMPZH,DaysSinceLastPurchase = as.integer(difftime(Sys.Date(),as.Date(Recency),units = 'days')))

#reading TimeBtwOrders
TimeBtwOrders  = read.csv("TimeBtwOrders.csv",header = TRUE,sep=',',stringsAsFactors = FALSE)


# reading Coupons issued by member in 2016
CouponsIssuedByMember  = read.csv("CouponsIssuedByMember.csv",header = TRUE,sep=',',stringsAsFactors = FALSE)

#reading Coupons redeemed by member in 2016
CouponsRedeemedByMember  = read.csv("CouponsRedeemedByMember.csv",header = TRUE,sep=',',stringsAsFactors = FALSE)

# A customer 10000767139 received 23 coupons and he only redeemed 2 of them.
# Does this make sense? How do you send coupons? to burn user points?


# filter out orders that have a lead value less than timestamp. Should be one per member
lastOrder = filter(TimeBtwOrders,Timestamp > LeadVal)
filteredOrders = filter(TimeBtwOrders,Timestamp < LeadVal)

# unique members (336199)
members = unique(TimeBtwOrders$MemberID)



filteredOrders = mutate(filteredOrders,Days = as.integer(difftime(as.Date(LeadVal),as.Date(Timestamp),units = 'days')))

#final mean time between orders by member
avgTimeBtwOrders = filteredOrders %>% 
                                  group_by(MemberID) %>%
                                  summarise(AvgDaysBtwOrders = mean(Days))


# get the single customer view with gold and basic status.
membersSCV =  read.csv("MembersSCV.csv",header = TRUE,sep='|',stringsAsFactors = FALSE)




#finalCustomerView
finalData = membersSCV %>%
            left_join(RFMPZH,by='MemberID') %>%
            left_join(avgTimeBtwOrders,by='MemberID') %>%
            left_join(CouponsIssuedByMember,by="MemberID") %>%
            left_join(CouponsRedeemedByMember,by="MemberID")

write.csv(finalData,"PZHCustomerView.csv")

# a basic member that spends $1000 becoames a gold member
# given that customers that signed up in 2016, how long did it take them to reach gold?
# Do customers spend more after Gold Status or in Basic Status.





## COHORT ANALYSIS
#1. Does share of wallet decrease over time?
#2 Does frequency of purchases decrease over time?
#3


Cohorts_2016 = filteredOrders %>%
               inner_join(membersSCV,by='MemberID') %>%
               filter(.,DateJoined >= '2016-01-01') %>%
               select(.,MemberID,Timestamp,Amount,LeadVal,Days,DateJoined,EmailAddress,Membership)

Cohorts_2016 = mutate(Cohorts_2016,DaysSinceJoining = as.integer(difftime(as.Date(Timestamp),as.Date(DateJoined),units = 'days')))


# write the Cohorts file to drive
write.csv(Cohorts_2016,"CohortAnalysis.csv")


#write the retention rate analysis to file
Retention_2016 = membersSCV %>%
  left_join(TimeBtwOrders,by='MemberID') %>%
  select(.,MemberID,Timestamp,Amount,LeadVal,DateJoined,EmailAddress,Membership,MobileNumber,DeviceID,ReceiveNews,Receive3rdPartyNews,FacebookID)

Retention_2016 = mutate(Retention_2016,DaysSinceJoining = as.integer(difftime(as.Date(Timestamp),as.Date(DateJoined),units = 'days')))
Retention_2016 = mutate(Retention_2016,PurchaseMonth = format(as.Date(Timestamp),"%m"))
Retention_2016 = mutate(Retention_2016,MonthJoined = format(as.Date(DateJoined),"%m"))

write.csv(Retention_2016,"RetentionAnalysis-2.csv")






# Cluster Analysis


#finalCustomerView
ClusterSet = membersSCV %>%
  inner_join(RFMPZH,by='MemberID') %>%
  inner_join(avgTimeBtwOrders,by='MemberID') %>%
  left_join(CouponsIssuedByMember,by="MemberID") %>%
  left_join(CouponsRedeemedByMember,by="MemberID") %>%
  select(.,MemberID,Membership,DateJoined,CLV,Frequency,AOV,DaysSinceLastPurchase,AvgDaysBtwOrders,TotalCouponsIssued,TotalCouponsRedeemed,ReceiveNews,Receive3rdPartyNews)


GoldMembers = filter(ClusterSet,Membership=='GOLD')
GoldMembers = mutate(GoldMembers,TimeInProg = as.integer(difftime(Sys.Date(),as.Date(DateJoined),units = 'days')))
GoldMembers = mutate(GoldMembers,RedemptionRate = TotalCouponsRedeemed/TotalCouponsIssued)
GoldMembers[is.na(GoldMembers)] = 0
# find out outliers

# using tukeys formula to remove outliers


boxplot(GoldMembers$CLV)
boxplot(GoldMembers$Frequency)


#Create a sample set for gold members 
goldMemberCluster = dplyr::select(GoldMembers,MemberID,CLV,Frequency,DaysSinceLastPurchase,AOV,AvgDaysBtwOrders,RedemptionRate,TimeInProg)

goldMemberCluster = filter(goldMemberCluster,CLV < (quantile(CLV,0.75)+IQR(CLV)*1.5 ))
goldMemberCluster = filter(goldMemberCluster,Frequency < (quantile(Frequency,0.75)+IQR(Frequency)*1.5 ))
goldMemberCluster = filter(goldMemberCluster,AvgDaysBtwOrders < (quantile(AvgDaysBtwOrders,0.75)+IQR(AvgDaysBtwOrders)*1.5 ))
goldMemberCluster = filter(goldMemberCluster,RedemptionRate < (quantile(RedemptionRate,0.75)+IQR(RedemptionRate)*1.5 ))
goldMemberCluster = filter(goldMemberCluster,AOV < (quantile(AOV,0.75)+IQR(AOV)*1.5 ))
goldMemberCluster = filter(goldMemberCluster,AOV > (quantile(AOV,0.25)-IQR(AOV)*1.5 ))
boxplot(goldMemberCluster$DaysSinceLastPurchase)

library('corrplot')
M = cor(goldMemberCluster[,-1])
corrplot(M,method='circle')

scaledClusterSet = data.frame(scale(goldMemberCluster[,c(-1,-2,-3,-7,-8)]))

wss <- (nrow(scaledClusterSet)-1)*sum(apply(scaledClusterSet,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(scaledClusterSet, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

library('corrplot')
M = cor(scaledClusterSet[,-1])
corrplot(M,method='circle')

set.seed(10)
seg.k <- kmeans(scaledClusterSet, 4,nstart=10) # 5 cluster solution
seg.k

#Cluster means:
#  DaysSinceLastPurchase        AOV AvgDaysBtwOrders TimeInProg
#1             0.4714903  1.4954602       -0.1628250 -0.3926076
#2            -0.1939647  0.2817701        1.7611157 -0.1115577
#3             1.2621794 -0.5483030       -0.5667658 -0.1755731
#4            -0.4246227 -0.3246840       -0.2567785  1.1261478
#5            -0.6450047 -0.3273681       -0.2831129 -0.8104845

#K-means clustering with 3 clusters of sizes 47437, 31610, 50024

#Cluster means:
#Within cluster sum of squares by cluster:
#[1] 36078.60 34891.50 28452.77
#(between_SS / total_SS =  61.5 %)

#  AOV TimeInProg
#1 -0.3293795  1.0735435
#2  1.2940890 -0.4580458
#3 -0.5053850 -0.7285874



#K-means clustering with 3 clusters of sizes 40409, 34371, 54291

#Cluster means:
#  Frequency        AOV
#1 -0.5353196  1.1551696 (High AOV, but lower frequency, lower coupon redemption, lapsed)
#2  1.3584216 -0.5037801 (Higher frequency, lower AOV, recent buyers,been in the program for a while)
#3 -0.4615604 -0.5408599 (lower frequency, lower AOV,been in program for a while,)


#Within cluster sum of squares by cluster:
#  [1] 31542.23 29432.65 32066.73
#(between_SS / total_SS =  64.0 %)\



#44338.10 50463.26 35999.79 44218.98
#(between_SS / total_SS =  61.1 %)

#K-means clustering with 4 clusters of sizes 30839, 55071, 27932, 36278

#Cluster means:
 # Frequency DaysSinceLastPurchase        AOV
#1 -0.54815487             0.1924001  1.4391830
#2 -0.08523975            -0.5625389 -0.2581731
#3  1.68265211            -0.6968366 -0.5528633
#4 -0.70017786             1.2269197 -0.4058255



#K-means clustering with 4 clusters of sizes 38667, 64588, 12926, 33939

#Cluster means:
#  DaysSinceLastPurchase        AOV AvgDaysBtwOrders
#1             1.1159589 -0.4603204      -0.27744810 (Gold members that havent bought for a while)
#2            -0.6999975 -0.4703804      -0.22953848 (Gold members that have been in prog for a while but have a really low AOV)
#3            -0.1556443  0.3120679       2.14595239 (Gold Members that buy occasionally)
#4             0.1199951  1.3007557      -0.06438208 (Gold members that buy frequently and have a high AOV)





goldMemberCluster$cluster = seg.k$cluster

#writing clusters back into file
write.csv(goldMemberCluster,"goldMemberCluster.csv")


plot(goldMemberCluster$AOV,goldMemberCluster$TimeInProg,type='p',col=goldMemberCluster$cluster)


cluster1 = filter(goldMemberCluster,cluster == 1)

library(factoextra)
seg.k$cluster
fviz_cluster(scaledClusterSet, geom = "point", frame.type = "norm")

sil = silhouette(seg.k$cluster,dist(scaledClusterSet))




# Exploratory factor analysis
library(nFactors)
library(GPArotation)
?nScree
nScree(goldMemberCluster[,c(-1,-2,-3)])
eigen(cor(goldMemberCluster[,c(-1,-2,-3)]))
factanal(goldMemberCluster[,c(-1,-2,-3)],factors=3)
