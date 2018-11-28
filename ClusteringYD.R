library('dplyr')
library('tidyr')
#changing drive location
setwd("/Users/hraja/Documents/DataProjects/RAG/YD")

# reading tarocash SCV
ydSCV  = read.csv("PurchaseBehaviour.csv",header = TRUE,sep=',',stringsAsFactors = FALSE)

#reading SubscriberStats
subscriberStats = read.csv('YDSubStats.csv',header = TRUE,sep=',',stringsAsFactors = FALSE)

#reading SubscriberStatsWithCat
subStatsWithCat = read.csv('SubStatsbyCat.csv',header = TRUE,sep=',',stringsAsFactors = FALSE)


#combining SCV and SubscriberStats
Stats_SCV = ydSCV %>%
  inner_join(.,subscriberStats,by="EmailAddress")

pivot1 = spread(subStatsWithCat,Category,TotalSends) %>% select(.,SubscriberID,PriceOfferSends=`Price Offers`,SeasonalSends=`New Arrivals/Seasonal Offers`,EventsSends=`Events based Offers`,RestSends=`Rest`) 
pivot1[is.na(pivot1)] = 0
pivot1 = pivot1 %>% group_by(SubscriberID) %>% summarise (PriceOfferSends = max(PriceOfferSends),SeasonalSends = max(SeasonalSends),EventSends = max(EventsSends),RestSends = max(RestSends))

pivot2 = spread(subStatsWithCat,Category,TotalOpens) %>% select(.,SubscriberID,PriceOfferOpens=`Price Offers`,SeasonalOpens=`New Arrivals/Seasonal Offers`,EventsOpens=`Events based Offers`,RestOpens = `Rest`)
pivot2[is.na(pivot2)] = 0
pivot2 = pivot2 %>% group_by(SubscriberID) %>% summarise (PriceOfferOpens = max(PriceOfferOpens),SeasonalOpens = max(SeasonalOpens),EventOpens = max(EventsOpens),RestOpens = max(RestOpens))

pivot3 = spread(subStatsWithCat,Category,TotalClicks) %>% select(.,SubscriberID,PriceOfferClicks=`Price Offers`,SeasonalClicks=`New Arrivals/Seasonal Offers`,EventsClicks=`Events based Offers`,RestClicks = `Rest`)
pivot3[is.na(pivot3)] = 0
pivot3 = pivot3 %>% group_by(SubscriberID) %>% summarise (PriceOfferClicks = max(PriceOfferClicks),SeasonalClicks = max(SeasonalClicks),EventClicks = max(EventsClicks),RestClicks = max(RestClicks))

pivot4 = spread(subStatsWithCat,Category,TotalBounces) %>% select(.,SubscriberID,PriceOfferBounces=`Price Offers`,SeasonalBounces=`New Arrivals/Seasonal Offers`,EventsBounces=`Events based Offers`,RestBounces = `Rest`)
pivot4[is.na(pivot4)] = 0
pivot4 = pivot4 %>% group_by(SubscriberID) %>% summarise (PriceOfferBounces = max(PriceOfferBounces),SeasonalBounces = max(SeasonalBounces),EventBounces = max(EventsBounces),RestBounces = max(RestBounces))

pivot5 = spread(subStatsWithCat,Category,TotalUnsubscribes) %>% select(.,SubscriberID,PriceOfferUnsub=`Price Offers`,SeasonalUnsub=`New Arrivals/Seasonal Offers`,EventsUnsub=`Events based Offers`,RestUnsub = `Rest`)
pivot5[is.na(pivot5)] = 0
pivot5 = pivot5 %>% group_by(SubscriberID) %>% summarise (PriceOfferUnsub = max(PriceOfferUnsub),SeasonalUnsub = max(SeasonalUnsub),EventUnsub = max(EventsUnsub),RestUnsub = max(RestUnsub))

# Final subscriberStats by EmailCategories
finalPivot = pivot1 %>%
             inner_join(pivot2,by="SubscriberID") %>%
             inner_join(pivot3,by="SubscriberID") %>%
             inner_join(pivot4,by="SubscriberID") %>%
             inner_join(pivot5,by="SubscriberID")



subStatsSummary = finalPivot %>%
                  mutate(.,PriceOpenRate = PriceOfferOpens/(PriceOfferSends-PriceOfferBounces) ) %>%
                  mutate(.,PriceClickRate = PriceOfferClicks/(PriceOfferSends-PriceOfferBounces) ) %>%
                  mutate(.,EventsOpenRate = EventOpens/(EventSends-EventBounces) ) %>%
                  mutate(.,EventsClickRate = EventClicks/(EventSends-EventBounces) ) %>%
                  mutate(.,SeasonalOpenRate = SeasonalOpens/(SeasonalSends-SeasonalBounces) ) %>%
                  mutate(.,SeasonalClickRate = SeasonalClicks/(SeasonalSends-SeasonalBounces) ) %>%
                  mutate(.,RestOpenRate = RestOpens/(RestSends-RestBounces) ) %>%
                  mutate(.,RestClickRate = RestClicks/(RestSends-RestBounces) )
# removing NAs
n = complete.cases(subStatsSummary)
filteredsubStatsSummary = subStatsSummary[n,] %>%
                          select(.,SubscriberID,PriceOpenRate,PriceClickRate,EventsOpenRate,EventsClickRate,SeasonalOpenRate,SeasonalClickRate,RestOpenRate,RestClickRate)



#combining Stats_SCV with subscriberStats by Category

consolidatedDataSet = Stats_SCV
                      #inner_join(filteredsubStatsSummary,by="SubscriberID")

consolidatedDataSet = mutate(consolidatedDataSet,TotalOpenRate = TotalOpens/(TotalSends-TotalBounces),TotalClickRate=TotalClicks/(TotalSends-TotalBounces),TotalCTOR = TotalClicks/TotalOpens)


#removing unsubscribes from the dataset
# 319708 customers
consolidatedDataSet = filter(consolidatedDataSet,TotalUnsubscribes==0)

#calculating Recency
consolidatedDataSet = mutate(consolidatedDataSet,Recency = as.numeric(as.Date("2016-09-30") -as.Date(RecentPurchaseDt) ))

# calculating days in program
consolidatedDataSet = mutate(consolidatedDataSet, DaysInProg = as.numeric(as.Date("2016-09-30")-as.Date(Opted.InDate)))


# calculating age
consolidatedDataSet = mutate(consolidatedDataSet, Age = ifelse(DOB=="NULL",0,(Sys.Date()-as.Date(DOB,""))/365))

#normalising lifetime value by days in program

consolidatedDataSet = mutate(consolidatedDataSet,CLVNorm = as.numeric(LifetimeValue)/DaysInProg)


# percentage of sale items of total items
consolidatedDataSet = mutate(consolidatedDataSet,SalePct = SaleItems/TotalItems)

#cleaning customers with sale percentage > 1. These customers have no of sale items greater than total items. Checked in the database.
#Problem is in POS_Order the NumberofItems related to that order is less than the actual items in Order Details.

consolidatedDataSet = filter(consolidatedDataSet,SalePct <= 1)


#filtering out subscribers with open rate > 1
consolidatedDataSet = filter(consolidatedDataSet,TotalOpenRate <= 1)

# adding lifetime value as clv
consolidatedDataSet = mutate(consolidatedDataSet,CLV = as.numeric(LifetimeValue))

# adding averageorder size as numeric
consolidatedDataSet = mutate(consolidatedDataSet,AOS = as.numeric(AvgOrderSize))


#checking outliers in dataset
outliers = filter(consolidatedDataSet,AOS > 10)

consolidatedDataSet = filter(consolidatedDataSet,AOS < 10)


#filtering out infinities from CTORs
#consolidatedDataSet = filter(consolidatedDataSet,PriceCTOR != 'Inf')
#consolidatedDataSet = filter(consolidatedDataSet,EventsCTOR != 'Inf')
#consolidatedDataSet = filter(consolidatedDataSet,SeasonalCTOR != 'Inf')
consolidatedDataSet = filter(consolidatedDataSet,TotalCTOR != 'Inf')
# removing subscribers with ctor > 1
consolidatedDataSet = filter(consolidatedDataSet,TotalCTOR <=1)
# creating a cluster set for doing kmeans


ClusterSet = select(consolidatedDataSet,c(CustomerID,DOB,CLV,Frequency,Recency,AOS,AvgOrderValue,SalePct,TotalClickRate,TotalOpenRate))

library('corrplot')
M = cor(ClusterSet)
corrplot(M,method='circle')

scaledClusterSet = data.frame(scale(ClusterSet[c(-1,-2,-7,-8)]))

wss <- (nrow(scaledClusterSet)-1)*sum(apply(scaledClusterSet,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(scaledClusterSet, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
#K-means clustering with 7 clusters of sizes 87675, 27406, 7620, 69506, 59785, 26403, 33900
seg.k6 <- kmeans(scaledClusterSet, 6) # 5 cluster solution

#K-means clustering with 6 clusters of sizes 128769, 32145, 2572, 27427, 105882, 15500

#Cluster means:
#           CLV    Frequency     Recency         AOS TotalClickRate TotalOpenRate
#1 -0.163555635 -0.013103656 -0.66074443 -0.29388098     -0.1211378    -0.2816857 (rookie)
#2  0.589162286 -0.240184293 -0.04369149  2.10500159     -0.1138370    -0.2212544 (occasional)
#3 -0.001390389  0.012357416 -0.49099313  0.05945765      8.7839284     2.3249228 (researcher)
#4 -0.070735442 -0.008499705 -0.33357971 -0.06494891      0.5006518     2.6442281 (window shopper)
#5 -0.367209751 -0.304721025  1.05772730 -0.28887336     -0.1768740    -0.3478360 (lapsed buyer)
#6  2.770762180  2.701540544 -0.97383835  0.15434852      0.1072394     0.1104021 (dream customer)


# adding clusters to the cluster set

ClusterSet$cluster = seg.k6$cluster
write.csv(ClusterSet,"ydSegments.csv",row.names = FALSE)


aggregate(ClusterSet[,c(-1,-5)],by=list(seg.k$cluster),FUN=mean)



#if we remove sale items
wss <- (nrow(scaledClusterSet)-1)*sum(apply(scaledClusterSet,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(scaledClusterSet, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
seg.k5 <- kmeans(scaledClusterSet, 5) # 5 cluster solution
seg.k7 = kmeans(scaledClusterSet, 7)

#K-means clustering with 5 clusters of sizes 4128, 6416, 16329, 32962, 13318
#CLV   Frequency     Recency        AOS TotalClickRate TotalOpenRate
#1  2.6315672  2.86677662 -0.77826408  0.2061749      0.7452083     0.1849806 (High lifetime value,high frequency,high email engagement,More likely to shop both online and instore,high average order value) Dream Customer
#2  0.7947442 -0.17432799 -0.09126684  2.1781240     -0.1345709    -0.2357213 (High average order sizes, low frequency, low recency and low email engagment) Occasional Buyer
#3 -0.3810975 -0.34320427  1.44085391 -0.1943737     -0.2699950    -0.3057502 (low lifetime value, low frequency, havent bought for a while,low engagement) Lapsed Buyer
#4 -0.2349686 -0.12551487 -0.50133773 -0.2906311     -0.1969293    -0.4939767 (Low lifetime value, low frequency, low recency and low email engagement. tend to buy when items on sale,low average order size) Recent Buyer
#5 -0.1497380 -0.07314626 -0.24060423 -0.1555956      0.6522842     1.6536900 (High email engagement, but low lifetime value.) Window Shopper


#Online Purchases
#Cluster 1 = 841/4128 = 20%
#Cluster 2 = 573/6416 = 7.4%
#Cluster 3 = 417/16239 = 2.6%
#Cluster4 = 1231/32962 = 3.7%
#Cluster5 = 939/12218 = 7.6%

#Cluster 1 has a higher avg lifetime value.Cluster 3 has the lowest avg lifetime value.
#Avg click rate is higher for Cluster 1. Click Rate for Cluster3 is the lowest
#Cluster 5 has a higher average Open Rate, Cluster 4 has the lowest average open rate
#Avg click rate on 

#Adding clusters back into cluster set

ClusterSet$cluster = seg.k5$cluster

boxplot(ClusterSet$RestClickRate ~ ClusterSet$cluster, ylab="LV", xlab="Cluster")
table(ClusterSet$cluster,cut(ClusterSet$RestClickRate,3))

#create a csv file with clusters
write.csv(ClusterSet,"tarocashSegments.csv",row.names = FALSE)

View(ClusterSet)

#Model based clustering

library(mclust)
seg.mc = Mclust(ClusterSet)
summary(seg.mc)



#High average order size, 
#High frequency and high lifetime value
#People who engage with Price Offers
#People who buy items not on sale
#People who havent shopped recently and are likely to buy items not on sale
#People who havent shopped recently


write.csv(consolidatedDataSet,"consolidatedDataSet.csv")


library('corrplot')
M = cor(ClusterSet[,c(-1,-5)])
corrplot(M,method='circle')
# Using just purchase and email behaviour
BehaviouralCluster = select(consolidatedDataSet,c(SubscriberID,as.numeric(LifetimeValue),Frequency,Recency,
                                                  PriceOpenRate,PriceClickRate,EventsOpenRate,EventsClickRate,SeasonalOpenRate,SeasonalClickRate,
                                                  RestOpenRate,RestClickRate))
BehaviouralCluster = mutate(BehaviouralCluster,CLV = as.numeric(LifetimeValue))

scaledClusterSet = data.frame(scale(BehaviouralCluster[,c(-1,-2)]))

wss <- (nrow(scaledClusterSet)-1)*sum(apply(scaledClusterSet,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(scaledClusterSet, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")



#Modified Behavioural CLuster
BehaviouralCluster2 = select(consolidatedDataSet,c(SubscriberID,LifetimeValue,Frequency,Recency,AvgOrderValue,TotalOpenRate,TotalClickRate))
BehaviouralCluster2 = mutate(BehaviouralCluster2,CLV = as.numeric(LifetimeValue))
scaledClusterSet = data.frame(scale(BehaviouralCluster2[,c(-1,-2)]))

wss <- (nrow(scaledClusterSet)-1)*sum(apply(scaledClusterSet,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(scaledClusterSet, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

seg.k <- kmeans(scaledClusterSet, 5) 
# (between_SS / total_SS =  40.2 %) with CLV,F,R,TotalOpenRtae,TotalClick,SalePct

#High Open Rate, Low Lifetime Value
#Low Open and Click Rate, Low Lifetime Value
#Low Open and Click Rate, High Recency
#High Frequency, Low Recency, High Click Rate, High LifetimeValue



#Doing RFM Analysis

# (between_SS / total_SS =  58.8 %) with Avg Order Value and SalePct
#(between_SS / total_SS =  58.4 %) without AOV and with SalePct
#(between_SS / total_SS =  57.7 %) with average order value
#(between_SS / total_SS =  50.7 %) with aov,salepct,totalopenrate,totalclickrate
#(between_SS / total_SS =  56.5 %) with RFM variables.
# (between_SS / total_SS =  63.0 %) for rfm and salepct with 5 clusters
# (between_SS / total_SS =  56.8 %) with 5 clusters and rfm,salepct and TotalClickRate
# (between_SS / total_SS =  60.1 %) with rfm,salepct, TotalOpenRate
#(between_SS / total_SS =  66.1 %) with rfm and 4 clusters
# (between_SS / total_SS =  61.7 %) with rfm and aos
# (between_SS / total_SS =  55.9 %) with rfm and aos and salepct

ClusterRFM = select(ClusterSet,c(LifetimeValue,Frequency,Recency,SalePct,AOS,TotalOpenRate,TotalClickRate))



ClusterRFMScaled = data.frame(scale(ClusterRFM[,-1]))
wss <- (nrow(ClusterRFMScaled)-1)*sum(apply(ClusterRFMScaled,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(ClusterRFMScaled, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
seg.k <- kmeans(ClusterRFMScaled, 7) # 5 cluster solution

#Frequency      Recency    SalePct         AOS TotalOpenRate TotalClickRate         CLV
#1 -0.3103374  1.537418678  0.3897817 -0.11628720    -0.3541988    -0.22947551 -0.39819295   (Low Engagement, Low LifetimeValue)
#2 -0.1158915 -0.526570721  0.6355945 -0.27306209    -0.4316181    -0.14249817 -0.31868193   ()
#3  0.4541879 -0.521989849  0.2107228  0.02966945     1.1149258     4.24695733  0.20457320
#4 -0.2435700 -0.005202131 -1.4692881 -0.26648873    -0.4001049    -0.23221824 -0.07186187
#5  2.8938539 -0.761796279 -0.1164647  0.18816561     0.1457687     0.36206950  2.66104786
#6 -0.1018273 -0.040042311  0.0341562 -0.14824732     2.0072475     0.02173971 -0.15708847
#7 -0.1779364 -0.116132928 -0.1641966  2.35750059    -0.2176577    -0.13176970  0.89674329






boxplot(scaledClusterSet$CLVNorm ~ seg.k$cluster, ylab="LV", xlab="Cluster")