setwd('/Users/hraja/Documents/DataProjects/Avis')

#coupons = read.csv('CouponClusters.csv',sep=',',stringsAsFactors = FALSE)
coupons = read.csv('CouponRedemption.csv',sep=',',stringsAsFactors = FALSE)

#This time i need to go with business segments:
#1. Which discounts appeal to cost conscious or sporadic travellers. Can cost consciousness be determined from the car class type. Those without a wizard account
# dont have a car class type.
#2. Repeat Leisure Rental Activity:
#3. First time renters
#4. Customers with total rental history of 6+ rentals or over $1000 are considered High Value candidates while the remaining are Standard.
#5. Non My Avis Customers who have opted in to receive emails.
#6. Avis Wizard Members.
#7. Customers who have redeemed coupons previously:

#Coupon redemption behaviour shouldbe done for MPP,TPP and UPP.


library(dplyr)
library(tidyr)


View(coupons)

coupons = mutate(coupons,Type = substring(COUPON_NO,0,3))

#coupons_filtered = filter(coupons,Type == 'MPP' | Type == 'TPP' | Type == 'UPD' | Type == 'UPN' | Type == 'UPP')
coupons_filtered = filter(coupons,Type == 'MPP' | Type == 'TPP' | Type == 'UPP')

mpp = coupons_filtered %>%
        filter(.,Type == 'MPP') %>%
        group_by(.,UNIQUE_ID) %>%
        summarise(MPP_No =n()) 

tpp = coupons_filtered %>%
  filter(.,Type == 'TPP') %>%
  group_by(.,UNIQUE_ID) %>%
  summarise(TPP_No =n()) 


upp = coupons_filtered %>%
  filter(.,Type == 'UPP') %>%
  group_by(.,UNIQUE_ID) %>%
  summarise(UPP_No =n()) 

upd = coupons_filtered %>%
  filter(.,Type == 'UPD') %>%
  group_by(.,Unique_ID) %>%
  summarise(UPD_No =n()) 

upn = coupons_filtered %>%
  filter(.,Type == 'UPN') %>%
  group_by(.,Unique_ID) %>%
  summarise(UPN_No =n()) 
coupons_deduped = coupons_filtered %>% group_by(.,UNIQUE_ID) %>% summarise()
coupons_joined = coupons_deduped %>%
                 left_join(.,mpp,'UNIQUE_ID') %>%
                 left_join(.,tpp,'UNIQUE_ID') %>%
                 left_join(.,upp,'UNIQUE_ID') 
#                 left_join(.,upd,'Unique_ID') %>%
#                 left_join(.,upn,'Unique_ID')

coupons_joined[is.na(coupons_joined)] = 0


#coupons_joined = mutate(coupons_joined,MPPPerc = MPP_No/(MPP_No+TPP_No+UPD_No+UPN_No+UPP_No)) %>%
#                 mutate(.,TPPPerc = TPP_No/(MPP_No+TPP_No+UPD_No+UPN_No+UPP_No)) %>%
#                 mutate(.,UPDPerc = UPD_No/(MPP_No+TPP_No+UPD_No+UPN_No+UPP_No)) %>%
#                 mutate(.,UPNPerc = UPN_No/(MPP_No+TPP_No+UPD_No+UPN_No+UPP_No)) %>%
#                 mutate(.,UPPPerc = UPP_No/(MPP_No+TPP_No+UPD_No+UPN_No+UPP_No))

coupons_joined = mutate(coupons_joined,MPPPerc = MPP_No/(MPP_No+TPP_No+UPP_No)) %>%
                 mutate(.,TPPPerc = TPP_No/(MPP_No+TPP_No+UPP_No)) %>%
                 mutate(.,UPPPerc = UPP_No/(MPP_No+TPP_No+UPP_No))

#coupons_kmeans = dplyr::select(coupons_joined,c(MPPPerc,TPPPerc,UPDPerc,
#                                         UPNPerc,UPPPerc))
set.seed(10)
coupons_kmeans = dplyr::select(coupons_joined,c(MPPPerc,TPPPerc,UPPPerc))

wss <- (nrow(coupons_kmeans)-1)*sum(apply(coupons_kmeans,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(coupons_kmeans, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#Scree plot

ratio_ss = rep(0,7)

for (k in (1:7)) {
  coupon_km = kmeans(coupons_kmeans,k,nstart = 20)
  ratio_ss[k] = coupon_km$tot.withinss/coupon_km$totss
}
plot(ratio_ss,type = "b",xlab = "k", ylab = "Ratio SS")


set.seed(10)
seg.k4 = kmeans(coupons_kmeans, 4,nstart=20)
seg.k3 = kmeans(coupons_kmeans,3,nstart = 20)
seg.k5 = kmeans(coupons_kmeans,5,nstart = 20)

#K-means clustering with 5 clusters of sizes 14968, 58206, 5898, 51023, 37293

#Cluster means:
#  MPPPerc     TPPPerc     UPDPerc      UPNPerc     UPPPerc
#1 0.03031912 0.023517303 0.902343009 0.0029546041 0.040865966  (Double Upgrade) A
#2 0.98684015 0.002894519 0.001530989 0.0015104864 0.007223853  (Earn Points and get discounts) B
#3 0.05000510 0.016123228 0.017594475 0.8616732912 0.054603906  (Double Points + Upgrade) C
#4 0.03987183 0.019572665 0.003519217 0.0026172714 0.934419012  (Free upgrade) D
#5 0.02406692 0.970107945 0.001387210 0.0004681775 0.003969747  (Free day) E




#K-means clustering with 3 clusters of sizes 59238, 50948, 57202

#Cluster means:
#  MPPPerc     TPPPerc     UPDPerc     UPNPerc     UPPPerc
#1 0.97828154 0.002764897 0.006599226 0.005255552 0.007098788 (Earn points/ Discounts)
#2 0.04033982 0.003635809 0.012725514 0.008704257 0.934594601 (Free Upgrade)
#3 0.01947481 0.654582841 0.225362486 0.080600343 0.019979521 (Free day and double upgrade)

#K-means clustering with 3 clusters of sizes 50948, 57202, 59238
#MPPPerc     TPPPerc     UPDPerc     UPNPerc     UPPPerc
#1 0.04033982 0.003635809 0.012725514 0.008704257 0.934594601
#2 0.01947481 0.654582841 0.225362486 0.080600343 0.019979521
#3 0.97828154 0.002764897 0.006599226 0.005255552 0.007098788



#KMeans with 3 Clusters for NEW DATA
#K-means clustering with 3 clusters of sizes 39175, 53249, 61480

#Cluster means:
#  MPPPerc    TPPPerc     UPPPerc
#1 0.003669065 0.99218867 0.004142265
#2 0.042672189 0.02340476 0.933923056
#3 0.973767673 0.01868191 0.007550419


#comparing ratio of within sum of squares
wss_ratio = seg.k3$tot.withinss/seg.k4$tot.withinss

wss_to_tss_k4 = seg.k4$tot.withinss/seg.k4$totss
wss_to_tss_k3 = seg.k3$tot.withinss/seg.k3$totss
wss_to_tss_k5 = seg.k5$tot.withinss/seg.k5$totss



# plotting k3 and k4

par(mfrow=c(1,3))
plot(coupons_kmeans$MPPPerc,coupons_kmeans$UPPPerc,main = "K = 3",col=seg.k3$cluster)
plot(log(coupons_joined$MPP_No),log(coupons_joined$UPP_No),col=seg.k3$cluster)

plot(coupons_kmeans$MPPPerc,coupons_kmeans$UPPPerc,main = "K = 4",col=seg.k4$cluster)

plot(coupons_kmeans$MPPPerc,coupons_kmeans$UPPPerc,main = "K = 5", col=seg.k5$cluster)

#Three ways to evaluate clusters and choose k in kmeans
# 1. Ratio of within cluster sum of squares to total sum of squares to measure compactness and separation of clusters
# 2. Silhouette Score: (average distance from members in nearest cluster - average distance to those in my cluster)/maximum of those two averages
      # +1 : perfect clusters, 0: some members ill suited, -1: lots of customers better off hanging in the other cluster.
# 3. Dunn Index = minimum intercluster distance / maximum cluster diameter
# 4. using the elbow method in a scree plot to choose the right k value

#clvalid package for dunns index

library(cluster)
library(fpc)

clusplot(coupons_kmeans, seg.k4$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
#calculating silhouette score for k4
sil4 = silhouette(seg.k4$cluster,dist(coupons_kmeans))



library(clValid)

dunn_km = dunn(clusters = seg.k4$cluster,Data = coupons_kmeans)



#Applying hierchical clustering

coupon_dist = dist(coupons_kmeans)











#Lets proceed with 5 clusters and goto classification

coupons_kmeans$cluster = seg.k5$cluster
coupons_joined$cluster = seg.k5$cluster

coupons_joined$cluster = seg.k3$cluster
clusterList = coupons_joined$cluster
categoryList = rep("",length(clusterList))
for (i in 1:length(clusterList)) {
  if (clusterList[i] == 1) {
    categoryList[i] = "A"
  }
  else if(clusterList[i] == 2) {
    categoryList[i] = "B"
  }
  else if(clusterList[i] == 3) {
    categoryList[i] = "C"
  }
  else if(clusterList[i] == 4) {
    categoryList[i] = "D"
  }
  else if(clusterList[i] == 5) {
    categoryList[i] = "E"
  }
}
coupons_joined$Category = categoryList

#avis_mcv = read.csv('Avis_MCV.csv',sep=',',stringsAsFactors = FALSE)
#AvisMCV_09-17- Updated file with more attributes and features

avis_mcv = read.csv('AvisMCV_09-17.csv',sep=',',stringsAsFactors = FALSE)
coupons_joined = mutate(coupons_joined,UNIQUE_ID = as.character(Unique_ID))
coupon_classification = coupons_joined %>%
                        inner_join(.,avis_mcv,'UNIQUE_ID')
                        
classify_coupons = dplyr::select(coupon_classification,c(PRESTIGE_STATUS,FTP_BAND,
                                                  CAR_CLASS_PREFERRED,AGE_BAND,DAYS_SINCE_FIRST_RENTAL,
                                                  OPTIN_EMAIL,OPTIN_DM,RENTALS_COUNT_12_MNTH,
                                                  HAS_WIZARD_RECORD,LEISURE_RENTALS_OVERALL,
                                                  BUSINESS_RENTALS_OVERALL,TOUR_RENTALS_OVERALL,
                                                  GENDER,GPS_PREFERENCE,cluster))
# changing NULLS to zeros
classify_coupons[classify_coupons$RENTALS_COUNT_12_MNTH == "NULL",]$RENTALS_COUNT_12_MNTH = 0
classify_coupons[classify_coupons$DAYS_SINCE_FIRST_RENTAL == "NULL",]$DAYS_SINCE_FIRST_RENTAL = 0
classify_coupons[classify_coupons$PRESTIGE_STATUS == "NULL",]$PRESTIGE_STATUS = "Non-Wizard Customer"
classify_coupons[classify_coupons$FTP_BAND == "NULL",]$FTP_BAND = "Non-Frequent Traveller"
# to remove the following error:
#1: In data.matrix(x) : NAs introduced by coercion


classify_coupons['cluster'] = lapply(classify_coupons['cluster'],as.factor)
classify_coupons['PRESTIGE_STATUS'] = lapply(classify_coupons['PRESTIGE_STATUS'],as.factor)
classify_coupons['FTP_BAND'] = lapply(classify_coupons['FTP_BAND'],as.factor)
classify_coupons['CAR_CLASS_PREFERRED'] = lapply(classify_coupons['CAR_CLASS_PREFERRED'],as.factor)
classify_coupons['AGE_BAND'] = lapply(classify_coupons['AGE_BAND'],as.factor)
classify_coupons['DAYS_SINCE_FIRST_RENTAL'] = lapply(classify_coupons['DAYS_SINCE_FIRST_RENTAL'],as.integer)
classify_coupons['RENTALS_COUNT_12_MNTH'] = lapply(classify_coupons['RENTALS_COUNT_12_MNTH'],as.integer)
classify_coupons['GENDER'] = lapply(classify_coupons['GENDER'],as.factor)

classify_coupons[is.na(classify_coupons)] = 0
classify_coupons[is.na(classify_coupons$PRESTIGE_STATUS),]$PRESTIGE_STATUS = "Non-Wizard Customer"
classify_coupons[classify_coupons$FTP_BAND == "NULL",]$FTP_BAND = "Non-Frequent Traveller"
#classify_coupons[classify_coupons$PRESTIGE_STATUS == "NULL",]$PRESTIGE_STATUS = "No Preference"

checkNull = function(x){
  x == "NULL"
}
checkNull("NULL")
classify_coupons = classify_coupons[!is.null(classify_coupons),]

classify_coupons = classify_coupons[!grepl("NULL", classify_coupons$PRESTIGE_STATUS),]
classify_coupons = classify_coupons[!grepl("NULL", classify_coupons$FTP_BAND),]
classify_coupons = classify_coupons[!grepl("NULL", classify_coupons$CAR_CLASS_PREFERRED),]
classify_coupons = classify_coupons[!grepl("NULL", classify_coupons$AGE_BAND),]
classify_coupons = classify_coupons[!grepl("NULL", classify_coupons$DAYS_SINCE_FIRST_RENTAL),]
classify_coupons = classify_coupons[!grepl("NULL", classify_coupons$OPTIN_EMAIL),]
classify_coupons = classify_coupons[!grepl("NULL", classify_coupons$OPTIN_DM),]
classify_coupons = classify_coupons[!grepl("NULL", classify_coupons$RENTALS_COUNT_12_MNTH),]
classify_coupons = classify_coupons[!grepl("NULL", classify_coupons$HAS_WIZARD_RECORD),]
classify_coupons = classify_coupons[!grepl("NULL", classify_coupons$LEISURE_RENTALS_OVERALL),]
classify_coupons = classify_coupons[!grepl("NULL", classify_coupons$BUSINESS_RENTALS_OVERALL),]
classify_coupons = classify_coupons[!grepl("NULL", classify_coupons$TOUR_RENTALS_OVERALL),]
classify_coupons = classify_coupons[!grepl("NULL", classify_coupons$GENDER),]
classify_coupons = classify_coupons[!grepl("NULL", classify_coupons$GPS_PREFERENCE),]

table(classify_coupons$CAR_CLASS_PREFERRED == "NULL")

apply(classify_coupons, 2, function(x) any(is.infinite(x)))
classify_coupons = classify_coupons[complete.cases(classify_coupons),]

classify_coupons = classify_coupons[,c(-3,-13)]

# 22000 subscribers
#creating a random forest model
library(randomForest)
train.prop = 0.75
train.cases = sample(nrow(classify_coupons),nrow(classify_coupons) * train.prop)
seg.rf.train = classify_coupons[train.cases,]
seg.rf.test = classify_coupons[-train.cases,]
seg.rf = randomForest(cluster ~ ., data = seg.rf.train,importance = TRUE )
plot(seg.rf)
varImpPlot(seg.rf, main="Variable importance by segment")
#The mean decrease accuracy is how much the model fit decreases when you drop a variable.  
#The greater the drop the more significant the variable.  
#I'd say the mean decrease is more significant for variable selection.  
#You could use a GINI index to describe the overall explanatory power of the variables. 
#Are they all equally as important, or does one have much greater explanatory value.  
#So mean decrease accuracy would be more important for variable selection.  
#GINI would give you information at a glance about the explanatory relationship between the variables selected.

pred = predict(seg.rf,seg.rf.test)

table(pred,seg.rf.test$cluster)
#Accuracy = 47%

# Removing Optin email from the model
train.prop = 0.75
classify_coupons2 = classify_coupons[,c(-5,-6)]
train.cases = sample(nrow(classify_coupons2),nrow(classify_coupons2) * train.prop)
seg.rf.train = classify_coupons2[train.cases,]
seg.rf.test = classify_coupons2[-train.cases,]
seg.rf = randomForest(cluster ~ ., data = seg.rf.train,importance = TRUE )
pred = predict(seg.rf,seg.rf.test)

table(pred,seg.rf.test$cluster)
#Accuracy 46%
varImpPlot(seg.rf, main="Variable importance by segment")


library(gplots)
library(RColorBrewer)
heatmap.2(t(importance(seg.rf)[ , 1:4]),
             col=brewer.pal(9, "Blues"),
             dend="none", trace="none", key=FALSE,
             margins=c(10, 10),
             main="Variable importance by segment"
             )



#ROC Curve
library(ROCR)
predProbs = predict(seg.rf,seg.rf.test,type = "prob")[,2]
pred = prediction(predProbs,seg.rf.test$cluster)
perf = performance(pred,"tpr","fpr")
plot(perf)

library(MASS)
fit <- lm(Category~.,data=seg.rf.train)
step <- stepAIC(fit, direction="both")
step$anova

#classify_coupons %>% drop_na()
summary(classify_coupons)

table(seg.rf.train$Category)
#A    B    C    D    E 
#4682 1873 4843 1112 2113 
#is this a class imbalance problem???


A = sample_n(filter(classify_coupons,Category == 'A'),1731)
B = sample_n(filter(classify_coupons,Category == 'B'),1731)
C = sample_n(filter(classify_coupons,Category == 'C'),1731)
D = sample_n(filter(classify_coupons,Category == 'D'),1731)
E = sample_n(filter(classify_coupons,Category == 'E'),1731)

sample_df = rbind(A,B,C,D,E)
sample_df = sample_df[,c(-3,-4,-6,-7,-10,-13)]
train.cases1 = sample(nrow(sample_df),nrow(sample_df) * train.prop)
seg.rf.train = sample_df[train.cases1,]
seg.rf.test = sample_df[-train.cases1,]
seg.rf = randomForest(Category ~ ., data = seg.rf.train,ntree = 3000,importance = TRUE )

pred = predict(seg.rf,seg.rf.test)
table(pred,seg.rf.test$Category)

library(caret)
#train_control <- trainControl(method="cv", number=10)
model <- train(cluster~., data=seg.rf.train, method="nb")
model = NaiveBayes(cluster ~.,data = seg.rf.train)
predNaive = predict(model,seg.rf.test,type = "raw")
table(predNaive$class,seg.rf.test$cluster)
#16358/39926 = 41%



#naive Bayes Classifier without email and dm optin
train.prop = 0.75
classify_coupons2 = classify_coupons[,c(-5,-6)]
train.cases = sample(nrow(classify_coupons2),nrow(classify_coupons2) * train.prop)
seg.nb.train = classify_coupons2[train.cases,]
seg.nb.test = classify_coupons2[-train.cases,]
model = NaiveBayes(cluster ~.,data = seg.nb.train)
predNaive2 = predict(model,seg.nb.test,type = "raw")
table(predNaive2$class,seg.nb.test$cluster)
# Accuracy 38.3% 
#One big assumption for naive bayes is that the features should be independent of each other.

#Parametric algorithms such as logistic regression suffer from high bias
#Non parametric algorithms such as decision trees could suffer from high variance
# although the idea behind random forests is to reduce variance by generating a lot of trees.


#Why is the random forest model accuracy so low? It is aroundd 46-47%
# One reason: The model has a high bias:
#We show that random forest variable importance measures are a sensible means for variable selection in many applications, but are not reliable in situations where potential predictor variables vary in their scale of measurement or their number of categories. This is particularly important in genomics and computational biology, where predictors often include variables of different types, for example when predictors include both sequence data and continuous variables such as folding energy, or when amino acid sequence data show different numbers of categories.
#https://link.springer.com/article/10.1186%2F1471-2105-8-25

#Feature selection based on impurity reduction is biased towards preferring variables with most categories.We might be
# wrongly concluding that one of the variables is a stronger predictor compared to others.


# After looking at the data, it seems a lot of the variables like Prestige Status and FTP Band have multiple categories with
#only one category dominating the data set. In case of FTP Band its "QF"
#     FB Non-Frequent Traveller    NZ OTHER    QF
#1     0                   6509    50   371 36258
#2     4                   8679    73   531 27501
#3     6                  12891   129   798 25975

# So might just do some feature engineering and merge some categories for better interpretation. Probably QF vs Non QF
#Look at class distribution for values of different features.

#table(seg.rf.train$cluster,seg.rf.train$AGE_BAND)
#   <25 25-29 30-34 35-39 40-44 45-49 50-54 55-59 60-64  65+
#1  136  1289  3713  4704  5065  5824  5638  5788  4679 6352
#2  145  1640  3951  4298  4457  4703  4638  4668  3697 4591
#3  158  1347  4130  5022  5161  5185  4675  4600  3712 5809

# Age band could be reduced 
age = seg.rf.train %>% group_by(.,AGE_BAND) %>% summarise(count = n())
rentalcount = seg.rf.train %>% group_by(.,cluster) %>% summarise(avgRentalCount = mean(RENTALS_COUNT_12_MNTH))
p = plot_ly(x = age$AGE_BAND,y = age$count,
  type = "bar"
)

p = plot_ly(x = age$AGE_BAND,y = age$count,
            type = "bar"
)
table(classify_coupons$cluster,classify_coupons$GENDER)

#adding gender to the mix of features
classify_coupons2 = filter(classify_coupons,GENDER == "F" | GENDER =="M")

classify_coupons_consolidated = select(classify_coupons2,c(FTP_BAND,GENDER,AGE_BAND,DAYS_SINCE_FIRST_RENTAL,
                                                           RENTALS_COUNT_12_MNTH,HAS_WIZARD_RECORD,
                                                           LEISURE_RENTALS_OVERALL,BUSINESS_RENTALS_OVERALL,
                                                           TOUR_RENTALS_OVERALL,GPS_PREFERENCE,cluster)) %>%
                              mutate(.,FTP = ifelse(FTP_BAND == "Non-Frequent Traveller","No FTP","Have FTP"))
#%>%
                            #  mutate(.,AGE_BAND = )
ageList = classify_coupons2$AGE_BAND
ageBands = rep("0",length(ageList))
for (i in 1:length(ageList)){
  if (ageList[i] == "<25" || ageList[i] == "25-29" || ageList[i] == "30-34" || ageList[i] == "35-39"){
    ageBands[i] = "< 25 - 39"
  }
  else if(ageList[i] == "40-44" || ageList[i] == "45-49" || ageList[i] == "50-54" || ageList[i] == "55-59") {
    ageBands[i] = "40-59"
  }
  else if(ageList[i] == "60-64" || ageList[i] == "65+") {
    ageBands[i] = "> 65"
  }
}

classify_coupons_consolidated = mutate(classify_coupons_consolidated,AGE_BAND_NEW = ageBands)
classify_coupons_consolidated[classify_coupons_consolidated$AGE_BAND_NEW == "> 65",]$AGE_BAND_NEW = "> 60"


# Days since first rental is continuous. Could be made categorical with three categories to reduce variance.
#The idea is to check distribution using a histogram for these continuous variables and choose suitable cutoff points
# based on domain knowledge of the business. So in this case with years in program i chose 5 year intervals and then consolidated
# everything above 10 years as one due to low volume.
classify_coupons_consolidated = mutate(classify_coupons_consolidated,YearsinProg = DAYS_SINCE_FIRST_RENTAL/365)
classify_coupons_consolidated = mutate(classify_coupons_consolidated,YearsinProg2 = cut_interval(YearsinProg,length = 5, width = 5))

YearsinProg = classify_coupons_consolidated$YearsinProg
YearsInterval = rep("0",length(YearsinProg))
for (i in 1:length(YearsinProg)){
  if (YearsinProg[i] >=0 && YearsinProg[i] <= 5 ){
    YearsInterval[i] = "0 - 5 years"
  }
  else if(YearsinProg[i] > 5 && YearsinProg[i] <= 10) {
    YearsInterval[i] = "6 - 10 years"
  }
  else if(YearsinProg[i] > 10 ) {
    YearsInterval[i] = "> 10 years"
  }
}
classify_coupons_consolidated = mutate(classify_coupons_consolidated,Lifetime = YearsInterval)

# rentals count's mean is 1 in the last 12 months.
#table(classify_coupons_consolidated$RENTALS_COUNT_12_MNTH == 0)

#FALSE  TRUE 
#63133 96568 
# since majority class is those who havent rented in the last 12 months, will go with Have rented in the last 12 months (Yes/NO) 

classify_coupons_consolidated = mutate(classify_coupons_consolidated,RentedinLast12Mths = ifelse(RENTALS_COUNT_12_MNTH > 0,"Yes","No"))

# Now looking at leisure rentals overall
hist(classify_coupons_consolidated$LEISURE_RENTALS_OVERALL)
mean(classify_coupons_consolidated$LEISURE_RENTALS_OVERALL)
median(classify_coupons_consolidated$LEISURE_RENTALS_OVERALL)
sd(classify_coupons_consolidated$LEISURE_RENTALS_OVERALL)
max(classify_coupons_consolidated$LEISURE_RENTALS_OVERALL)

# Looking at the leisure rentals,business rentals and tour rentals, the dominating class is 0 so doesnt make sense to
# use it in the algorithm
#----------------------------------------------------
table(classify_coupons_consolidated$cluster,classify_coupons_consolidated$LEISURE_RENTALS_OVERALL)

#0      1 
#154810   4891 
#table(classify_coupons_consolidated$BUSINESS_RENTALS_OVERALL)

#0      1 
#134251  25450 
#table(classify_coupons_consolidated$TOUR_RENTALS_OVERALL)

#0      1 
#144282  15419 
#----------------------------------------------

classify_coupons_engineered = select(classify_coupons_consolidated,FTP,AGE_BAND_NEW,GENDER,YearsinProg,
                                     Lifetime,RentedinLast12Mths,HAS_WIZARD_RECORD,
                                     GPS_PREFERENCE,cluster)


classify_coupons_engineered['FTP'] = lapply(classify_coupons_engineered['FTP'],as.factor)
classify_coupons_engineered['AGE_BAND_NEW'] = lapply(classify_coupons_engineered['AGE_BAND_NEW'],as.factor)
classify_coupons_engineered['Lifetime'] = lapply(classify_coupons_engineered['Lifetime'],as.factor)
classify_coupons_engineered['RentedinLast12Mths'] = lapply(classify_coupons_engineered['RentedinLast12Mths'],as.factor)
classify_coupons_engineered['PRESTIGE_STATUS'] = classify_coupons2['PRESTIGE_STATUS']

train.cases_engineered = sample(nrow(classify_coupons_engineered),nrow(classify_coupons_engineered) * train.prop)
seg.rf.train_eng = classify_coupons_engineered[train.cases_engineered,]
seg.rf.test_eng = classify_coupons_engineered[-train.cases_engineered,]
seg.rf.eng = randomForest(cluster ~ ., data = seg.rf.train_eng,importance = TRUE )
#Accuracy 41.6%

pred = predict(seg.rf.eng,seg.rf.test_eng)

table(pred,seg.rf.test_eng$cluster)


varImpPlot(seg.rf.eng, main="Variable importance by segment")
heatmap.2(t(importance(seg.rf.eng2)[ , 1:2]),
          col=brewer.pal(9, "Blues"),
          dend="none", trace="none", key=FALSE,
          margins=c(10, 10),
          main="Variable importance by segment"
)


# Binary class: Price based vs Value Based
classify_coupons_engineered2 = mutate(classify_coupons_engineered,Segment = ifelse(cluster == 1,"Price based","Value based"))
classify_coupons_engineered2 = classify_coupons_engineered2[,c(-7)]
classify_coupons_engineered2['Segment'] = lapply(classify_coupons_engineered2['Segment'],as.factor)

train.cases_engineered2 = sample(nrow(classify_coupons_engineered2),nrow(classify_coupons_engineered2) * train.prop)
seg.rf.train_eng2 = classify_coupons_engineered2[train.cases_engineered2,]
seg.rf.test_eng2 = classify_coupons_engineered2[-train.cases_engineered2,]

seg.rf.eng2 = randomForest(Segment ~ ., data = seg.rf.train_eng2,importance = TRUE ,ntree = 1000)

varImpPlot(seg.rf.eng2, main="Variable importance by segment")

#class balancing error
# use down sampling to balance the majority and minority classes
nmin = 43263
sampsize = rep(nmin, 2)

seg.rf.sample = randomForest(Segment ~ ., data = seg.rf.train_reduced,importance = TRUE ,ntree = 1000,sampsize=sampsize)

pred2 = predict(seg.rf.eng2,seg.rf.test_eng2)

table(pred2,seg.rf.test_eng2$Segment)



seg.rf.train_reduced = seg.rf.train_eng2[,c(1,4,5,6,7)]
seg.rf.reduced= randomForest(Segment ~ ., data = seg.rf.train_reduced,importance = TRUE )

seg.rf.eng_down = randomForest(cluster ~ ., data = seg.rf.train_eng2,importance = TRUE,ntree = 1000,sampsize = rep(36798,3) )



# Now we know that there is a class balancing issue so lets do it with more predictors
#53%
seg.rf.final = randomForest(Segment ~ .,data = seg.rf.train_eng2,importance = TRUE,ntree = 1000,sampsize = rep(43263,2))
varImpPlot(seg.rf.final, main="Variable importance by segment")
heatmap.2(t(importance(seg.rf.final)[ , 1:2]),
          col=brewer.pal(9, "Blues"),
          dend="none", trace="none", key=FALSE,
          margins=c(10, 10),
          main="Variable importance by segment"
)

# how does this model perform on test and training set

predTrain = predict(seg.rf.final,seg.rf.train_eng2)
table(predTrain,seg.rf.train_eng2$Segment)

predTest = predict(seg.rf.final,seg.rf.test_eng2)
table(predTest,seg.rf.test_eng2$Segment)

# This model might be suffering from high bias since it performs poorly on both training and test set.



#41.1%
seg.rf.eng_downSample =  randomForest(cluster ~ .,data = seg.rf.train_eng,importance = TRUE,ntree = 1000,sampsize = rep(36798,3))
varImpPlot(seg.rf.eng_downSample, main="Variable importance by segment")

library(ROCR)
predProbs = predict(seg.rf.final,seg.rf.test_eng2,type = "prob")[,2]
pred = prediction(predProbs,seg.rf.test_eng2$Segment)
predProbs2 = predict(seg.rf.eng_down,seg.rf.test_eng2,type = "prob")[,2] 
pred2 = prediction(predProbs2,seg.rf.test_eng2$Segment)
perf = performance(pred,"tpr","fpr")
perf2 = performance(pred2,"tpr","fpr")
plot(perf,col = "blue")
plot(perf2, add = TRUE, col = "red")


# using caret package for cross validation
#Accuracy:  40%
train_control <- trainControl(method="cv", number=10)
model <- train(cluster~., data=trainSampled, trControl = train_control,method="rf")



# reduce the training set and then do the classification. Cross Validation takes a ridiculous amount of time
trainSampled = seg.rf.train_eng %>% group_by(cluster) %>% sample_n(size = 10000)

#If your model is performing really we;ll on the training set, but much poorer on the holdout set, then its suffering
# high variance. If your model performs poorly on both training and test data sets , it suffering from high bias.



#Improving model acuracy
# 1. Add more data: Drive variance down
# 2. Add more features: Imcreases model flexibility and reduces bias
# 3. Do feature selection
# 4. Use Regularization:
# 5: Bagging: Boostrap Aggregation: use several versions of the same model trained on slightly different samples of training data to reduce variance
# 6: Boosting: Training several models succesively each trying to learn from the errors of models preceding it.


#Adding feature: Step 2:
classify_coupons_engineered['PRESTIGE_STATUS'] = classify_coupons['PRESTIGE_STATUS']
classify_add_feature = filter(classify_coupons_engineered,PRESTIGE_STATUS == 'Non-Wizard Customer' | PRESTIGE_STATUS == 'PREF RENTER')

#sample by Prestige status
PrestigeSampled = classify_add_feature %>% group_by(PRESTIGE_STATUS) %>% sample_n(size = 9891)
train.cases_prestige = sample(nrow(PrestigeSampled),nrow(PrestigeSampled) * train.prop)
seg.rf.train_prestige = PrestigeSampled[train.cases_prestige,]
seg.rf.test_prestige = PrestigeSampled[-train.cases_prestige,]
seg.rf.prestige =  randomForest(cluster ~ .,data = seg.rf.train_prestige,importance = TRUE,ntree = 1000,sampsize = rep(4576,3))
#41% accuracy
# Class error for second class is really high.Majority of class 2 is being identified as class 3.
#1 2812 51 2269   0.4520655
#2 2191 24 2361   0.9947552
#3 1971 33 3124   0.3907956
varImpPlot(seg.rf.prestige, main="Variable importance by segment")


#Again i ll change it to two classes: merge 2 and 3
PrestigeSampledBinary = mutate(PrestigeSampled,Segment = ifelse(cluster == 1,"Price based","Value based"))
PrestigeSampledBinary['Segment'] = lapply(PrestigeSampledBinary['Segment'],as.factor)
PrestigeSampledBinary = PrestigeSampledBinary[,-7]
train.cases_prestige_binary = sample(nrow(PrestigeSampledBinary),nrow(PrestigeSampledBinary) * train.prop)
seg.rf.train_prestige_binary = PrestigeSampledBinary[train.cases_prestige_binary,]
seg.rf.test_prestige_binary = PrestigeSampled[-train.cases_prestige_binary,]

#54.3%
seg.rf.prestige_binary =  randomForest(Segment ~ .,data = seg.rf.train_prestige_binary,importance = TRUE,ntree = 3000,sampsize = rep(5155,2))
varImpPlot(seg.rf.prestige_binary, main="Variable importance by segment")

heatmap.2(t(importance(seg.rf.prestige_binary)[ , 1:2]),
          col=brewer.pal(9, "Blues"),
          dend="none", trace="none", key=FALSE,
          margins=c(10, 10),
          main="Variable importance by segment"
)


#Added gender,prestige status
PrestigeSampledNew = classify_add_feature %>% group_by(FTP) %>% sample_n(size = 26260)
PrestigeSampledNew = PrestigeSampledNew %>% group_by(PRESTIGE_STATUS) %>% sample_n(size = 9193)
PrestigeSampledNew = PrestigeSampledNew %>% group_by(AGE_BAND_NEW) %>% sample_n(size = 4226)
PrestigeSampledNew2 = PrestigeSampledNew %>% group_by(AGE_BAND_NEW) %>% sample_n(size = 3512)
#Remove Years in Prog and Has Wizard Record: Has wizard record is same as Prestige Status. does not add extra info
PrestigeSampledNew2 = PrestigeSampledNew2[,-4]
PrestigeSampledNew2 = PrestigeSampledNew2[,-6]
train.prop = 0.75
train.cases_prestige_new = sample(nrow(PrestigeSampledNew2),nrow(PrestigeSampledNew2) * train.prop)
seg.rf.train_prestige_new = PrestigeSampledNew2[train.cases_prestige_new,]
seg.rf.test_prestige_new = PrestigeSampledNew2[-train.cases_prestige_new,]
library(randomForest)
seg.rf.prestige =  randomForest(cluster ~ .,data = seg.rf.train_prestige_new,importance = TRUE,ntree = 3000,sampsize = rep(2429,3))
varImpPlot(seg.rf.prestige, main="Variable importance by segment")
library(gplots)
library(RColorBrewer)
heatmap.2(t(importance(seg.rf.prestige)[ , 1:3]),
          col=brewer.pal(9, "Blues"),
          dend="none", trace="none", key=FALSE,
          margins=c(10, 10),
          main="Variable importance by segment"
)

pred = predict(seg.rf.prestige,seg.rf.test_prestige_new)
table(pred,seg.rf.test_prestige_new$cluster)

#The method calls for class balancing: Not feature balancing. Above i tried to sample equal feature sizes for all features. Thats 
#not correct. We are losing information because of that. Only class labels should be balanced
#One way to toss out unimportant features is to check their p values.
#LASSO (Least Absolute SHrinkage and Selection Operator) is a powerful feature selection technique that is useful for regression problems. Lasso is a regularization method. Its a way to reduce overfitting
#using less complicated functions

classify_add_feature = classify_add_feature[,-4]
train.prop = 0.75
train.cases_features = sample(nrow(classify_add_feature),nrow(classify_add_feature) * train.prop)
seg.rf.train_features = classify_add_feature[train.cases_features,]
seg.rf.test_features = classify_add_feature[-train.cases_features,]
seg.rf.features =  randomForest(cluster ~ .,data = seg.rf.train_features,importance = TRUE,ntree = 3000,sampsize = rep(26787,3))
varImpPlot(seg.rf.features, main="Variable importance by segment")
heatmap.2(t(importance(seg.rf.features)[ , 1:3]),
          col=brewer.pal(9, "Blues"),
          dend="none", trace="none", key=FALSE,
          margins=c(10, 10),
          main="Variable importance by segment"
)

pred = predict(seg.rf.features,seg.rf.test_features)
#41%
table(pred,seg.rf.test_features$cluster)

classify_add_feature_binary = mutate(classify_add_feature,Segment = ifelse(cluster == 3,"Price based","Value based"))
classify_add_feature_binary['Segment'] = lapply(classify_add_feature_binary['Segment'],as.factor)
classify_add_feature_binary = classify_add_feature_binary[,c(-4,-7,-8,-9)]
#drop the levels from the GENDER column
classify_add_feature_binary$GENDER = droplevels(classify_add_feature_binary$GENDER)
classify_add_feature_binary$PRESTIGE_STATUS = droplevels(classify_add_feature_binary$PRESTIGE_STATUS)

train.prop = 0.75
library(randomForest)
train.cases_features = sample(nrow(classify_add_feature_binary),nrow(classify_add_feature_binary) * train.prop)
seg.rf.train_feature_binary = classify_add_feature_binary[train.cases_features,]
seg.rf.test_feature_binary = classify_add_feature_binary[-train.cases_features,]
seg.rf.features_binary =  randomForest(Segment ~ .,data = seg.rf.train_feature_binary,importance = TRUE,ntree = 3000,sampsize = rep(31678,2))
varImpPlot(seg.rf.features_binary, main="Variable importance by segment")
#so we had 46.3% error with "Has Wizard Record" and after removing it we get 46.15%

# With Has wizard record
#OOB estimate of  error rate: 46.3%
#Confusion matrix:
#  Price based Value based class.error
#Price based       22203        9453   0.2986164
#Value based       31352       25132   0.5550598

#Without wizard record

#OOB estimate of  error rate: 46.15%
#Confusion matrix:
#  Price based Value based class.error
#Price based       22279        9505   0.2990498
#Value based       31171       25185   0.5531088




#remove GPS Preference
classify_add_feature_binary = classify_add_feature_binary[,-6]

#Without GPS error rate improves to 45.99%
#OOB estimate of  error rate: 45.99%
#Confusion matrix:
##  Price based Value based class.error
#Price based       21740        9940   0.3137626
#Value based       30595       25865   0.5418881
heatmap.2(t(importance(seg.rf.features_binary)[ , 1:2]),
          col=brewer.pal(9, "Blues"),
          dend="none", trace="none", key=FALSE,
          margins=c(10, 10),
          main="Variable importance by segment"
)

write.csv(classify_add_feature_binary,"classify_add_feature_binary.csv",row.names = FALSE)

sample_rf = seg.rf.train_feature_binary %>% group_by(Segment) %>% sample_n(size = 31635)
write.csv(sample_rf,"train_rf.csv",row.names = FALSE)


#OOB estimate of  error rate: 45.84%
#Confusion matrix:
#  Price based Value based class.error
#Price based       21466       10269   0.3235859
#Value based       30136       26269   0.5342789

# really random behaviour with the same model.
#OOB estimate of  error rate: 42.37%
#Confusion matrix:
#  Price based Value based class.error
#Price based       20624       11011   0.3480639
#Value based       26336       30169   0.4660826

pred = predict(seg.rf.features_binary,seg.rf.test_feature_binary)
table(pred,seg.rf.test_feature_binary$Segment)

#FTP is the single biggest predictor between price and value based offers. if we need to find other variables
# i need to remove and try the model without it
classify_add_feature_binary_wFTP = classify_add_feature_binary[,-1]
train.cases_features = sample(nrow(classify_add_feature_binary_wFTP),nrow(classify_add_feature_binary_wFTP) * train.prop)
seg.rf.train_feature_binarywFTP = classify_add_feature_binary_wFTP[train.cases_features,]
seg.rf.test_feature_binarywFTP = classify_add_feature_binary_wFTP[-train.cases_features,]
seg.rf.features_binarywFTP =  randomForest(Segment ~ .,data = seg.rf.train_feature_binarywFTP,importance = TRUE,ntree = 3000,sampsize = rep(31642,2))
varImpPlot(seg.rf.features_binarywFTP, main="Variable importance by segment")
heatmap.2(t(importance(seg.rf.features_binarywFTP)[ , 1:2]),
          col=brewer.pal(9, "Blues"),
          dend="none", trace="none", key=FALSE,
          margins=c(10, 10),
          main="Variable importance by segment"
)
write.csv(seg.rf.train_feature_binarywFTP,"seg.rf.train_feature_binarywFTP.csv",row.names = FALSE)
sample_rfwFTP = seg.rf.train_feature_binarywFTP %>% group_by(Segment) %>% sample_n(size = 31642) %>% data.frame(.)
write.csv(sample_rfwFTP,"sample_rfwFTP.csv",row.names = FALSE)
# removing ftp lights up other variables especially age and prestige status

#using xgboost:



library(xgboost)
classify_xgboost = classify_add_feature
classify_xgboost['clusterClass'] = lapply(classify_xgboost['cluster'],as.numeric)
classify_xgboost['clusterClass'] = classify_xgboost$clusterClass -1
classify_xgboost = classify_xgboost[,-9]
classify_xgboost$FTP = as.numeric(classify_xgboost$FTP)
# xgboost only takes in numeric variables. need to convert categorical variables using one hot encoding.
library(caret)
#,AGE_BAND_NEW,GENDER,Lifetime,PRESTIGE_STATUS
classify_xgboost$GENDER = droplevels(classify_xgboost$GENDER)
classify_xgboost$PRESTIGE_STATUS = droplevels(classify_xgboost$PRESTIGE_STATUS)
classify_xgboost$FTP = classify_xgboost$FTP - 1
classify_xgboost$GENDER = as.numeric(classify_xgboost$GENDER)
classify_xgboost$GENDER = classify_xgboost$GENDER - 1
classify_xgboost$PRESTIGE_STATUS = as.numeric(classify_xgboost$PRESTIGE_STATUS)
classify_xgboost$PRESTIGE_STATUS = classify_xgboost$PRESTIGE_STATUS - 1
classify_xgboost$RentedinLast12Mths = as.numeric(classify_xgboost$RentedinLast12Mths)
classify_xgboost$RentedinLast12Mths = classify_xgboost$RentedinLast12Mths - 1



dummy = dummyVars("~AGE_BAND_NEW",data = classify_xgboost)
age <- data.frame(predict(dummy, newdata = classify_xgboost))
classify_xgboost["25 - 39"] = age['AGE_BAND_NEW...25...39']
classify_xgboost["40 - 59"] = age['AGE_BAND_NEW.40.59']
classify_xgboost["60+"] = age['AGE_BAND_NEW...60']

dummy = dummyVars("~Lifetime",data = classify_xgboost)
lifetime <- data.frame(predict(dummy, newdata = classify_xgboost))
classify_xgboost["Lifetime 0 - 5"] = lifetime['Lifetime.0...5.years']
classify_xgboost["Lifetime 6 - 10"] = lifetime['Lifetime.6...10.years']
classify_xgboost["Lifetime 10+"] = lifetime['Lifetime...10.years']

classify_xgboost = classify_xgboost[,c(-2,-5,-7,-8)]

train_index = sample(nrow(classify_xgboost),nrow(classify_xgboost) * train.prop)

#Full data set
data_variables <- as.matrix(classify_xgboost[,-5])
data_label <- classify_xgboost$clusterClass
data_matrix <- xgb.DMatrix(data = data_variables, label = data_label)

# split train data and make xgb.DMatrix
train_data   <- data_variables[train_index,]
train_label  <- data_label[train_index]
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)

# split test data and make xgb.DMatrix
test_data  <- data_variables[-train_index,]
test_label <- data_label[-train_index]
test_matrix <- xgb.DMatrix(data = test_data, label = test_label)

numberOfClasses <- length(unique(classify_xgboost$clusterClass))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses)
nround    <- 50 # number of XGBoost rounds
cv.nfold  <- 5
# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model <- xgb.cv(params = xgb_params,
                   data = train_matrix, 
                   nrounds = nround,
                   nfold = cv.nfold,
                   verbose = FALSE,
                   prediction = TRUE)
# confusion matrix
# confusion matrix
OOF_prediction <- data.frame(cv_model$pred) %>%
  mutate(max_prob = max.col(., ties.method = "last"),
         label = train_label + 1)
confusionMatrix(factor(OOF_prediction$label), 
                factor(OOF_prediction$max_prob),
                mode = "everything")

bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = nround)
names = colnames(classify_xgboost[,-5])
importance_matrix = xgb.importance(feature_names = names,model = bst_model)
head(importance_matrix)

gp = xgb.plot.importance(importance_matrix)
print(gp) 

xgboost_binary = mutate(classify_xgboost,Segment = ifelse(clusterClass == 2,1,0))
xgboost_binary = xgboost_binary[,-6]

xgboost_binary = xgboost_binary[,-3]


# Get equal samples of 0 and 1
xgboost_sampled = xgboost_binary %>% group_by(.,Segment) %>% sample_n(42288) %>% data.frame(.)

train_index = sample(nrow(xgboost_sampled),nrow(xgboost_sampled) * 0.75)

training_set = xgboost_sampled[train_index,-11]
test_set = xgboost_sampled[-train_index,-11]
trainLabel = xgboost_sampled[train_index,]$Segment
testLabel = xgboost_sampled[-train_index,]$Segment

dtrain = xgb.DMatrix(data = as.matrix(training_set),label = trainLabel)
dtest = xgb.DMatrix(data = as.matrix(test_set),label = testLabel)

params = list(booster = "gbtree", objective = "binary:logistic", eta = 0.3, gamma = 0, max_depth = 6,min_child_weight = 1, subsample = 1, colsample_bytree = 1)
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)

xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 1, watchlist = list(val=dtest,train=dtrain), print.every.n = 10, early.stop.round = 10, maximize = F , eval_metric = "auc")
xgbpred <- predict (xgb1,dtest)
xgbpred <- ifelse (xgbpred > 0.5,1,0)
library(caret)
confusionMatrix (xgbpred, testLabel)
mat <- xgb.importance (feature_names = colnames(training_set),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20]) 


data_variables_2 <- as.matrix(xgboost_binary[,-12])
data_label_2 <- xgboost_binary$Segment
data_matrix_3 <- xgb.DMatrix(data = data_variables_2, label = data_label_2)

# split train data and make xgb.DMatrix
train_data2   <- data_variables_2[train_index,]
train_label2  <- data_label_2[train_index]
train_matrix2 <- xgb.DMatrix(data = train_data2, label = train_label2)

# split test data and make xgb.DMatrix
test_data2  <- data_variables_2[-train_index,]
test_label2 <- data_label_2[-train_index]
test_matrix2 <- xgb.DMatrix(data = test_data2, label = test_label2)

numberOfClasses <- length(unique(xgboost_binary$Segment))
xgb_params <- list("objective" = "binary:logistic",
                   "eval_metric" = "error",
                   "num_class" = numberOfClasses)
nround    <- 50 # number of XGBoost rounds
cv.nfold  <- 5
# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model <- xgb.cv(params = xgb_params,
                   data = train_matrix2, 
                   nrounds = nround,
                   nfold = cv.nfold,
                   verbose = FALSE,
                   prediction = TRUE)
OOF_prediction <- data.frame(cv_model$pred) %>%
  mutate(max_prob = max.col(., ties.method = "last"),
         label = train_label2 + 1)
confusionMatrix(factor(OOF_prediction$label), 
                factor(OOF_prediction$max_prob),
                mode = "everything")
bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix2,
                       nrounds = nround)
names = colnames(xgboost_binary[,-12])
importance_matrix = xgb.importance(feature_names = names,model = bst_model)
head(importance_matrix)

gp = xgb.plot.importance(importance_matrix)
print(gp) 

# testing the model on test set
pred = predict(bst_model,test_data2)
prediction = as.numeric(pred > 0.5)
err = mean(as.numeric(pred > 0.5) != test_label2)
print(paste("test-error=", err))

#xgboosts builds on weak learners to create strong learners. it learns from errors in the previous models and then tries to improve 
# upon those errors and missclassification rate.

#Evaluating Classification Models
# Confusion Matrix
# Cross Validation
# Accuracy = (TP+TN)/(TP+FP+TN+FN)
# ROC Curves: Plotting true positive rate against false positive rate and checking the area under the curve
# True Positive Rate = TP/(TP + FN)
# False Positive Rate = FP/(FP + TN)
# Precision = TP/TP + FP
# Recall = True Positive rate
# F1 Score = 2 * (Precision * Recall)/(Precision + Recall)




#Conclusion
#1. Customers part of a Frequent Traveller program are more responsive to price based discounts than value based offers.
#2. Preferred Renters are more responsive to value based offers even though majority are a part of a frequent travellers program. More than 90% are a part of FTP. 65% of those who have FTP respond to value based offers compared to 58% of non wizard customers that have FTP and are attracted to value based offers.
#3. Younger audience segments from 25 - 39 respond to value based offers.
#4. Older customers are more responsive to price based offers compared to new customers.
#5. 
#6. 




#NEW AND UPDATED DATA WORK with only 3 coupons MPP, TPP and UPP

classify_coupons = dplyr::select(coupon_classification,c(PRESTIGE_STATUS,FTP_BAND,
                                                         AGE_BAND,YEARS_SINCE_FIRST_RENTAL,
                                                         LOR_BAND,TOTAL_SPEND_12_MNTH_BAND,TOTAL_TK_12_MNTH_BAND,RENTALS_COUNT_12_MNTH,
                                                         HAS_WIZARD_RECORD,LEISURE_RENTALS_OVERALL,
                                                         BUSINESS_RENTALS_OVERALL,TOUR_RENTALS_OVERALL,PACKAGE_FLAG,SEGMENT,
                                                         GENDER,GPS_PREFERENCE,cluster))


# Making the Frequent Flyer status a binary variable (0 means they are not part of the prog)
classify_coupons = mutate(classify_coupons,FTP_Status = ifelse(FTP_BAND == "NULL",0,1))


#Finding out sporadic travelers using Rentals_Count
table(classify_coupons$RENTALS_COUNT_12_MNTH)

classify_coupons[classify_coupons$RENTALS_COUNT_12_MNTH == "NULL",]$RENTALS_COUNT_12_MNTH = 0


# differentiating between sporadic travellers and repeat renters
classify_coupons = mutate(classify_coupons,TravellerType = ifelse(RENTALS_COUNT_12_MNTH == '0' | RENTALS_COUNT_12_MNTH == '1',"Sporadic Travellers","Repeat Renters"))


#Identifying first time renters: Renters that have rental count = 1 and years since first rental either 0 or 1

classify_coupons = mutate(classify_coupons,FirstTimeRenter = ifelse((RENTALS_COUNT_12_MNTH == '1') & (YEARS_SINCE_FIRST_RENTAL == '0' | YEARS_SINCE_FIRST_RENTAL == '1'),1,0))

#table(classify_coupons$PRESTIGE_STATUS,classify_coupons$HAS_WIZARD_RECORD)
# All preferred members have a wizard record.
#AVIS PREFERRED PLUS      0      3
#NULL                110635      0
#PREF RENTER              0  28037
#PRESIDENT CLUB           0     11
#PRESIDENT CLUB PREF      0    338
#STANDARD WIZARD          0   8007

#Part of preferred program: identified if a person has a wizard record, he is part of the avis prefered
classify_coupons = mutate(classify_coupons,AvisPreferred = ifelse(HAS_WIZARD_RECORD == 1,1,0))

classify_coupons = mutate(classify_coupons,TotalFlagRentals = LEISURE_RENTALS_OVERALL + BUSINESS_RENTALS_OVERALL + TOUR_RENTALS_OVERALL)


# There are only 1220 unique subscribers who have done business and leisure rentals. Its really a small population so no worth it to include in the model.



# Differentiate between customers who spent more than a 1000 dollars in a year compared to those who didnt.


classify_coupons = mutate(classify_coupons,SpendOver1000 = ifelse(TOTAL_SPEND_12_MNTH_BAND == '1 - 249' |
                                                                    TOTAL_SPEND_12_MNTH_BAND == '250 - 499' |
                                                                  TOTAL_SPEND_12_MNTH_BAND == '500 - 999' | TOTAL_SPEND_12_MNTH_BAND == 'None',0,1))

classify_final = select(classify_coupons,c(AGE_BAND,LOR_BAND,TOTAL_SPEND_12_MNTH_BAND,TOTAL_TK_12_MNTH_BAND,HAS_WIZARD_RECORD,PACKAGE_FLAG,SEGMENT,GENDER,GPS_PREFERENCE,FTP_Status,TravellerType,FirstTimeRenter,
                                           AvisPreferred,SpendOver1000,cluster))
classify_final = filter(classify_final,GENDER == 'M' | GENDER == 'F')
classify_final['AGE_BAND'] = lapply(classify_final['AGE_BAND'],as.factor)
classify_final['LOR_BAND'] = lapply(classify_final['LOR_BAND'],as.factor)
classify_final['TOTAL_SPEND_12_MNTH_BAND'] = lapply(classify_final['TOTAL_SPEND_12_MNTH_BAND'],as.factor)
classify_final['TOTAL_TK_12_MNTH_BAND'] = lapply(classify_final['TOTAL_TK_12_MNTH_BAND'],as.factor)
classify_final['HAS_WIZARD_RECORD'] = lapply(classify_final['HAS_WIZARD_RECORD'],as.factor)
classify_final['PACKAGE_FLAG'] = lapply(classify_final['PACKAGE_FLAG'],as.factor)
classify_final['SEGMENT'] = lapply(classify_final['SEGMENT'],as.factor)
classify_final['GENDER'] = lapply(classify_final['GENDER'],as.factor)
classify_final['GPS_PREFERENCE'] = lapply(classify_final['GPS_PREFERENCE'],as.factor)
classify_final['FTP_Status'] = lapply(classify_final['FTP_Status'],as.factor)
classify_final['TravellerType'] = lapply(classify_final['TravellerType'],as.factor)
classify_final['FirstTimeRenter'] = lapply(classify_final['FirstTimeRenter'],as.factor)
classify_final['AvisPreferred'] = lapply(classify_final['AvisPreferred'],as.factor)
classify_final['SpendOver1000'] = lapply(classify_final['SpendOver1000'],as.factor)
classify_final['cluster'] = lapply(classify_final['cluster'],as.factor)

train.prop = 0.75
train.cases = sample(nrow(classify_final),nrow(classify_final) * train.prop)
seg.rf.train = classify_final[train.cases,]
seg.rf.test = classify_final[-train.cases,]
library(randomForest)
set.seed(100)
seg.rf = randomForest(cluster ~ ., data = seg.rf.train,importance = TRUE ,ntree = 3000,sampsize = rep(20985,3))
varImpPlot(seg.rf, main="Variable importance by segment")

#Feature Selection Algorithms
#filter methods: chi squared test, information gain and correlation coefficient scores.
#wrapper methods: forward and backward passes to add and remove features. stepwise regressions

caret::varImp(seg.rf)
#                             1         2          3
#AGE_BAND                  45.851343  27.94199  -2.357127
#LOR_BAND                 291.771802 298.08337  92.504398
#TOTAL_SPEND_12_MNTH_BAND  59.981830  53.10795   4.419908
#TOTAL_TK_12_MNTH_BAND     60.220291  42.86101   8.848030
#HAS_WIZARD_RECORD         32.636326  33.93862   6.397128
#PACKAGE_FLAG              95.981145  17.12935  19.363828
#SEGMENT                   55.930094  60.07720  37.294806
#GENDER                     8.107064  17.02326  -4.358676
#GPS_PREFERENCE            90.397330 -12.70794 -18.955925
#FTP_Status                77.437955  57.62769  51.770760
#TravellerType             50.608903  33.47262  18.547245
#FirstTimeRenter           69.786914  12.40672 -27.144192
#AvisPreferred             33.432054  32.67242   7.617400
#SpendOver1000             32.160841 -18.64541  34.027217


#Looking at the importance of features on each cluster
#For cluster 1: LOR_BAND, PACKAGE_FLAG,GPS_PREFERENCE,FTP_STATUS and FirstTimeRenter are Important
#For cluster 2: LOR_BAND,FTP_Status
#For cluster 3: LOR_BAND,FTP_Status

write.csv(classify_final,"classify_final.csv",row.names = FALSE)




#Model Accuracy remains around approx 50%. Time for some serious feature engineering
#Use a combination of features to find the signal. 
#Look at the features by importance scores outlined by random forest and build a combination of those
#LOR_BAND,FTP_STATUS,TravellerType,FirstTimeRenter,AvisPrefferred,SpendOver1000,PACKAGE FLAG.
#1. Customers part of FTP program that have an average length of rental more than 6 days (1/0)
#2. Customers part of FTP programs that have an average LOR of 0-3 (1/0)
#3. Customers part of FTP proram that have an average LOR of 3 -6 (1/0)
#4 Customers not part of the FTP program but have an average LOR greater than 6 days (1/0)
#5 Customers not part of FTP program and have an average LOR < 3 days
#6 Sporadic travellers that are part of the FTP program
#7 Repeat renters that are not part of the FTP Program.
#8 FirstTime Renters who are part of FTP
#9.First time Renters that rent for more than 6 days
#7. Avis Preferred Customers who are not part of the FTP Program
#8. Avis Preferred Customers who rent for less than 3 days
#9. Avis Preferred Customers whose average LOR is greater than 6 days.
#10 Avis Preferred Customers who are sporadic travellers
#11 Avis Preferred Customers who are repeat renters.
#12 Customers part of FTP that spent over 1000 dollars in the last 12 months
#12 FTP Customers that order extra package
#13 Customers with average LOR > 6 days that order an extra package.
#14 Customers that have spent over a 1000 dollars in the last 12 months and have ordered a package
#15 First time renters who have spend over a 1000 dollars in the last 12 months.
#16 Repeat renters that have spent over 1000 dollars in the last 12 months but are not part of Avis Preffered.


library(dplyr)
library(caret)
library(randomForest)


classify_modified = read.csv('classify_modified.csv',sep=',',stringsAsFactors = FALSE)

# Customers part of FTP program that have an average length of rental more than 6 days (1/0)

classify_modified = mutate(classify_modified,FTP_LOR_6_or_more = ifelse((FTP_Status == 1 & LOR_BAND_NEW == '6 and above'),1,0))

#Customers part of FTP program that have an average LOR of 0-3 (1/0)
classify_modified = mutate(classify_modified,FTP_LOR_below_3 = ifelse((FTP_Status == 1 & LOR_BAND_NEW == 'Below 3'),1,0))

#Customers part of FTP proram that have an average LOR of 3 -6 (1/0)
classify_modified = mutate(classify_modified,FTP_LOR_3_to_6 = ifelse((FTP_Status == 1 & LOR_BAND_NEW == '3 - 6'),1,0))


#Customers not part of the FTP program but have an average LOR greater than 6 days (1/0)
classify_modified = mutate(classify_modified,No_FTP_LOR_6_or_more = ifelse((FTP_Status == 0 & LOR_BAND_NEW == '6 and above'),1,0))

#Customers not part of FTP program and have an average LOR < 3 days
classify_modified = mutate(classify_modified,No_FTP_LOR_below_3 = ifelse((FTP_Status == 0 & LOR_BAND_NEW == 'Below 3'),1,0))

#Customers not part of FTP program and have an average LOR 3-6 days
classify_modified = mutate(classify_modified,No_FTP_LOR_3_to_6 = ifelse((FTP_Status == 0 & LOR_BAND_NEW == '3 - 6'),1,0))

#The three above are really small populations so might have to discard these ones,
classify_modified['No_FTP_LOR_6_or_more'] = NULL
classify_modified['No_FTP_LOR_below_3'] = NULL
classify_modified['No_FTP_LOR_3_to_6'] = NULL

#Sporadic travellers that are part of the FTP program
classify_modified = mutate(classify_modified,FTP_Sporadic_Travellers = ifelse((FTP_Status == 1 & TravellerType == 'Sporadic Travellers'),1,0))


#
#7 Repeat renters that are not part of the FTP Program.
classify_modified = mutate(classify_modified,No_FTP_Repeat_Renters = ifelse((FTP_Status == 0 & TravellerType == 'Repeat Renters'),1,0))

classify_modified['No_FTP_Repeat_Renters'] = NULL


#FirstTime Renters who are part of FTP
classify_modified = mutate(classify_modified,FTP_FirstTimeRenter = ifelse((FTP_Status == 1 & FirstTimeRenter == 1),1,0))
classify_modified['FTP_FirstTimeRenter'] = NULL


#Avis Preferred Customers who are not part of the FTP Program
classify_modified = mutate(classify_modified,Preferred_and_No_FTP = ifelse((AvisPreferred == 1 & FTP_Status == 0),1,0))
classify_modified['Preferred_and_No_FTP'] = NULL


#FTP Customers that are not part of the Avis Preferred Program
classify_modified = mutate(classify_modified,FTP_No_Preferred = ifelse((AvisPreferred == 0 & FTP_Status == 1),1,0))

#Repeat Customers that are not part of the Avis Preferred Program

#Find out if there are enough training examples available.
table(classify_modified$TravellerType,classify_modified$AvisPreferred)
#No



#8. Avis Preferred Customers who rent for less than 3 days
table(classify_modified$AvisPreferred,classify_modified$LOR_BAND_NEW)
#No




#FTP Customers that spent over 1000 dollars in the last 12 months
table(classify_modified$FTP_Status,classify_modified$SpendOver1000)

#FTP Customers that spent less than a 1000 dollars in the last 12 months.
classify_modified = mutate(classify_modified,FTP_Spend_LessThan1000 = ifelse((FTP_Status == 1 & SpendOver1000 == 0),1,0))

#How many training examples do i have for that feature.
# FTP Customers that order extra package
table(classify_modified$FTP_Status,classify_modified$PACKAGE_FLAG)

classify_modified = mutate(classify_modified,FTP_PackageFlag = ifelse((FTP_Status == 1 & PACKAGE_FLAG == 1),1,0))

#Customers with average LOR > 6 days that order an extra package.
table(classify_modified$PACKAGE_FLAG,classify_modified$LOR_BAND_NEW)


#FTP Customers that did not rent in the last 12 months

classify_modified = mutate(classify_modified,FTP_NotRent = ifelse((FTP_Status == 1 & RentedinLast12Mths == 0),1,0))


#AvisPreferredCustomers that did not rent in the last 12 months.
classify_modified = mutate(classify_modified,Preferred_NotRent = ifelse((AvisPreferred == 1 & RentedinLast12Mths == 0),1,0))
classify_modified['Preferred_NotRent'] = NULL


#Sporadic Travellers that rent below 3 days
classify_modified = mutate(classify_modified,Sporadic_Below_3 = ifelse((TravellerType == 'Sporadic Travellers' & LOR_BAND_NEW == "Below 3"),1,0))


#Sporadic Travellers that rent 6 and above days
classify_modified = mutate(classify_modified,Sporadic_Above_6 = ifelse((TravellerType == 'Sporadic Travellers' & LOR_BAND_NEW == "6 and above"),1,0))



classify_data = classify_modified
classify_data['FTP_Status'] = lapply(classify_data['FTP_Status'],as.factor)
classify_data['TravellerType'] = lapply(classify_data['TravellerType'],as.factor)
classify_data['FirstTimeRenter'] = lapply(classify_data['FirstTimeRenter'],as.factor)
classify_data['AvisPreferred'] = lapply(classify_data['AvisPreferred'],as.factor)
classify_data['SpendOver1000'] = lapply(classify_data['SpendOver1000'],as.factor)
classify_data['PACKAGE_FLAG'] = lapply(classify_data['PACKAGE_FLAG'],as.factor)
classify_data['LOR_BAND_NEW'] = lapply(classify_data['LOR_BAND_NEW'],as.factor)
classify_data['RentedinLast12Mths'] = lapply(classify_data['RentedinLast12Mths'],as.factor)
classify_data['cluster'] = lapply(classify_data['cluster'],as.factor)
classify_data['FTP_LOR_6_or_more'] = lapply(classify_data['FTP_LOR_6_or_more'],as.factor)
classify_data['FTP_LOR_below_3'] = lapply(classify_data['FTP_LOR_below_3'],as.factor)
classify_data['FTP_LOR_3_to_6'] = lapply(classify_data['FTP_LOR_3_to_6'],as.factor)
classify_data['FTP_Sporadic_Travellers'] = lapply(classify_data['FTP_Sporadic_Travellers'],as.factor)
classify_data['FTP_No_Preferred'] = lapply(classify_data['FTP_No_Preferred'],as.factor)
classify_data['FTP_Spend_LessThan1000'] = lapply(classify_data['FTP_Spend_LessThan1000'],as.factor)
classify_data['FTP_PackageFlag'] = lapply(classify_data['FTP_PackageFlag'],as.factor)
classify_data['FTP_NotRent'] = lapply(classify_data['FTP_NotRent'],as.factor)
classify_data['Sporadic_Below_3'] = lapply(classify_data['Sporadic_Below_3'],as.factor)
classify_data['Sporadic_Above_6'] = lapply(classify_data['Sporadic_Above_6'],as.factor)



for(i in 1:ncol(classify_data)) {
  a = lapply(classify_data[,i],as.factor)
  classify_data[,i] = a
}

write.csv(classify_data,'classify_data.csv',row.names = FALSE)


library(FSelector)#For method
weights<- chi.squared(cluster~., classify_data)

print(weights)

subset<- cutoff.k(weights, 5)

featuresSample = classify_data %>% group_by(.,cluster) %>% sample_n(.,27950)

library(randomForest)
seg.rf.features =  randomForest(cluster ~ .,data = featuresSample,importance = TRUE,ntree = 1000,sampsize = rep(27950,3))

heatmap.2(t(importance(seg.rf.features)[ , 1:2]),
          col=brewer.pal(9, "Blues"),
          dend="none", trace="none", key=FALSE,
          margins=c(10, 10),
          main="Variable importance by segment"
)

tab <- xtabs(~FTP_Status + TravellerType, data = classify_data)
summary(assocstats(tab))


train_control <- trainControl(method="cv", number=10)
model <- train(cluster~., data=featuresSample, trControl = train_control,method="rf")



classify_data['AGE_BAND'] = classify_final['AGE_BAND']
classify_data['GENDER'] = classify_final['GENDER']
classify_data['TOTAL_TK_12_MNTH_BAND'] = classify_final['TOTAL_TK_12_MNTH_BAND']
classify_data['TOTAL_TK_12_MNTH_BAND'] = NULL


ageList = classify_data$AGE_BAND
ageBands = rep("0",length(ageList))
for (i in 1:length(ageList)){
  if (ageList[i] == "25-29" || ageList[i] == "30-34" || ageList[i] == "35-39"){
    ageBands[i] = "25 - 39"
  }
  else if(ageList[i] == "40-44" || ageList[i] == "45-49" || ageList[i] == "50-54" ) {
    ageBands[i] = "40 - 54"
  }
  else if(ageList[i] == "55-59" || ageList[i] == "60-64" || ageList[i] == "65+") {
    ageBands[i] = "55 and above"
  }
  else {
    ageBands[i] = "0"
  }
}

classify_data = mutate(classify_data,AGE_BAND_NEW = ageBands)
classify_data = filter(classify_data,AGE_BAND_NEW  != "0")


classify_data_final = read.csv('classify_data_final.csv',sep=',',stringsAsFactors = FALSE)
classify_data_final['FTP_Status'] = lapply(classify_data_final['FTP_Status'],as.factor)
classify_data_final['TravellerType'] = lapply(classify_data_final['TravellerType'],as.factor)
classify_data_final['FirstTimeRenter'] = lapply(classify_data_final['FirstTimeRenter'],as.factor)
classify_data_final['AvisPreferred'] = lapply(classify_data_final['AvisPreferred'],as.factor)
classify_data_final['SpendOver1000'] = lapply(classify_data_final['SpendOver1000'],as.factor)
classify_data_final['PACKAGE_FLAG'] = lapply(classify_data_final['PACKAGE_FLAG'],as.factor)
classify_data_final['LOR_BAND_NEW'] = lapply(classify_data_final['LOR_BAND_NEW'],as.factor)
classify_data_final['RentedinLast12Mths'] = lapply(classify_data_final['RentedinLast12Mths'],as.factor)
classify_data_final['cluster'] = lapply(classify_data_final['cluster'],as.factor)
classify_data_final['FTP_LOR_6_or_more'] = lapply(classify_data_final['FTP_LOR_6_or_more'],as.factor)
classify_data_final['FTP_LOR_below_3'] = lapply(classify_data_final['FTP_LOR_below_3'],as.factor)
classify_data_final['FTP_LOR_3_to_6'] = lapply(classify_data_final['FTP_LOR_3_to_6'],as.factor)
classify_data_final['FTP_Sporadic_Travellers'] = lapply(classify_data_final['FTP_Sporadic_Travellers'],as.factor)
classify_data_final['FTP_No_Preferred'] = lapply(classify_data_final['FTP_No_Preferred'],as.factor)
classify_data_final['FTP_Spend_LessThan1000'] = lapply(classify_data_final['FTP_Spend_LessThan1000'],as.factor)
classify_data_final['FTP_PackageFlag'] = lapply(classify_data_final['FTP_PackageFlag'],as.factor)
classify_data_final['FTP_NotRent'] = lapply(classify_data_final['FTP_NotRent'],as.factor)
classify_data_final['Sporadic_Below_3'] = lapply(classify_data_final['Sporadic_Below_3'],as.factor)
classify_data_final['Sporadic_Above_6'] = lapply(classify_data_final['Sporadic_Above_6'],as.factor)

#More features
# 25 - 39 Customers part of FTP Program
# 40 - 54 Customers part of FTP Program
# 55 and above Customers part of FTP Program.
# Males part of FTP Program
#Females part of FTP Program
#
classify_data_final = mutate(classify_data_final,FTP_and_25_39 = ifelse((FTP_Status == '1' & AGE_BAND_NEW == '25 - 39'),1,0))

classify_data_final = mutate(classify_data_final,FTP_and_40_44 = ifelse((FTP_Status == '1' & AGE_BAND_NEW == '40 - 54'),1,0))

classify_data_final = mutate(classify_data_final,FTP_and_55_and_above = ifelse((FTP_Status == '1' & AGE_BAND_NEW == '55 and above'),1,0))

classify_data_final = mutate(classify_data_final,FTP_males = ifelse((FTP_Status == '1' & GENDER == 'M'),1,0))

classify_data_final = mutate(classify_data_final,FTP_females = ifelse((FTP_Status == '1' & GENDER == 'F'),1,0))


#females not part of the Avis Preferred
classify_data_final = mutate(classify_data_final,Females_Not_Preferred = ifelse((AvisPreferred == '0' & GENDER == 'F'),1,0))

#males not part of the Avis Preferred
classify_data_final = mutate(classify_data_final,Males_Not_Preferred = ifelse((AvisPreferred == '0' & GENDER == 'M'),1,0))


#Males with LOR BAND 3 - 6
classify_data_final = mutate(classify_data_final,Males_LOR_3_to_6 = ifelse((LOR_BAND_NEW == '3 - 6' & GENDER == 'M'),1,0))

#Males with LOR BAND Below 3
classify_data_final = mutate(classify_data_final,Males_LOR_Below_3 = ifelse((LOR_BAND_NEW == 'Below 3' & GENDER == 'M'),1,0))

#Males with LOR BAND 6 and above
classify_data_final = mutate(classify_data_final,Males_LOR_6_and_above = ifelse((LOR_BAND_NEW == '6 and above' & GENDER == 'M'),1,0))


#Females with LOR BAND 3 - 6
classify_data_final = mutate(classify_data_final,Females_LOR_3_to_6 = ifelse((LOR_BAND_NEW == '3 - 6' & GENDER == 'F'),1,0))

#Females with LOR BAND Below 3
classify_data_final = mutate(classify_data_final,Females_LOR_Below_3 = ifelse((LOR_BAND_NEW == 'Below 3' & GENDER == 'F'),1,0))

#Females with LOR BAND 6 and above
classify_data_final = mutate(classify_data_final,Females_LOR_6_and_above = ifelse((LOR_BAND_NEW == '6 and above' & GENDER == 'F'),1,0))


#25-39 with LOR BAND 3 - 6
classify_data_final = mutate(classify_data_final,LOR3to6_25to39 = ifelse((LOR_BAND_NEW == '3 - 6' & AGE_BAND_NEW == '25 - 39'),1,0))

#25 - 39 with LOR BAND below 3
classify_data_final = mutate(classify_data_final,LORBelow3_25to39 = ifelse((LOR_BAND_NEW == 'Below 3' & AGE_BAND_NEW == '25 - 39'),1,0))
#25 - 39 with LOR BAND 6 and above
classify_data_final = mutate(classify_data_final,LOR6andabove_25to39 = ifelse((LOR_BAND_NEW == '6 and above' & AGE_BAND_NEW == '25 - 39'),1,0))

#40 - 54 with LOR BAND 3 - 6
classify_data_final = mutate(classify_data_final,LOR3to6_40to54 = ifelse((LOR_BAND_NEW == '3 - 6' & AGE_BAND_NEW == '40 - 54'),1,0))

#40 - 54 with LOR BAND Below 3
classify_data_final = mutate(classify_data_final,LORBelow3_40to54 = ifelse((LOR_BAND_NEW == 'Below 3' & AGE_BAND_NEW == '40 - 54'),1,0))

#40 - 54 with LOR BAND 6 and abpve
classify_data_final = mutate(classify_data_final,LOR6andabove_40to54 = ifelse((LOR_BAND_NEW == '6 and above ' & AGE_BAND_NEW == '40 - 54'),1,0))


#55 and above with LOR BAND 3 - 6
classify_data_final = mutate(classify_data_final,LOR3to6_55andabove = ifelse((LOR_BAND_NEW == '3 - 6' & AGE_BAND_NEW == '55 and above'),1,0))

#55 and above with LOR BAND Below 3
classify_data_final = mutate(classify_data_final,LORBelow3_55andabove = ifelse((LOR_BAND_NEW == 'Below 3' & AGE_BAND_NEW == '55 and above'),1,0))

#55 and above with LOR BAND 6 and abpve
classify_data_final = mutate(classify_data_final,LOR6andabove_55andabove = ifelse((LOR_BAND_NEW == '6 and above ' & AGE_BAND_NEW == '55 and above'),1,0))




#Males that havent rented in the last 12 months
classify_data_final = mutate(classify_data_final,Males_Not_Rent = ifelse((RentedinLast12Mths == '0' & GENDER == 'M'),1,0))

#Females that havent rented in the last 12 months
classify_data_final = mutate(classify_data_final,Females_Not_Rent = ifelse((RentedinLast12Mths == '0' & GENDER == 'F'),1,0))

#Males that are repeat renters
classify_data_final = mutate(classify_data_final,Male_Repeat_Renters = ifelse((TravellerType == 'Repeat Renters' & GENDER == 'M'),1,0))

#Males that are sporadic travellers
classify_data_final = mutate(classify_data_final,Male_Sporadic_Travellers = ifelse((TravellerType == 'Sporadic Travellers' & GENDER == 'M'),1,0))


#Females that are repeat renters
classify_data_final = mutate(classify_data_final,Female_Repeat_Renters = ifelse((TravellerType == 'Repeat Renters' & GENDER == 'F'),1,0))

#Females that are sporadic travellers
classify_data_final = mutate(classify_data_final,Female_Sporadic_Travellers = ifelse((TravellerType == 'Sporadic Travellers' & GENDER == 'F'),1,0))


#Males who have ordered a package flag
classify_data_final = mutate(classify_data_final,Male_PackageFlag = ifelse((PACKAGE_FLAG == '1' & GENDER == 'M'),1,0))

#Females who have ordered a package flag
classify_data_final = mutate(classify_data_final,Female_PackageFlag = ifelse((PACKAGE_FLAG == '1' & GENDER == 'F'),1,0))


#25 - 39 who are repeat renters
#40 -54 who are repeat renters
#55 and above who are repeat renters


#25 - 39 sporadic travellers
#40 - 54 sporadic travellers
# 55 and above sporadic travellers.

classify_data_final['AGE_BAND_NEW'] = lapply(classify_data_final['AGE_BAND_NEW'],as.factor)
classify_data_final['FTP_and_25_39'] = lapply(classify_data_final['FTP_and_25_39'],as.factor)
classify_data_final['FTP_and_40_44'] = lapply(classify_data_final['FTP_and_40_44'],as.factor)
classify_data_final['FTP_and_55_and_above'] = lapply(classify_data_final['FTP_and_55_and_above'],as.factor)
classify_data_final['FTP_males'] = lapply(classify_data_final['FTP_males'],as.factor)
classify_data_final['FTP_females'] = lapply(classify_data_final['FTP_females'],as.factor)
classify_data_final['Females_Not_Preferred'] = lapply(classify_data_final['Females_Not_Preferred'],as.factor)
classify_data_final['Males_Not_Preferred'] = lapply(classify_data_final['Males_Not_Preferred'],as.factor)
classify_data_final['Males_LOR_3_to_6'] = lapply(classify_data_final['Males_LOR_3_to_6'],as.factor)
classify_data_final['Males_LOR_6_and_above'] = lapply(classify_data_final['Males_LOR_6_and_above'],as.factor)
classify_data_final['Females_LOR_3_to_6'] = lapply(classify_data_final['Females_LOR_3_to_6'],as.factor)
classify_data_final['Females_LOR_6_and_above'] = lapply(classify_data_final['Females_LOR_6_and_above'],as.factor)
classify_data_final['Males_LOR_Below_3'] = lapply(classify_data_final['Males_LOR_Below_3'],as.factor)
classify_data_final['Females_LOR_Below_3'] = lapply(classify_data_final['Females_LOR_Below_3'],as.factor)
classify_data_final['LOR3to6_25to39'] = lapply(classify_data_final['LOR3to6_25to39'],as.factor)
classify_data_final['LORBelow3_25to39'] = lapply(classify_data_final['LORBelow3_25to39'],as.factor)
classify_data_final['LOR6andabove_25to39'] = lapply(classify_data_final['LOR6andabove_25to39'],as.factor)
classify_data_final['LOR3to6_40to54'] = lapply(classify_data_final['LOR3to6_40to54'],as.factor)
classify_data_final['LORBelow3_40to54'] = lapply(classify_data_final['LORBelow3_40to54'],as.factor)
classify_data_final['LOR6andabove_40to54'] = lapply(classify_data_final['LOR6andabove_40to54'],as.factor)
classify_data_final['LOR3to6_55andabove'] = lapply(classify_data_final['LOR3to6_55andabove'],as.factor)
classify_data_final['LORBelow3_55andabove'] = lapply(classify_data_final['LORBelow3_55andabove'],as.factor)
classify_data_final['LOR6andabove_55andabove'] = lapply(classify_data_final['LOR6andabove_55andabove'],as.factor)
classify_data_final['Males_Not_Rent'] = lapply(classify_data_final['Males_Not_Rent'],as.factor)
classify_data_final['Females_Not_Rent'] = lapply(classify_data_final['Females_Not_Rent'],as.factor)
classify_data_final['Male_Repeat_Renters'] = lapply(classify_data_final['Male_Repeat_Renters'],as.factor)
classify_data_final['Male_Sporadic_Travellers'] = lapply(classify_data_final['Male_Sporadic_Travellers'],as.factor)
classify_data_final['Female_Repeat_Renters'] = lapply(classify_data_final['Female_Repeat_Renters'],as.factor)
classify_data_final['Female_Sporadic_Travellers'] = lapply(classify_data_final['Female_Sporadic_Travellers'],as.factor)
classify_data_final['Male_PackageFlag'] = lapply(classify_data_final['Male_PackageFlag'],as.factor)
classify_data_final['Female_PackageFlag'] = lapply(classify_data_final['Female_PackageFlag'],as.factor)
classify_data_final['GENDER'] = lapply(classify_data_final['GENDER'],as.factor)
classify_data_final = classify_data_final[,c(-1,-21)]





set.seed(10)
seg.rf.features =  randomForest(cluster ~ .,data = classify_data_final,importance = TRUE,ntree = 3000,mtry = 2)


#randomForest(formula = cluster ~ ., data = classify_data_final,      importance = TRUE, ntree = 3000, mtry = 2) 
#Type of random forest: classification
#Number of trees: 3000
#No. of variables tried at each split: 2

#OOB estimate of  error rate: 51.81%
#Confusion matrix:
#  1     2     3 class.error
#1 5015  7633 15172   0.8197340
#2 1669 21210 16852   0.4661599
#3 2379 15444 28784   0.3824104





new_DF <- classify_data_final[rowSums(is.nan(classify_data_final$GENDER)) > 0,]

set.seed(100)
bestmtry <- tuneRF(classify_data_final[,-9], classify_data_final$cluster, stepFactor=1.5, improve=1e-5, ntree=3000)
print(bestmtry)


dummy = dummyVars("~AGE_BAND_NEW",data = classify_data_final)
age <- data.frame(predict(dummy, newdata = classify_data_final))
classify_data_final["25 - 39"] = age['AGE_BAND_NEW25...39']
classify_data_final["40 - 54"] = age['AGE_BAND_NEW40...54']
classify_data_final["55+"] = age['AGE_BAND_NEW55.and.above']

dummy = dummyVars("~LOR_BAND_NEW",data = classify_data_final)
lor <- data.frame(predict(dummy, newdata = classify_data_final))
classify_data_final["LOR_3_to_6"] = lor['LOR_BAND_NEW3...6']
classify_data_final["LOR_6_and_above"] = lor['LOR_BAND_NEW6.and.above']
classify_data_final["LOR_Below_3"] = lor['LOR_BAND_NEWBelow.3']


dummy = dummyVars("~TravellerType",data = classify_data_final)
traveller <- data.frame(predict(dummy, newdata = classify_data_final))
classify_data_final["RepeatRenters"] = traveller['TravellerTypeRepeat.Renters']
classify_data_final["SporadicTravellers"] = traveller['TravellerTypeSporadic.Travellers']


dummy = dummyVars("~GENDER",data = classify_data_final)
gender <- data.frame(predict(dummy, newdata = classify_data_final))
classify_data_final["Female"] = gender['GENDERF']
classify_data_final["Male"] = gender['GENDERM']



classify_data_final = classify_data_final[,c(-1,-3,-8,-21,-22,-23)]
classify_data_final['clusterClass'] = classify_data_final$cluster -1


classify_data_final = classify_data_final[,c(-7)]
library(xgboost)

set.seed(100)
classifySampled = classify_data_final %>% group_by(.,clusterClass) %>% sample_n(27820)

  
train_data   <- as.matrix(classify_data_final[,-57])
train_label  <- classify_data_final$clusterClass
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)


numberOfClasses <- length(unique(classify_data_final$clusterClass))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses, eta = 0.3)
nround    <- 1000 # number of XGBoost rounds

bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = 1000)
bst_model
test_pred <- predict(bst_model, newdata = train_matrix)
test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                          ncol=length(test_pred)/numberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(label = train_label + 1,
         max_prob = max.col(., "last"))
confusionMatrix(factor(test_prediction$label),
                factor(test_prediction$max_prob),
                mode = "everything")
names = colnames(classifySampled[,-57])
importance_matrix = xgb.importance(feature_names = names,model = bst_model)
head(importance_matrix)
gp = xgb.plot.importance(importance_matrix)
print(gp) 



library(rpart)
fit <- rpart(clusterClass ~ .,
             method="class", data=classifySampled)
printcp(fit)
summary(fit)


plot(fit, uniform=TRUE, 
     main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

pred = predict(fit,classifySampled)
table(classifySampled$clusterClass,pred)
classifySampled['clusterClass'] = lapply(classifySampled['clusterClass'],as.factor)
classifySampled['clusterClass'] = lapply(classifySampled['clusterClass'],as.numeric)

library(MASS)
fit <- lm(clusterClass~.,data=classifySampled)
step <- stepAIC(fit, direction="both")
step$anova # display results



#it feels like undersampling majority class introduces more bias and reduces the accuracy of class 3. Lets try
# oversampling the minority class using SMOTE

library(DMwR)
classify_data_final$clusterClass <- factor(ifelse(classify_data_final$clusterClass== 0,"rare","common"))
table(classify_data_final$class1)
newData <- SMOTE(clusterClass ~ ., classify_data_final, perc.over = 100,perc.un 
                 ' 
 2
2der=0)


library(caret)
featurePlot(x=classify_data_final[,1:4], y=classify_data_final[,9], plot="density", scales=list(x=list(relation="free"), y=list(relation="free")), auto.key=list(columns=3))

classify_data_final2 = select(LOR_BAND_NEW,RentedinLast12Mths,FTP_LOR_6_or)

train.prop = 0.75
train.cases = sample(nrow(classify_data_final),nrow(classify_data_final) * train.prop)
seg.rf.train = classify_data_final[train.cases,]
seg.rf.test = classify_data_final[-train.cases,]

pred = predict(seg.rf.features,seg.rf.test)
table(pred,seg.rf.test$cluster)

set.seed(10)
seg.rf.features =  randomForest(cluster ~ .,data = classify_data_final,importance = TRUE,ntree = 3000,mtry = 2)



write.csv(classify_final,"classify_data_final2.csv",row.names = FALSE)
