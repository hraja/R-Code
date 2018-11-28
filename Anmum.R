library('dplyr')
library('tidyr')
setwd('/Users/hraja/Documents/DataProjects/Anmum Malaysia')

conversions = read.csv('Conversions-Modified.csv',header = TRUE,sep=',',stringsAsFactors = FALSE)
nonconversions = read.csv('NonConversions-Modified.csv',header = TRUE,sep=',',stringsAsFactors = FALSE)


conversions = mutate(conversions,Source_Anlene_Num = ifelse(Source_Anlene == "Yes",1,0))
conversions = mutate(conversions,Source_CallCenter_Num = ifelse(Source_CallCenter == "Yes",1,0))
conversions = mutate(conversions,Source_Followup_Num = ifelse(Source_Followup == "Yes",1,0))
conversions = mutate(conversions,Source_HCP_Num = ifelse(Source_HCP == "Yes",1,0))

conversions = mutate(conversions,Essential_Num = ifelse(Essential > 0,1,0))
conversions = mutate(conversions,Honey_Num = ifelse(Honey > 0,1,0))
conversions = mutate(conversions,Lacta_Num = ifelse(Lacta > 0,1,0))
conversions = mutate(conversions,Materna_Num = ifelse(Materna > 0,1,0))
conversions = mutate(conversions,Plain_Num = ifelse(Plain > 0,1,0))
conversions = mutate(conversions,PlainChoc_Num = ifelse(Plain.and.Choc > 0,1,0))


conversions_final = select(conversions,Contact.ID,Source_Anlene_Num,Source_CallCenter_Num,Source_Followup_Num,Source_HCP_Num,Essential_Num,Honey_Num,Lacta_Num,Materna_Num,Plain_Num,PlainChoc_Num)
conversions_final = mutate(conversions_final,CMCAccount="Yes")

nonconversions = mutate(nonconversions,Essential_Num = ifelse(Essential > 0,1,0))
nonconversions = mutate(nonconversions,Honey_Num = ifelse(Honey > 0,1,0))
nonconversions = mutate(nonconversions,Lacta_Num = ifelse(Lacta > 0,1,0))
nonconversions = mutate(nonconversions,Materna_Num = ifelse(Materna > 0,1,0))
nonconversions = mutate(nonconversions,Plain_Num = ifelse(Plain > 0,1,0))
nonconversions = mutate(nonconversions,PlainChoc_Num = ifelse(Plain...Choc > 0,1,0))

nonconversions = mutate(nonconversions,Source_Anlene_Num = ifelse(Source_Anlene > 0,1,0) )
nonconversions = mutate(nonconversions,Source_CallCenter_Num = ifelse(Source_CallCenter > 0,1,0) )
nonconversions = mutate(nonconversions,Source_Followup_Num = ifelse(Source_Sample > 0,1,0))
nonconversions = mutate(nonconversions,Source_HCP_Num = ifelse(Source_HCP > 0,1,0) )

nonconversions_final = dplyr::select(nonconversions,Contact.ID,Source_Anlene_Num,Source_CallCenter_Num,Source_Followup_Num,Source_HCP_Num,Essential_Num,Honey_Num,Lacta_Num,Materna_Num,Plain_Num,PlainChoc_Num)
nonconversions_final = mutate(nonconversions_final,CMCAccount="No")

nonconversions_sample = sample_n(nonconversions_final,248)

finalDataset  = rbind(conversions_final,nonconversions_sample)
finalDataset = finalDataset[,-1]
finalDataset = mutate(finalDataset,CMCAccountNum = ifelse(CMCAccount=="Yes",1,0))
finalDataset = finalDataset[,-11]
library(randomForest)
library(rpart)
set.seed(100)
train.prop = 0.65
train.cases = sample(nrow(finalDataset),nrow(finalDataset) * train.prop)
seg.df.train = finalDataset[train.cases,]
seg.df.test  = finalDataset[-train.cases,]

set.seed(1000)
seg.rf = randomForest(CMCAccountNum~.,data = finalDataset,ntree=1000)

mean(seg.df.test$CMCAccountNum==seg.rf)


fit = rpart(CMCAccountNum ~ .,method="anova",data=finalDataset)
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits


plot(fit, uniform=TRUE, 
     main="Regression Tree for Mileage ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)



#linear regression
model = lm(formula = CMCAccountNum ~ ., data = finalDataset[,c(-8,-10)])


#stepwise regression
library(MASS)
fit <- lm(CMCAccountNum ~ .,data=finalDataset[,c(-8,-10)])
step <- stepAIC(fit, direction="both")
step$anova # display results



library(leaps)
attach(seg.df.train)
leaps<-regsubsets(CMCAccountNum~.,data=finalDataset,nbest=10)
# view results 
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps,scale="r2")
library(car)
subsets(leaps, statistic="rsq")


alldata = rbind(conversions_final,nonconversions_final)
write.csv(alldata,"alldata.csv",row.names = FALSE)
