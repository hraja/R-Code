#Feature Engineering and Selection

minda = read.csv("ClassificationDataset-1.csv")
names(minda)
attach(minda)
#response=ifelse(Cancellation.After.100.Days=='Yes',1,0)
#response
#minda$Cancellation.After.100.Days=response
library(caret)
mindaNew<-minda[,c(-1,-9,-10,-11,-17,-18)]
mindadummy<-dummyVars(~.,data=mindaNew)
Sdummy<-predict(mindadummy,minda)
View(Sdummy)
Sdummy<-Sdummy[,c(-1,-3,-10,-11,-17,-18,-24,-28,-30,-32,-34,-36)]
Sdummy=data.frame(Sdummy)
Sdummy['Age'] = Age
Sdummy[,"Cancellation.After.100.Days"] = Cancellation.After.100.Days
#Sdummy[,"Frequency"] = Frequency
#Sdummy[,"Recency"] = Recency
Sdummy[,"Total.Gifts"] = Total.Gifts
Sdummy[,"Monthly.Gift.Amount"] = Monthly.Gift.Amount
Sdummy[,"Gift.Status.Completed"] = NULL
Sdummy = na.omit(Sdummy)

#Stepwise Regression
library(MASS)
fit <- lm(Cancellation.After.100.Days~.,data=Sdummy)
step <- stepAIC(fit, direction="both")
step$anova # display results

#Subsets Regression
library(leaps)
attach(Sdummy)
leaps<-regsubsets(Cancellation.After.100.Days~.,data=Sdummy,nbest=10)
summary(leaps)
plot(leaps, scale="adjr2")


minda = read.csv("ClassificationDataset-1.csv")
minda = na.omit(minda)
names(minda)
attach(minda)
#response=ifelse(Cancellation.After.100.Days=='Yes',1,0)
#response
#minda$Cancellation.After.100.Days=response
library(caret)
mindaNew<-minda[,-1]
mindadummy<-dummyVars(~.,data=mindaNew)
Sdummy<-predict(mindadummy,minda)
Sdummy<-Sdummy[,c(-1,-3,-10,-15,-17,-20,-26,-28,-30,-32,-34,-36,-38,-40)]
Sdummy=data.frame(Sdummy)
Sdummy = na.omit(Sdummy)
View(Sdummy)

attach(minda)
mindaNew<-data.frame(Age,Sdummy$Confirmation.Call.Voicemail,Monthly.Gift.Amount,Sdummy$Payment.Type.Credit.Card,
                     Sdummy$Email.Address.No,Sdummy$Home.Phone.No,Cancellation.After.100.Days)
View(mindaNew)
set.seed(3)
train = sample(1:nrow(mindaNew),nrow(mindaNew)*3/4)
test = -train
training_data = mindaNew[train,]
test_data = mindaNew[test,]
logistic_model = glm(Cancellation.After.100.Days~.,data=training_data,family=binomial)
logistic_model
summary(logistic_model)
logistic_probs = predict(logistic_model,test_data,type="response")
logistic_probs
testing_y = test_data$Cancellation.After.100.Days
logistic_pred_y = rep(1, length(testing_y))
logistic_pred_y[logistic_probs < 0.5] = 0
table(logistic_pred_y, testing_y)
mean(testing_y!=logistic_pred_y)
# Misclassification Rate: 0.12
library(boot)
cv.err = cv.glm(mindaNew,logistic_model,K=10)
cv.err
exp(-0.4808+-1.7348 *Total.Gifts)/(1+exp(-0.4808+-1.7348 *Total.Gifts))
test_data["Probabilities"] = logistic_probs
write.table(test_data,"testdata2.csv",row.names=FALSE)



minda = read.csv("Cancellation Prediction.csv")
names(minda)
attach(minda)
library(caret)
mindaNew<-minda[,c(-1,-9,-10,-16,-17,-18)]
mindadummy<-dummyVars(~.,data=mindaNew)
Sdummy<-predict(mindadummy,minda)
View(Sdummy)
Sdummy<-Sdummy[,c(-1,-3,-10,-15,-17,-20,-26,-28,-30,-32,-34,-36,-38)]
Sdummy=data.frame(Sdummy)
View(Sdummy)
Sdummy[,"Frequency"] = Frequency
#Sdummy[,"Recency"] = Recency
Sdummy[,"Total.Gifts"] = Total.Gifts
Sdummy[,"Monthly.Gift.Amount"] = Monthly.Gift.Amount
Sdummy[,"Cancellation.Status"] = Cancellation.Status
View(Sdummy)


#Stepwise Regression
library(MASS)
fit <- lm(Cancellation.Status~.,data=Sdummy)
step <- stepAIC(fit, direction="both")
step$anova # display results

#Subsets Regression
library(leaps)
attach(Sdummy)
leaps<-regsubsets(Cancellation.Status~.,data=Sdummy,nbest=10)
summary(leaps)
plot(leaps, scale="adjr2")


#Cancellation.Status ~ Gender.Male + Region.North + Sign.Up.Location.Type.D2D + 
#Recency + Total.Gifts + Monthly.Gift.Amount
#Gender.Male, Region.North, Sign.Up.Location.Type.D2D, Frequency, Recency, Total.Gifts, Monthly.Gift.Amount


mindaNew = data.frame(Gender.Male,Region.East,Region.Interstate,Region.North,Sign.Up.Location.Type.D2D,Employment.Status.Other,
                      Frequency,Confirmation.Call.Voicemail,Deceased.No,Update.Preference.Email,Update.Preference.Post,
                      Update.Preference.SMS,Update.Preference.SMS.Email,Update.Preference.SMS.Email.Post,Update.Preference.SMS.Post,
                      Home.Phone.No,Mobile.Number.No,Monthly.Gift.Amount,
                      Email.Address.No,Payment.Type.Credit.Card,Total.Gifts,Cancellation.Status)
mindaNew = na.omit(mindaNew)
View(mindaNew)
set.seed(2)
training = sample(1:nrow(mindaNew),nrow(mindaNew)*3/4)
test = -training
training_data = mindaNew[training,]
test_data = mindaNew[test,]
logistic_model = glm(Cancellation.Status~.,data=training_data,family=binomial)
logistic_model
summary(logistic_model)
logistic_probs = predict(logistic_model,test_data,type="response")
logistic_probs
testing_y = test_data$Cancellation.Status
logistic_pred_y = rep(1, length(testing_y))
logistic_pred_y[logistic_probs < 0.5] = 0
table(logistic_pred_y, testing_y)
mean(testing_y!=logistic_pred_y)
#Misslcassification Rate: 22.0%

#LDA
lda_model = lda(Cancellation.Status~.,data=training_data)
summary(lda_model)
lda_predict = predict(lda_model,test_data)
names(lda_predict)
lda_predict$posterior
lda_Cancellation = lda_predict$class
testing_cancellation = test_data$Cancellation.Status
table(lda_Cancellation,testing_cancellation)
mean(lda_Cancellation != testing_cancellation)
#Misslassification: 22%

#QDA
qda_model = qda(Cancellation.Status~.,data=training_data)
summary(qda_model)
qda_predict = predict(qda_model,test_data)
qda_Cancellation = qda_predict$class
testing_cancellation = test_data$Cancellation.Status
table(qda_Cancellation,testing_cancellation)
mean(qda_Cancellation != testing_cancellation)

#Tree model

library(tree)
minda = read.csv("MindaClass4Freq.csv")
attach(minda)
set.seed(2)
train = sample(1:nrow(minda),nrow(minda)*3/4)
test = -train
training_data = minda[train,]
training_data= training_data[,c(-9,-10,-18)]
test_data = minda[test,c(-9,-10,-18)]
tree_model = tree(Cancellation.After.100.Days~.-Recency,data=training_data)
#printcp(tree_model)
#plotcp(tree_model)
summary(tree_model)
plot(tree_model)
text(tree_model,pretty=0)
tree.pred = predict(tree_model,test_data,type="class")
testCancellation = minda[test,c(-9,-10,-18)]$Cancellation.After.100.Days
table(tree.pred,testCancellation)
cv.minda = cv.tree(tree_model,FUN=prune.misclass)
cv.minda
plot(cv.minda)
prune.minda = prune.misclass(tree_model,best=4)
plot(prune.minda)
text(prune.minda,all=TRUE,cex=0.5,pretty=0)
tree.pred = predict(prune.minda,test_data,type="class")
testCancellation = minda[test,c(-9,-10,-18)]$Cancellation.After.100.Days
table(tree.pred,testCancellation)





