westminster = read.csv("Data Mining Five Years Final.csv")
View(westminster)
westminster = unique(westminster[,1:43])
newdata = westminster[,c(12,13,16,17,18,19,20,21,22,23,24,36,40)]
attach(westminster)
academicPerf = ifelse((GradeANum+GradeBNum) > (GradeCNum+GradeDNum+GradeENum),1,0)
avgEffort = (Effort1Num*1+Effort2Num*2+Effort3Num*3+Effort4Num*4+Effort5Num*5)/(Effort1Num+Effort2Num+Effort3Num+Effort4Num+Effort5Num)
avgEffort = ifelse(avgEffort >=0,avgEffort,0)
newdata[,"AcademicPerformance"] = academicPerf
newdata[,"AverageEffort"] = avgEffort
View(newdata)
set.seed(2)
library(tree)
train = sample(1:nrow(newdata),nrow(newdata)*3/4)
test = -train
training_data = newdata[train,]
test_data = newdata[test,]
tree_model = tree(Retained~.,training_data)
tree_model
plot(tree_model)
text(tree_model,pretty=0)
tree_pred = predict(tree_model,test_data,type="class")
testRetained = test_data$Retained
table(tree_pred,testRetained)
summary(tree_model)


cv.tree = cv.tree(tree_model,FUN=prune.misclass)
cv.tree
plot(cv.tree)
prune.tree = prune.misclass(tree_model,best=4)
plot(prune.tree)
text(prune.tree,pretty=0)
summary(prune.tree)



#Random Forest
library(caret)
library(randomForest)
library(party)
newdata = na.omit(newdata)
train = sample(1:nrow(newdata),nrow(newdata) * 3/4)
training = newdata[train,]
test = -train
testing = newdata[test,]
predictors = training[,-12]
class = training[,12]
#treeBag = bag(predictors,class,B=10,
#             bagControl=bagControl(fit=ctreeBag$fit,predict=ctreeBag$pred,aggregate=ctreeBag$aggregate))
r2 = randomForest(Retained ~., data=training, importance=TRUE, do.trace=100)
r2
pred3 = predict(r2,testing[,-16])
confusionMatrix(testing$Retained,pred3)
importance(r2)
varImpPlot(r2)


disability = ifelse(westminster$Disability == "Y",1,0)
payontime = ifelse(westminster$PayOnTime == "Y",1,0)
scholarship = ifelse(westminster$Scholarship == "Y",1,0)
parentsOldScholars = ifelse(westminster$ParentOldScholar == "Y",1,0)
Retention = ifelse(westminster$Retained == "Y",1,0)
SibAtSchool = ifelse(westminster$SibAtSchool == "Y",1,0)
SplitFamily = ifelse(westminster$SplitFamily == "Y",1,0)
Boarder = ifelse(westminster$Boarder == "Boarder",1,0)
Sex = ifelse(westminster$Sex == "M",1,0)
Indigenous = ifelse(westminster$Indigenous == "Y",1,0)
PastSibling = ifelse(westminster$PastSibbling == "Y",1,0)
westminster[,"AcademicPerformance"] = academicPerf
westminster[,"AverageEffort"] = avgEffort
dataLatest = data.frame(disability,payontime,scholarship,parentsOldScholars,Retention,SibAtSchool,SplitFamily,Boarder,
                        Sex,Indigenous,PastSibling,westminster$AcademicPerformance,westminster$AverageEffort)

attach(dataLatest)
#Stepwise Regression
library(MASS)
fit <- lm(Retention~.,data=dataLatest)
step <- stepAIC(fit, direction="both")
step$anova # display results


#All Subsets Regression
library(leaps)
attach(dataLatest)
leaps<-regsubsets(Retention~.,data=dataLatest,nbest=10)
summary(leaps)
plot(leaps, scale="adjr2")


