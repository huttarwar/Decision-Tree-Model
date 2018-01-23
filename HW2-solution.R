#Heramb Vijay Uttarwar
#A20398330
#CS-422 HW-2


library(rpart)
library(caret)
library(rpart.plot)
library(ROCR)
setwd("C:/Program Files/RStudio/Data FIles/")
rm(list=ls())

ilpd=read.csv("ILPD.csv", header = T, sep=",")

set.seed(100)
# Splitting into 60-40 (train-test).
index_ilpd<- sample(1:nrow(ilpd), size = 0.6*nrow(ilpd))
train_ilpd<- ilpd[index_ilpd, ]
test_ilpd <- ilpd[-index_ilpd, ]
# There are 234 and 349 instances to test and train the data

#a.For the training dataset, produce a correlation scatterplot of the variables.

library(psych)
pairs.panels(ilpd, pch=19)

#i)Strongest correlated pair :- db(Direct Bilirubin) and tb(Total Bilirubin)
#ii)Weakest correlated pair :-  tp(Total Proteins) and db(Direct Bilirubin), sex and ag(Ratio of Albumin to Globulin),sgpaa(Sgpt Alamine Aminotransferase) and ag(Ratio of Albumin to Globulin)
#iii)Most negatively correlated :- age and alb(Albumin)
#iv)Variables appear to follow a Gaussian distribution :- age, tp(Total Proteins), alb(Albumin), ag(Ratio of Albumin to Globulin)

#b)Yes, I think normalising or scaling the attributes will help the classification task. Because, normalising the attributes will result in more linear relationship. It will also help in providing a robust correlation. There is no point in normalising the data which is linear. 
  #Attributes with non-linear range of values that should be normalised are:- Age, tp(Total Proteins), alb(Albumin), ag(Ratio of Albumin to Globulin)


#c)
model <- rpart(label~ ., method = "class", data =train_ilpd)
rpart.plot(model)
pred <- predict(model, test_ilpd, type = "class")
confusionMatrix(pred, test_ilpd[,11], positive = "1")
#Accuracy: 68.8%
#TPR(Sensitivity) : 0.8683          
#TNR(Specificity) : 0.2388          
#PPV(Pos Pred Value) : 0.7398  

#d)
#-Prune
plotcp(model)
printcp(model)

model.pruned <- prune(model, cp = 0.021)
pred.pruned <- predict(model.pruned, test_ilpd, type = "class")
confusionMatrix(pred.pruned, test_ilpd[, 11], positive = "1")

rpart.plot(model.pruned)
#Accuracy of the model is 68.8% and by changing the values of cp I got a better accuracy of 69.66%.
#I think that with better accuracy after pruning it reduces the size of learning tree. Thus, because of reduced cost of complexity it increases the accuracy.

#e)Build a model
newmodel<- rpart(label ~ alb+ag+aap, method = "class", data = train_ilpd)
rpart.plot(newmodel)
newpred <- predict(newmodel,test_ilpd,type = "class")
confusionMatrix(newpred,test_ilpd[,11], positive = "1")

summary(ilpd)
sd(ilpd$age)
sd(ilpd$sex)
sd(ilpd$tb)
sd(ilpd$db)
sd(ilpd$aap)
sd(ilpd$sgpaa)
sd(ilpd$sgoaa)
sd(ilpd$tp)
sd(ilpd$alb)
sd(ilpd$ag)

# Accuracy :- 72.65%(Increased by 3.85%)
#TPR(Sensitivity) : 0.9102(Increased by 4.19%)
#TNR(Specificity) : 0.2687(Increased by 2.99%)         
#PPV(Pos Pred Value) : 0.7562(Increased by 1.64%)  

#f)
#(i) a ROC curve using the ROCR package.
#ROC for model
pred.roc <- predict(model,newdata=test_ilpd,type="prob")[,2]
f.pred <- prediction(pred.roc,test_ilpd$label)
f.perf <- performance(f.pred, "tpr", "fpr")
plot(f.perf, colorize=T, lwd=3, main = "ROC")
abline(0,1)
auc<-performance(f.pred,measure = "auc")
auc@y.values[[1]]

#ROC for newmodel
pred.roc1 <- predict(newmodel,newdata=test_ilpd,type="prob")[,2]
f.pred1 <- prediction(pred.roc1,test_ilpd$label)
f.perf1 <- performance(f.pred1, "tpr", "fpr")
plot(f.perf1, colorize=T, lwd=3, main = "ROC")
abline(0,1)
auc<-performance(f.pred1,measure = "auc")
auc@y.values[[1]]

#ii) AUC for model: 66.75%
#   AUC for newmodel: 64.55%

#iii) Previous Model is better because the auc is closest to 1 as compared to newmodel.



