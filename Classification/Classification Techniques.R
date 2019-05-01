rm(list=ls())

#import data
hr_data <- read.csv("E:/R/HR dataset.csv")
View(hr_data)

# removing role & salary as we using role_code, salary_code.

hr_data1 = hr_data[,-c(9,10)]

#coverting role_codes and Salary_codes into factors

hr_data1$role_code = as.factor(hr_data1$role_code)
hr_data1$salary.code = as.factor(hr_data1$salary.code)

#view the data
View(hr_data1)

summary(hr_data1)

#boxplot to check relation between the satisfaction level and wheater employee left
boxplot(hr_data1$satisfaction_level ~ hr_data1$left ,main = "Boxplot for satisfaction_level")
#boxplot to check relation between the average monthly hour spent and wheater employee left
boxplot(hr_data1$average_montly_hours ~ hr_data1$left ,main = "Boxplot for Avg Monthly hours")

#box plot to check relation between time spent with company and wheather the employee left
boxplot(hr_data1$time_spend_company ~ hr_data1$left , main = "Box plot for time spent with company")

#mosiac plot to check relation between time spent with company and wheater the employee left
mosaicplot(hr_data1$left~hr_data1$salary.code , color = 'skyblue')

#mosiac plot to check relation between promotion_last_5years and wheater the employee left
mosaicplot(hr_data1$left~hr_data1$promotion_last_5years , color = 'skyblue')

#correlation among variables
cor(hr_data1[,1:8])
corrplot(cor(hr_data1[,1:8]), method = "circle")

set.seed(1234)
splitIndex <- createDataPartition(hr_data1$left,p=.70,list = FALSE,times = 1)

trainsplit <- hr_data1[splitIndex,]
testsplit <- hr_data1[-splitIndex,]
print(table(trainsplit$left))
print(table(testsplit$left))

#check for the event rate
prop.table(table(trainsplit$left))
prop.table(table(testsplit$left))

#decision tree using rpart algorithm

fit = rpart(left ~ ., data = trainsplit,method = "class",control = rpart.control(minsplit = 30, cp =0.01))
rpart.plot(fit,cex=0.60)
summary(fit)
plotcp(fit)

prediction <- predict(fit, trainsplit,type = "class")
View(prediction)

caret::confusionMatrix(prediction,as.factor(trainsplit$left))

predictiontest <- predict(fit, testsplit,type = "class")
caret::confusionMatrix(predictiontest,as.factor(testsplit$left))


#Random Forest model.
install.packages("randomForest")
library(randomForest)

modelrf <- randomForest(as.factor(left) ~ ., data = trainsplit, do.trace=T)
modelrf

#checking variable importance in random forest.
importance(modelrf)
varImpPlot(modelrf)

predrf_tr <- predict(modelrf,trainsplit)
predrf_test <-predict(modelrf,testsplit)

confusionMatrix(predrf_tr,as.factor(trainsplit$left))
confusionMatrix(predrf_test,as.factor(testsplit$left))


#prediction and model evaluation using confusion matrix
#decision tree roc
auc1 <- roc(as.numeric(testsplit$left),as.numeric(predictiontest))
plot(auc1,col = 'blue',main=paste('AUC:',round(auc1$auc[[1]],3)))


#random forest roc
aucrf <- roc(as.numeric(testsplit$left),as.numeric(predrf_test),ci = TRUE)
plot(aucrf,ylim=c(0,1),print.thres=TRUE,main=paste('Random Forest AUC:',round(aucrf$auc[[1]],3)),col = 'blue')

#comparison of decion tree and roc
plot(aucrf,ylim=c(0,1),main=paste('ROC Comparision :RF(blue),C5.0(Black)) '),col = 'blue')
par(new = TRUE)
plot(auc1)
par(new = TRUE)



#naivebayes
modelnb <- naive_bayes(as.factor(left) ~. ,data = trainsplit)
modelnb

modelnb <- naiveBayes(as.factor(left) ~., data = trainsplit)
modelnb

prednb_tr <- predict(modelnb,trainsplit)
prednb_test <- predict(modelnb,testsplit)


#performance of naive bayes using confusion matrix.
confusionMatrix(as.factor(prednb_tr),as.factor(trainsplit$left))
confusionMatrix(as.factor(prednb_test),as.factor(testsplit$left))



#knn classification.
dummy_df = dummy.data.frame(hr_data[,c('role','salary')])
View(dummy_df)

hr_data2 = hr_data
hr_data2 = cbind.data.frame(hr_data2, dummy_df)

hr_data2 = hr_data2[,!(names(hr_data2) %in% c('role_code','salary.code'))]
hr_data2 = hr_data2[-c(9,10)]
hr_data2$Work_accident = as.numeric(hr_data$Work_accident)
hr_data2$promotion_last_5years = as.numeric(hr_data2$promotion_last_5years)

x = hr_data2[,!(names(hr_data2) %in% c('left'))]
hr_data2_scaled = as.data.frame(scale(x))

str(hr_data2_scaled)

hr_train <- hr_data2_scaled[splitIndex,]
hr_test <- hr_data2_scaled[-splitIndex,]

hr_train_labels <- hr_data2[splitIndex,'left']
hr_test_labels <- hr_data2[-splitIndex,'left']

library(class)
library(gmodels)

test_pred_1 <- knn(train = hr_train,test = hr_test, cl = hr_train_labels,k=5)
CrossTable(x=hr_test_labels, y=test_pred_1,prop.chisq = FALSE)

confusionMatrix(as.factor(test_pred_1),as.factor(hr_test_labels))

#thumb rule to decide on k for knn is sqrt(n)/2
k = sqrt(nrow(hr_train))/2
k

test_pred_rule <- knn(train = hr_train,test = hr_test,cl = hr_train_labels,k=k)
CrossTable(x=hr_test_labels,y=test_pred_rule,prop.chisq = FALSE)


#another method to determine the k for knn
set.seed(400)
ct <- trainControl(method = "repeatedcv",repeats = 3)
fit <- train(as.factor(left) ~ .,data=hr_data2,method = "knn",trControl = ct, preProcess = c("center","scale"),tuneLength = 20)
fit

#checking the accuracy with k = 7

test_pred_7 <- knn(train = hr_train,test = hr_test,cl = hr_train_labels, k=7)
CrossTable(x=hr_test_labels,y=test_pred_7,prop.chisq = FALSE)
confusionMatrix(hr_test_labels,test_pred_7)