library(caret)
library(kernlab)
library(ROCR)

?segmentationData

data("segmentationData")
dim(segmentationData)
head(segmentationData)

#Distribution of target variables
table(segmentationData$Class)

table(segmentationData$Class)/length(segmentationData$Class)

Index <- createDataPartition(segmentationData$Class,p =0.7,list = F)

svm.train <- segmentationData[Index , ]
svm.validate <- segmentationData[-Index,]
trainX <- svm.train[,4:61]
set.seed(123)

ctrl <- trainControl(method = "cv",number = 5,
                     summaryFunction = twoClassSummary,
                     classProbs = T)

#grid search to fine tune svm
grid <- expand.grid(sigma = c(0.01,.015,.2),
                    C=c(0.75,0.9,1,1.1,1.25))

svm.tune <- train(x = trainX ,y = svm.train$Class,method = "svmRadial",
                  metric = "ROC",
                  tuneGrid = grid,
                  trControl = ctrl)
print(svm.tune)

valX <- svm.validate[,4:61]
pred <- predict(svm.tune , valX, type = "prob")[2]

head(pred)

pred_val <- prediction(pred , svm.validate$Class)
View(pred_val)

#calculating area under the curve
perf_val <- performance(pred_val,"auc")
perf_val

#calculating true positive and false positive rate
perf_val <- performance(pred_val,"tpr","fpr")

#plot the ROC curve
plot(perf_val,col = "green",lwd = 1.5)
