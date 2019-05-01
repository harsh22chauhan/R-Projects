rm(list=ls())
train <-  read.csv("E:/R/it risk/NetworkIntrusionTrainData.csv")
test <- read.csv("E:/R/it risk/NetworkIntrusionTestData.csv")
validate <- read.csv("E:/R/it risk/NetworkIntrusionValidateData.csv")
summary(test)
head(train)

summary(train)

colSums(is.na(train))

summary(train$service)
summary(train$flag)

train$service <- NULL
test$service <- NULL


View(train)


train$num_outbound_cmds <- NULL
test$num_outbound_cmds <- NULL

train$is_host_login <- NULL
test$is_host_login <- NULL


dummy_prot <- dummy_cols(train$protocol_type,remove_first_dummy =  T)
dummy_prott <- dummy_cols(test$protocol_type,remove_first_dummy =  T)


train <- data.frame(train,dummy_prot)
test <- data.frame(test,dummy_prott)

View(train)

train$protocol_type <- NULL
train$.data <- NULL

test$protocol_type <- NULL
test$.data <- NULL

dummy_flag <- dummy_cols(train$flag,remove_most_frequent_dummy  = T)
dummy_flag_t <- dummy_cols(test$flag,remove_most_frequent_dummy  = T)

train <- data.frame(train,dummy_flag)
test <- data.frame(test,dummy_flag_t)

train$flag <- NULL
train$.data <- NULL

test$flag <- NULL
test$.data <- NULL

summary(train)

train$class <- ifelse(train$class == "normal",1,0)
View(train)

model <- glm(class~., data=train)
summary(model)

ctrl <- trainControl(method = "cv",number = 5,
                     summaryFunction = twoClassSummary,
                     )

levels(train$class) <- make.names(levels(factor(train$class)))
PCFit <- train(as.factor(class) ~.,
               data = train, 
               method ="rf",
               trControl = ctrl)


model1 <- randomForest( as.factor(class) ~., data= train, do.trace = T)
summary(model1)

importance(model1)
varImpPlot(model1)

predict_tr <- predict(model1 , train)

confusionMatrix(as.factor(predict_tr), as.factor(train$class))
summary(predict_tr)
View(predict_tr)

predict_te <- predict(model1, test)
predict_te
View(test)

predict_te <- ifelse(predict_te == 1 , "normal","anomaly")
predict_te
test_1 <- read.csv("E:/R/it risk/NetworkIntrusionTestData.csv")
test_1 <- data.frame(test_1 , predict_te)
View(test_1)

View(validate)

validate$class <- ifelse( validate$class == "normal",1,0)

confusionMatrix(as.factor(predict_te) , as.factor(validate$class))
