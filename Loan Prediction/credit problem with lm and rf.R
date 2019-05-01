
rm(list = ls())
set.seed(123)
data <- read.csv("E:/R/competion case study/loan/train_u6lujuX_CVtuZ9i.csv",na.strings = c(""," ","NA"))
View(data)

test_1 <- read.csv("E:/R/competion case study/loan/test_Y3wMUE5_7gLdaTN.csv",na.strings = c(""," ","NA"))
sum(is.na(test))
summary(test_1)
str(data)

summary(data)

data <- data[-c(1)]

data$Gender <- ifelse(data$Gender == "Female",1,0)
data$Gender[(is.na(data$Gender))] = 0

data$Married <- ifelse(data$Married == "Yes",1,0)
data$Married[(is.na(data$Married))] = 0

data$Dependents[(is.na(data$Dependents))] = 0
dum_dependents <- dummy_cols(data$Dependents,remove_first_dummy = T)
data <- data.frame(c(data,dum_dependents))
View(data)

data$Self_Employed <- ifelse(data$Self_Employed == "Yes",1,0)
data$Self_Employed[(is.na(data$Self_Employed))] = 0
data$Education <- ifelse(data$Education =="Graduate",1,0)
summary(data)

dum_property <- dummy_cols(data$Property_Area,remove_first_dummy = T)
data <- data.frame(c(data,dum_property))
summary(data)
View(data)
data <- data[-c(3,11,13,17)]
summary(data)

data$Loan_Status <- ifelse(data$Loan_Status == "Y",1,0)
summary(data)

data$Loan_Amount_Term[(is.na(data$Loan_Amount_Term))] = 360
data$LoanAmount[(is.na(data$LoanAmount))] = 128

data$Credit_History[(is.na(data$Credit_History))] = 1

summary(data)

bx <- boxplot(data$ApplicantIncome)
quantile(data$ApplicantIncome , seq(0,1,0.02))
bx$stats
data$ApplicantIncome <- ifelse(data$ApplicantIncome > 10139,10139,data$ApplicantIncome)

cx <- boxplot(data$CoapplicantIncome)
cx$stats
quantile(data$CoapplicantIncome,seq(0,1,0.02))

data$CoapplicantIncome <- ifelse(data$CoapplicantIncome > 5701.0 ,5701.0,data$CoapplicantIncome)

dx <- boxplot(data$Loan_Amount_Term,data$LoanAmount)
dx$stats
quantile(data$LoanAmount,seq(0,1,0.02))

data$LoanAmount <- ifelse(data$LoanAmount > 275.00,275.00,data$LoanAmount)
quantile(data$Loan_Amount_Term,seq(0,1,0.02))

data$Loan_Amount_Term <- ifelse(data$Loan_Amount_Term > 360,360,data$Loan_Amount_Term )
data$Loan_Amount_Term <- ifelse(data$Loan_Amount_Term < 180,180,data$Loan_Amount_Term )
summary(data)

View(test)
test<- test_1[-c(1)]

test$Gender <- ifelse(test$Gender == "Female",1,0)
test$Gender[(is.na(test$Gender))] = 0

test$Married <- ifelse(test$Married == "Yes",1,0)
test$Married[(is.na(test$Married))] = 0

test$Dependents[(is.na(test$Dependents))] = 0
dum_dependents <- dummy_cols(test$Dependents,remove_first_dummy = T)
test <- data.frame(c(test,dum_dependents))
View(test)

test$Self_Employed <- ifelse(test$Self_Employed == "Yes",1,0)
test$Self_Employed[(is.na(test$Self_Employed))] = 0
test$Education <- ifelse(test$Education =="Graduate",1,0)
summary(test)

dum_property <- dummy_cols(test$Property_Area,remove_first_dummy = T)
test <- data.frame(c(test,dum_property))
summary(test)
View(test)
test <- test[-c(3,11,12,16)]
summary(test)


test$Loan_Amount_Term[(is.na(test$Loan_Amount_Term))] = 360
test$LoanAmount[(is.na(test$LoanAmount))] = 128

test$Credit_History[(is.na(test$Credit_History))] = 1

summary(test)



model <- glm(Loan_Status ~. ,data = data)
summary(model)

vif(model)
step(model)

model2 <-  glm(formula = Loan_Status ~ Married + Education + LoanAmount + 
                 Credit_History + .data_1 + .data_Semiurban, data = data)
summary(model2)

model3 <-  glm(formula = Loan_Status ~ Married + Education + LoanAmount + 
                 Credit_History  + .data_Semiurban, data = data)
summary(model3)

data$predict_lm <- predict(model3,data)
data$predict_lm <- ifelse(data$predict > 0.60 ,"Y","N")
confusionMatrix(as.factor(data$Loan_Status),as.factor(data$predict_lm))

test$predict <- predict(model3,test)
test$predict <- ifelse(test$predict > 0.60,"Y","N")
test_1$Loan_Status <- test$predict


model_rf <- randomForest(Loan_Status ~., data = data , do.trace=T)
model_rf
importance(model_rf)
varImpPlot(model_rf)

data$predict <- predict(model_rf,data)

data$predict <- ifelse(data$predict > 0.60 ,"Y","N")
confusionMatrix(as.factor(data$Loan_Status),as.factor(data$predict))
test$preict <- predict(model_rf,test)
test$predict <- ifelse(test$preict > 0.60,"Y","N")
View(test)

test_1$Loan_Status <-ifelse(test$preict > 0.60,"Y","N")

View(test_1)

submission_lm1 = data.frame(test_1[c("Loan_ID","Loan_Status")])
write.csv(submission_lm1,"E:/R/competion case study/loan/submission_lm.csv",row.names = F)

submission2 = data.frame(test_1[c("Loan_ID","Loan_Status")])
write.csv(submission2,"E:/R/competion case study/loan/submission2.csv",row.names = F)

