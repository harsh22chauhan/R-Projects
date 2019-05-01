rm(list=ls())
train <- read.csv("E:/R/competion case study/train_LZdllcl.csv",na.strings=c(""," ","NA"))
test <- read.csv("E:/R/competion case study/test_2umaH9m.csv")

str(train)
summary(train)
View(train)

# after analysing the summary we get to know that previous_year_rating contains NA values.
train$previous_year_rating[(is.na(train$previous_year_rating))] = 3
summary(train$previous_year_rating)

train$dept_analy <- as.numeric(train$department == "Analytics")
train$dept_fin <- as.numeric(train$department == "Finance")
train$dept_hr <- as.numeric(train$department == "HR")
train$dept_leg <- as.numeric(train$department == "Legal")
train$dept_oper <- as.numeric(train$department == "Operations")
train$dept_proc <- as.numeric(train$department == "Procurement")
train$dept_sale <- as.numeric(train$department == "Sales & Marketing")
train$dept_tech <- as.numeric(train$department == "Technology")


#dummy_department <- dummy_cols(train$department,remove_first_dummy = T)
#data <- data.frame(c(train,dummy_department))



summary(train)
View(train)

train$gender_m <-  as.numeric(train$gender == "m")

train$recr_re <- as.numeric(train$recruitment_channel == "referred")
train$recr_sou <- as.numeric(train$recruitment_channel == "sourcing")

train$region_1 <- as.numeric(train$region == "region_1")
train$region_2 <- as.numeric(train$region == "region_2")
train$region_3 <- as.numeric(train$region == "region_3")
train$region_4 <- as.numeric(train$region == "region_4")
train$region_5 <- as.numeric(train$region == "region_5")
train$region_6 <- as.numeric(train$region == "region_6")
train$region_7 <- as.numeric(train$region == "region_7")
train$region_8 <- as.numeric(train$region == "region_8")
train$region_9 <- as.numeric(train$region == "region_9")
train$region_10 <- as.numeric(train$region == "region_10")
train$region_11 <- as.numeric(train$region == "region_11")
train$region_12 <- as.numeric(train$region == "region_12")
train$region_13 <- as.numeric(train$region == "region_13")
train$region_14 <- as.numeric(train$region == "region_14")
train$region_15 <- as.numeric(train$region == "region_15")
train$region_16 <- as.numeric(train$region == "region_16")
train$region_17 <- as.numeric(train$region == "region_17")
train$region_18 <- as.numeric(train$region == "region_18")
train$region_19 <- as.numeric(train$region == "region_19")
train$region_20 <- as.numeric(train$region == "region_20")
train$region_21 <- as.numeric(train$region == "region_21")
train$region_22<- as.numeric(train$region == "region_22")
train$region_23 <- as.numeric(train$region == "region_23")
train$region_24 <- as.numeric(train$region == "region_24")
train$region_25 <- as.numeric(train$region == "region_25")
train$region_26 <- as.numeric(train$region == "region_26")
train$region_27 <- as.numeric(train$region == "region_27")
train$region_28 <- as.numeric(train$region == "region_28")
train$region_29 <- as.numeric(train$region == "region_29")
train$region_30 <- as.numeric(train$region == "region_30")
train$region_31 <- as.numeric(train$region == "region_31")
train$region_32 <- as.numeric(train$region == "region_32")
train$region_33<- as.numeric(train$region == "region_33")

summary(train)





str(test)
summary(test)
View(test)

# after analysing the summary we get to know that previous_year_rating contains NA values.
test$previous_year_rating[(is.na(test$previous_year_rating))] = 3
summary(test$previous_year_rating)

test$dept_analy <- as.numeric(test$department == "Analytics")
test$dept_fin <- as.numeric(test$department == "Finance")
test$dept_hr <- as.numeric(test$department == "HR")
test$dept_leg <- as.numeric(test$department == "Legal")
test$dept_oper <- as.numeric(test$department == "Operations")
test$dept_proc <- as.numeric(test$department == "Procurement")
test$dept_sale <- as.numeric(test$department == "Sales & Marketing")
test$dept_tech <- as.numeric(test$department == "Technology")


#dummy_department <- dummy_cols(test$department,remove_first_dummy = T)
#data <- data.frame(c(test,dummy_department))



summary(test)
View(test)

test$gender_m <-  as.numeric(test$gender == "m")

test$recr_re <- as.numeric(test$recruitment_channel == "referred")
test$recr_sou <- as.numeric(test$recruitment_channel == "sourcing")

test$region_1 <- as.numeric(test$region == "region_1")
test$region_2 <- as.numeric(test$region == "region_2")
test$region_3 <- as.numeric(test$region == "region_3")
test$region_4 <- as.numeric(test$region == "region_4")
test$region_5 <- as.numeric(test$region == "region_5")
test$region_6 <- as.numeric(test$region == "region_6")
test$region_7 <- as.numeric(test$region == "region_7")
test$region_8 <- as.numeric(test$region == "region_8")
test$region_9 <- as.numeric(test$region == "region_9")
test$region_10 <- as.numeric(test$region == "region_10")
test$region_11 <- as.numeric(test$region == "region_11")
test$region_12 <- as.numeric(test$region == "region_12")
test$region_13 <- as.numeric(test$region == "region_13")
test$region_14 <- as.numeric(test$region == "region_14")
test$region_15 <- as.numeric(test$region == "region_15")
test$region_16 <- as.numeric(test$region == "region_16")
test$region_17 <- as.numeric(test$region == "region_17")
test$region_18 <- as.numeric(test$region == "region_18")
test$region_19 <- as.numeric(test$region == "region_19")
test$region_20 <- as.numeric(test$region == "region_20")
test$region_21 <- as.numeric(test$region == "region_21")
test$region_22<- as.numeric(test$region == "region_22")
test$region_23 <- as.numeric(test$region == "region_23")
test$region_24 <- as.numeric(test$region == "region_24")
test$region_25 <- as.numeric(test$region == "region_25")
test$region_26 <- as.numeric(test$region == "region_26")
test$region_27 <- as.numeric(test$region == "region_27")
test$region_28 <- as.numeric(test$region == "region_28")
test$region_29 <- as.numeric(test$region == "region_29")
test$region_30 <- as.numeric(test$region == "region_30")
test$region_31 <- as.numeric(test$region == "region_31")
test$region_32 <- as.numeric(test$region == "region_32")
test$region_33<- as.numeric(test$region == "region_33")

summary(test)


final_train <- train[-c(1,2,3,4,5,6)]
summary(final_train)

final_test <- test[-c(1,2,3,4,5,6)]
summary(final_test)

m <- cor(final_train)
corrplot(m,method = "circle")



library(randomForest)

modelrf <- randomForest(as.factor(is_promoted) ~ ., data = final_train, do.trace=T)
modelrf
summary(modelrf)
importance(modelrf)
varImpPlot(modelrf)

predict_rf <- predict(modelrf,final_train)

confusionMatrix(predict_rf, as.factor(final_train$is_promoted))

predict_test <- predict(modelrf,test)

test$is_promoted <- predict(modelrf,test)
View(test)
submission = data.frame(test[c("employee_id","is_promoted")])
write.csv(submission,"E:/R/competion case study/submission.csv",row.names = F)
