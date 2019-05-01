#clear the enviornment.

rm(list=ls())

#import dataset.
data <- read.csv("logistic regression/titanic.csv")

#check the missing values and the mean,median,mode
summary(data)

#in this case, we have 261 missing values for the variable age. we need to treat the missing values first.


#histogram to see how the data is skewed
hist(data$age)

#replacing the NA values for variable 1 with 29
data$age[is.na(data$age)]=  29

#check if the missing values are replaced
summary(data)

#since we have handeled the missing values so we see the head of data
head(data)

#as seen in data, some of our variables are categorical, which we need to create as a dummy variables first

data$female <- ifelse(data$sex == "female",1,0)
data$embarked_a<- ifelse(data$embarked == "S",1,0)
data$embarked_c<- ifelse(data$embarked == "C",1,0 )

#checking the dummy variables
head(data)

#removing the categorical column s(3,4,9)
final_data <- data[ -c(3,4,9)]

#lets check our final data
head(final_data)

# since all the values are either continous or binary we can now begin with analysis process
# we first start with univarate analysis

bx <- boxplot(final_data$age)
bx$stats

#getting the quantile values

quantile(final_data$age , seq(0,1,0.02))

#based on the boxplot outliers we are capping below 4% & above 96%

final_data$age <- ifelse(final_data$age >= 52,52,final_data$age)
final_data$age <- ifelse(final_data$age <= 4,4,final_data$age)

#checking the outliers
boxplot(final_data$age)

#lets see the outliers for fare as well

cx <- boxplot(final_data$fare)
cx$stats

#quantile values for fare

quantile(final_data$fare, seq(0,1,0.02))

#fare capping at 96 percentile

final_data$fare <- ifelse(final_data$fare >=  136,136,final_data$fare)
boxplot(final_data$fare)

#now lets check do the bi-variate analysis just to analysis data

library(car)
scatterplot(final_data$age,final_data$survived)

#survived vs fare
scatterplot(final_data$fare,final_data$survived)

#lets divide the data into test and train

set.seed(222)

t=sample(1:nrow(final_data),0.7*nrow(final_data))

t_train = final_data[t,]
t_test = final_data[-t,]

#checking the multi-collinearity

mod <- lm(survived ~ ., data=t_train)
t = vif(mod)
sort(t,decreasing = T)

# since vif factor is less than the value of 5, we consider all the variables.

#since all variables are below the threshold of 5, we can proceed with te model.

mod1 <- glm(as.factor(survived) ~ .,family="binomial",data=t_train)
summary(mod1)

#instead of removing all these variables one by one, we use step function, which
#automatically calculated the best equation

stpmod = step(mod1, direction = "both")
formula(stpmod)
summary(stpmod)

mod2 <-  glm(formula = as.factor(survived) ~ pclass + age + sibsp + female + 
               embarked_c, family = "binomial", data = t_train)


#checking the probability for each observation by creating a variable names score
t_train$score = predict(mod2,newdata = t_train,type= "response")
head(t_train$score)
tail(t_train$score)

library(lattice)
library(ggplot2)
library(caret)
install.packages("e1071")
library(e1071)

prediction <- ifelse(t_train$score>=0.6,1,0)
confusionMatrix(as.factor(prediction),as.factor(t_train$survived),positive="1")

install.packages("InformationValue")
library(InformationValue)
plotROC(actuals = t_train$survived,predictedScores = as.numeric(fitted(mod2)))
ks_plot(actuals = t_train$survived,predictedScores = as.numeric(fitted(mod2)))
ks_stat(actuals = t_train$survived,predictedScores = as.numeric(fitted(mod2)))

t_test$score2 = predict(mod2,t_test,type= "response")
View(t_test)
t_test$prediction <- ifelse(t_test$score2>=0.6,1,0)
View(t_test)
confusionMatrix(prediction,t_test$survived)
caret::confusionMatrix(as.factor(prediction),as.factor(t_test$survived))
write.csv(t_test , "E:/titanic_test.csv")
