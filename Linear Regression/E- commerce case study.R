#clear the enviornment
rm(list=ls())
getwd()

#read the data files
test <- read.csv("test.csv") #test data
data <- read.csv("data.csv") #train data
View(data)

#summary Statistics of data
summary(data)

#removing NA values
hist(data$Age)

#As data is normally distributed we used mean of data, If data is skewed we always use median for NA values
data$Age[(is.na(data$Age))]= 39

#summary of data after removing NA values
summary(data)

#view of data i.e No of columns, names of columns etc
head(data,10)

#creating dummy variables as we can't do our analysis on categorical variables
data$Job.type_Employed<-as.numeric(data$Job.Type=="Employed")
data$Job.type_Retired<-as.numeric(data$Job.Type=="Retired")
data$Job.type_Unemployed<-as.numeric(data$Job.Type=="Unemployed")
data$Married_y<-as.numeric(data$Marital.Status=="Yes")
data$Education_Secondary<-as.numeric(data$Education=="Secondry")
data$Education_gra<-as.numeric(data$Education=="Graduate")
data$Metro_y<-as.numeric(data$Metro.City=="Yes")


#view of data after creating dummy variables
head(data)


#removal of categorical variable columns as we created dummy variables for them
final_data <- data[-c(2,3,4,5)]

#view of our final data
head(final_data)

#partition of plot area for clear visualisation
par(mfrow = c(1,2))

#univariate Analysis
#plot box plot for detecting outliers in Age column
bx = boxplot(final_data$Age)

#showing values of box plot
bx$stats

#treatment of outliers in age column
quantile(final_data$Age, seq(0,1,0.02)) #divide data into quantiles

#we take 2% values for our outliers
final_data$Age <- ifelse(final_data$Age>60,57,final_data$Age) 

#boxplot after removal of outliers
boxplot(final_data$Age)

#univariate analysis for signed.in.since.Days
cx <- boxplot(final_data$Signed.in.since.Days.)
cx$stats
quantile(final_data$Signed.in.since.Days., seq(0,1,0.02))
final_data$Signed.in.since.Days. <- ifelse(final_data$Signed.in.since.Days.<45,48,final_data$Signed.in.since.Days.)
boxplot(final_data$Signed.in.since.Days.)
par(mfrow=c(1,2))

#plot histogram for dependent variable
hist(final_data$Purchase.made, main = 'Depend')
px <- boxplot(final_data$Purchase.made)  # analysis of dependent variable


#Bivariate Analysis

library(car)

#scatter plot of age,purchase made
scatterplot(final_data$Age,final_data$Purchase.made)

#scatter plot of signed.in,purchase made
scatterplot(final_data$Signed.in.since.Days.,final_data$Purchase.made)

#correlation of each variable.
cor(final_data)

#apply linear Model
model1 <- lm(Purchase.made~.,data=final_data)
vif(model1)



#this will give the best model for our dataset
step(model1)
model2 <- lm(Purchase.made ~ Age + Signed.in.since.Days. + Job.type_Retired + 
               Job.type_Unemployed + Married_y + Education_gra + Metro_y, 
             data = final_data)
summary(model2)


#when p is low null will go. =  null rejected    ??? 0.05
#when p is high null will fly. =fail to reject   > 0.05
# null hypothesis is hypothesis of no impact


model3 <- lm(Purchase.made ~ Signed.in.since.Days. + 
               Job.type_Unemployed + Married_y + Education_gra + Metro_y, 
             data = final_data)
summary(model3)


#testing of linear model
install.packages("lmtest")
library(lmtest)
par(mfrow=c(2,2))

plot(model3)

#improving our linear model
quantile(final_data$Purchase.made ,seq(0,1,0.02))

final_data_new = final_data[(final_data$Purchase.made >=510 & final_data$Purchase.made <= 13500),]
mod2 <- lm(Purchase.made ~ Signed.in.since.Days. + 
             Job.type_Unemployed + Married_y + Education_gra + Metro_y, 
           data = final_data_new)
summary(mod2)
mod3 <- lm(Purchase.made ~ Signed.in.since.Days. + 
              Married_y + Education_gra + Metro_y, 
           data = final_data_new)
summary(mod3)


#after improving plot our model
plot(mod3)

#assumption checking of linear model

#1.durbinwatson Test
durbinWatsonTest(mod3)


#value of D-W statistic is considered good if it is less than 2.
hist(residuals(mod3))

#2. errors are normally distributed
plot(final_data_new$Purchase.made,residuals(mod3))


#applying our model on test dataset.
predict.lm(mod3,test)

#adding prediction column to test data
test$prediction <- predict.lm(mod3,test)
head(test)


#sorting of data in descending order
test1 <- test[order(-test$prediction),]

#taking top 60 values
final_test <-test1[c(1:60),]
head(final_test)
tail(final_test)


#export the file into excel format
library(xlsx)
write.xlsx(final_test, "E:/final_test.xlsx")
