#clear the enviornment.
rm(list=ls())

#load the data
data <- read.delim("E:/R/rawdata.txt", header = F, sep = ",", dec = ".",na.strings = "?")

#view of data
View(data)

#remove this column as it contains only car names
data <- data[-c(3)]

#Again view the data after removing the column
View(data)

#Set names of the columns for better analysis
colnames(data) <-  
  c("symboling","company", "fueltype" , "aspiration", "doornumber",
    "carbody", "drivewheel", "enginelocation", "wheelbase", "carlength", 
    "carwidth", "carheight", "curbweight", "enginetype", "cylindernumber" , 
    "enginesize", "fuelsystem" , "boreratio", "stroke", "compressionratio", 
    "horsepower", "peakrpm" , "citympg", "highwaympg", "price" )

View(data)

#lets summarise the data for better understanding of the dataset
summary(data)

summary(data$price)

#treatment for the missing values of the data
data$fueltype <- ifelse(data$fueltype== "diesel" ,1,0)
data$aspiration <- ifelse(data$aspiration== "std" ,1,0)
data$doornumber <- ifelse(data$doornumber == "four",1,0)
data$doornumber[is.na(data$doornumber)] = 1
data$boreratio[is.na(data$boreratio)] =3.33
data$stroke[is.na(data$stroke)]=3.255
data$horsepower[is.na(data$horsepower)]= 95
data$peakrpm [is.na(data$peakrpm)]= 5200
data$price [is.na(data$price)]= 10295
summary(data)


#remove this column as it contains 20% NA values 
data <- data[-c(2)]
summary(data)


#convert categorical variables to numerical variables

data$carbody_con<-as.numeric(data$carbody=="convertible")
data$carbody_hard<-as.numeric(data$carbody=="hardtop")
data$carbody_hatch<-as.numeric(data$carbody=="hatchback")
data$carbody_sedan<-as.numeric(data$carbody=="sedan")

data$drivewheel_4wd<-as.numeric(data$drivewheel=="4wd")
data$drivewheel_fwd<-as.numeric(data$drivewheel=="fwd")

data$enginelocation_front<-as.numeric(data$enginelocation=="front")

data$enginetype_dohc<-as.numeric(data$enginetype=="dohc")
data$enginetype_dohcv<-as.numeric(data$enginetype=="dohcv")
data$enginetype_l<-as.numeric(data$enginetype=="l")
data$enginetype_ohc<-as.numeric(data$enginetype=="ohc")
data$enginetype_ohcf<-as.numeric(data$enginetype=="ohcf")
data$enginetype_ohcv<-as.numeric(data$enginetype=="ohcv")


data$cylindernumber_8<-as.numeric(data$cylindernumber=="eight")
data$cylindernumber_5<-as.numeric(data$cylindernumber=="five")
data$cylindernumber_4<-as.numeric(data$cylindernumber=="four")
data$cylindernumber_6<-as.numeric(data$cylindernumber=="six")
data$cylindernumber_3<-as.numeric(data$cylindernumber=="three")
data$cylindernumber_2<-as.numeric(data$cylindernumber=="two")

data$fuelsystem_mpfi<-as.numeric(data$fuelsystem=="mpfi")
data$fuelsystem_2bbl<-as.numeric(data$fuelsystem=="2bbl")
data$fuelsystem_idi<-as.numeric(data$fuelsystem=="idi")
data$fuelsystem_1bbl<-as.numeric(data$fuelsystem=="1bbl")
data$fuelsystem_spdi<-as.numeric(data$fuelsystem=="spdi")
data$fuelsystem_4bbl<-as.numeric(data$fuelsystem=="4bbl")

View(data)
head(data)


#remove the columns that contains categorical data because we converted these variables into numerical variables
final_data <- data[-c(5,6,7,13,14,16)]
View(final_data)
summary(final_data)


#after completing EDA now see how correleated data we have.

par(mfrow = c(1,1))
m <- cor(final_data)
corrplot(m , method = "circle",number.font = .5)


#Now treatment of outliers in the data
bx<- boxplot(final_data$horsepower)
bx$stats


quantile(final_data$horsepower , seq(0,1,0.02))
final_data$horsepower <- ifelse(final_data$horsepower>184,182,final_data$horsepower)

cx<- boxplot(final_data$peakrpm)
bx$stats


quantile(final_data$peakrpm , seq(0,1,0.02))
final_data$peakrpm <- ifelse(final_data$peakrpm>6000,6000,final_data$peakrpm)

dx <- boxplot(final_data$compressionratio)
quantile(final_data$compressionratio, seq(0,1,0.02))
final_data$compressionratio <- ifelse(final_data$compressionratio > 10.94,10.94,final_data$compressionratio)


ex <- boxplot(final_data$enginesize)
quantile(final_data$enginesize, seq(0,1,0.02))
final_data$enginesize <- ifelse(final_data$enginesize > 256.08,256.08,final_data$enginesize)

fx <- boxplot(final_data$curbweight)
quantile(final_data$curbweight , seq(0,1,0.02))


gx <- boxplot(final_data$wheelbase)
quantile(final_data$wheelbase , seq(0,1,0.02))
final_data$wheelbase <- ifelse(final_data$wheelbase > 120.900 , 114.200,final_data$wheelbase)


hx <- boxplot(final_data$carlength)

iX <- boxplot(final_data$carwidth)
quantile(final_data$carwidth , seq(0,1,0.02))
final_data$carwidth <- ifelse (final_data$carwidth > 70.852 ,70.852 , final_data$carwidth)


jX <- boxplot(final_data$carheight)
kx <- boxplot(final_data$citympg)



#divide the data into test and train
set.seed(121)

t=sample(1:nrow(final_data),0.7*nrow(final_data))

t_train = final_data[t,]
t_test = final_data[-t,]


#apply linear regression to the training data set
model1 <- lm(price~.,data= t_train)
step(model1)


#final model for the training dataset
model2 <- lm(formula = price ~ symboling + aspiration + carwidth + carheight + 
               enginesize + boreratio + stroke + compressionratio + horsepower + 
               peakrpm + carbody_con + carbody_sedan + drivewheel_4wd + 
               drivewheel_fwd + enginelocation_front + enginetype_dohc + 
               enginetype_dohcv + enginetype_l + enginetype_ohc + enginetype_ohcf + 
               enginetype_ohcv + cylindernumber_8 + cylindernumber_3, data = t_train)
summary(model2)


#assumption for checking of linear model

durbinWatsonTest(model2) #value is less than 2 ,so our model is good

#check for errrors are normally distriduted or not
hist(residuals(model2))

#testing of linear model
install.packages("lmtest")
library(lmtest)
par(mfrow=c(2,2))

plot(model3)

quantile(final_data$price , seq(0,1,0.02))

final_data_new = final_data[(final_data$price >= 5389 & final_data$price  <= 36809.60),]


#this is our final training model for the training dataset
model3 <- lm(formula = price ~ symboling + aspiration + carwidth + carheight + 
               enginesize + boreratio + stroke + compressionratio + horsepower + 
               peakrpm + carbody_con + carbody_sedan + drivewheel_4wd + 
               drivewheel_fwd + enginelocation_front + enginetype_dohc + 
               enginetype_dohcv + enginetype_l + enginetype_ohc + enginetype_ohcf + 
               enginetype_ohcv + cylindernumber_8 + cylindernumber_3, data = t_train)

summary(model3)

# checking assumptions of linear model
plot(model3)
cor(final_data$horsepower,final_data$price)
boxplot(final_data$horsepower)
durbinWatsonTest(model3)

hist(residuals(model3))


#predict the test dataset from training
predict.lm(model3,t_test)


#add the column of predicted price to the test dataset.
t_test$predict <- predict.lm(model3,t_test)

View(t_test)

#showing head of test data set
head(t_test)

#check the mean absolute error for test data
mape = function(price,predict){
  res = mean(abs((price-predict)/price))
  return(res)
}
mape(t_test$price , t_test$predict)

