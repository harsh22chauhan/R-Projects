rm(list=ls())

mydata <- read.csv("E:/R/ann/rawNNdata.csv")

summary(mydata)

any(is.na(mydata))

View(mydata)

normalize <- function(x){
  return((x-min(x))/(max(x)-min(x))) 
}

maxmindf <- as.data.frame(lapply(mydata,normalize))  
head(maxmindf)

i <- createDataPartition(mydata$dividend,p=0.8,list = F,times = 1)
train_NN <- maxmindf[i,]
test_NN <- maxmindf[-i,]

install.packages("neuralnet")
library(neuralnet)

nn <- neuralnet(dividend ~ fcfps + earnings_growth + de + mcap + current_ratio,
                data = train_NN,hidden = c(2,1),
                linear.output = F ,threshold = 0.01)
nn$result.matrix
plot(nn)

temp_test <- subset(test_NN,select = c("fcfps","earnings_growth","de", "mcap","current_ratio"))

head(temp_test)
nn.results <- compute(nn,temp_test)
results <- data.frame(actual = test_NN$dividend,prediction = nn.results$net.result)

roundedresults <- sapply(results,round,digit = 0)
roundedresultsdf <- data.frame(roundedresults)

attach(roundedresultsdf)
table(actual,prediction)

confusionMatrix(as.factor(actual),as.factor(prediction))
