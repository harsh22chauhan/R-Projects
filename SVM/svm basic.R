x =c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
y =c(3,4,5,4,8,10,10,11,14,20,23,24,32,34,35,37,42,48,53,60)

train = data.frame(x,y)

plot(train, pch = 16)

model <- lm(y~x,train)
summary(model)

abline(model)

library(e1071)

model_svm <- svm(y~x,train)
summary(model_svm)
pred <- predict(model_svm,train)

points(train$x,pred,col = 'blue',pch = 4)

error <- model$residuals
lm_error <- sqrt(mean(error ^ 2))
lm_error

error_2 <- train$y - pred
svm_error <- sqrt(mean(error_2^2))
svm_error
