# SVM : Iris Prediction
install.packages("e1071")
library(e1071)
set.seed(123)
x <-matrix(rnorm(20*2),ncol=2) # Normal distribution
y<- c(rep(-1,10),rep(1,10)) # Scatter
x[y==1, ] <-x[y==1, ] +1

plot(x,col=(3-y)) #

dat <- data.frame(x=x,y=as.factor(y)) # Binary Classification as.factor(y)
str(dat)

svm_fit <- svm(y ~ ., data=dat, kernel='linear',cost=10 , scale = F) # cost Margin for learning The higher the cost, the wider the margin
svm_fit
plot(svm_fit, dat)

attributes(svm_fit)
svm_fit$index # Check Vector
summary(svm_fit)

# Changing Coas
svm_fit <- svm(y ~ ., data=dat, kernel='linear',cost=0.1 , scale = F) # cost 0.1
plot(svm_fit,dat)
svm_fit$index # result: 14

# finding best cost - cross validation tune()
set.seed(123) # making Random Number
tune.out <-tune(svm, y ~., data = dat, kernel='linear', range=list(cost=c(0.001,0.01,0.1,1.5,10,50)))
summary(tune.out) # best cost = 1.5

bestmod <-tune.out$best.model
bestmod
summary(bestmod)

# Prediction
xtest <- matrix(rnorm(20 * 2),ncol=2) # Sampling
ytest <- sample(c(-1,1),20,rep=T) # Sampling
xtest
ytest

xtest[ytest==1, ] <-xtest[ytest==1, ] +1
testda <- data.frame(x=xtest, y=as.factor(ytest))
testda


ypred <- predict(bestmod,testda)
table(예측값=ypred, 실제값=testda$y)
(5+10)/nrow(testda)


# Iris Dataset Prediction
model <- svm(Species ~., data=iris) #Species을 기준으로 분류
model # Support Vectors 만듦

pred <- predict(model, iris[,-5])
pred
table(pred,iris$Species) #교차분할표
(50 + 48 + 48) / nrow(iris)