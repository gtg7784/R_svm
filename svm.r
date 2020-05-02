# 지도학습(회귀, SVM 택 1) 기반 R 프로그래밍
# 201911314 경영학과 1학년 엄다니엘

# Q. 데이터셋 범주/배경 이해(외부 리서치 활용 가능)
# A. 주식 종가 데이터셋입니다.
# 주3가, 거래량, OBV, 기관_순매수누적, 외국인_순매수누적, 유로, 달러, 엔화, 위안, 소비자심리지수, 자동차판매, 예상종가 가 있습니다.


# Q. 알고리즘 선택 이유
# A. Linear Regression을 사용해도 되는 문제이지만, SVM은 다음 강의인 Knn 모델과 유사한, Regression에서만 사용되는 것이 아닌, Classification에도 사용 할 수 있기 때문에, SVM 모델을 사용했습니다.
# SVM 모델은 Hyperplane 또는 Hyperplane의 집합으로 이뤄진 모델입니다.
# Regression에서의 SVM은 일정한 Margin 안에서, 두 Class간의 거리를 최대로 늘리고, 최대한 많은 데이터셋을 두 Class 사이에 최대한 많은 데이터셋이 들어갈 수 있도록, 선형 회귀 모델을 만든 후, 그를 기반으로 테스트하는 것 입니다.
# Classification에서의 SVM은 Perceptron에서 사용되는 개념을 토대로 데이터를 Classification하는 방식의 모델입니다.
# SVM은 Train Dataset을 사용하여, Perceptron Function을 정의 한 후, Test Dataset을 기준으로 분류를 하면서 학습을 진행하는 형태입니다.

# Q. 알고리즘 적용 분석 단계별 설명
# A. 아래 코드에 작성했습니다. 코드마다 설명을 적어놓았으니, 아래 코드를 참고하시면 될 것 같습니다.
# 주식 차트 분석을 접해본 경험이 없어서, 단순히 주가와 거래량을 기준으로 해봤습니다.
# 아직 R 언어에 대한 이해도가 부족하여, 원하는 대로 코드를 완성시키지 못하였습니다. 이점 참고 부탁드립니다.

# Q. 분석 결과/검증 결과 또는 run되지 않을지라도 예측되는 결과 기술
# A. 주가와 거래량을 기반으로 예상 종가를 예측하는 모델입니다. X축에 주가를 넣고, Y축에 거래량을 넣어서 예상종가에 대한 예측을 SVM 선형회귀로 할 예정입니다.
# 결과값은 plot을 사용하여, Rplots.pdf 라는 파일명으로 저장됩니다.

# 레포트에 필요한 패키지를 다운로드 받습니다. e1071 패키지가 없으면 아래 코드의 주석을 풀어서, 실행하면 됩니다.
# install.packages("e1071", repos="http://cran.us.r-project.org")

library(e1071) # SVM 모델을 위한 e1701 패키지를 사용합니다.

csv <- read.csv('./dataset.csv', na = "-", fileEncoding = "CP949", encoding = "UTF-8") # 현재 디렉토리에 있는 dataset.csv룰 불러옵니다. dataset에 한국어가 있기 때문에 파일을 CP949로 인코딩합니다.
print(dim(csv)) # dataset의 행과 열의 개수를 출력합니다.
print(str(csv)) # dataset의 속성과, 길이, 미리보기를 출력합니다.

dataset = data.frame(x=csv$주가, y=csv$거래량) # x값을 주가로 하고, y값을 거래량으로 하는 데이터 프레임을 만듭니다.

fit <- svm(y ~ ., data=dataset, kernel='linear', cost=10, sacle=F) # 마진값이 최대인 모델을 만듧니다.
fit
plot(fit, dataset, main="Model Fit") # plot로 시각화를 합니다.

attributes(fit)
print("Vector") # 벡터를 확인합니다.
print(fit$index)
summary(fit)

# Cost 를 바꿉니다.
fit <- svm(y ~ ., data=dataset, kernel='linear', cost=0.1, sacle=F) # cost를 0.1로 바꿉니다.
attributes(fit)
print("Vector") # 벡터를 확인합니다.
print(fit$index)

# 가장 좋은 cost를 찾습니다.
set.seed(123) # 난수를 만듭니다
bestCost <-tune(svm, y ~., data = dataset, kernel='linear', range=list(cost=c(0.001,0.01,0.1,1.5,10,50)))
model <- bestCost$best.model # 
model
summary(model)

t_index <- sample(1:nrow(dataset), size=nrow(dataset)*0.7) # 훈련 데이터셋과 테스트 데이터셋을 나누기 위한 index를 만듭니다.
train <- dataset[t_index, ] # 훈련 데이터셋입니다.
test <- dataset[-t_index, ] # 테스트 데이터셋입니다.

# model <- svm(y ~ ., data=train, gamma=1, cost=16, scale=F) # 훈련 데이터셋을 사용하여 svm 모델을 만듭니다.

predict <- predict(model, test)
table(예측값=predict, 실제값=test$y) # table을 만듭니다.
(5+10)/nrow(test)

print(attributes(model)) # 모델의 속성을 출력합니다.
print(summary(model)) # 모델의 미리보기를 출력합니다.

result <- predict(model, test)

plot(model, dataset)

# plot(x=test$주가, y=result, main="Support Vector Regression")

mse <- mean((result - test$주가)^2) # 평균 제곱 오차를 구해, 오차를 만듭니다.
print(mse) # mse를 출력합니다.

# 감사합니다. 앞으로 더 열심히 공부하도록 하겠습니다.