# 불러올 데이터파일 위치 지정
setwd("C:/Users/CS3-32/Desktop/수업자료/빅데이터기술/R프로젝트")
getwd()

# 데이터 불러오기
money = read.csv("money.csv")
# 필요없는 열 제거
money <- money[,!names(money)%in%c("fnlwgt","education_num","capital.gain","capital.loss")]

# 불러온 데이터 확인 
str(money)
head(money)
View(money)

# 범주 확인
levels(money$workclass)
table(money$workclass)

levels(money$occupation)
table(money$occupation)

levels(money$native.country)
table(money$native.country)

# ?값 변경 
levels(money$workclass)[1] <- " Private"
levels(money$occupation)[1] <- " etc"
levels(money$native.country)[1] <- " etc"

# 결측치 행 제거 및 확인 
install.packages("dplyr")
library(dplyr)
money <- na.omit(money)
money
# 결측치는 없는것으로 확인 

# 이상치 제거하기
boxplot(money$hours.per.week)
boxplot(money$hours.per.week)$stats
money <- money[money$hours.per.week <= 60,]
money <- money[money$hours.per.week >= 25,]

# 데이터 분리하기
install.packages('caret')
library(caret)
set.seed(137)
test_idx <- createDataPartition(money$income, p=0.1)$Resample1
money.test <- money[test_idx,]
nrow(money.test)
money.train <- money[-test_idx,]
nrow(money.train)

# 교차검증준비
createFolds(money.train$income, k=10)
# 함수로 구현
create_ten_fold_cv <- function() {
  set.seed(137)
  lapply(createFolds(money.train$income, k=10), function(idx){
    return(list(train=money.train[-idx,],
                validation=money.train[idx,]))
  })
}
x <- create_ten_fold_cv()
str(x)
head(x$Fold01$train)

# 세가지 종류의 데이터를 하나의 파일로 저장
save(money, money.test, money.train, file="money.RData")

# summary함수로 데이터 비율 분석
install.packages("Hmisc")
library(Hmisc)
data <- money.train
str(data)
summary(income ~ age + workclass + education + marital.status + 
          occupation + relationship + race + sex, method="reverse", data=data)

# featureplot으로 numeric 데이터 시각화
install.packages("ellipse")
library(ellipse)
data.complete <- data[complete.cases(data),]
featurePlot(
  data.complete[,
                sapply(names(data.complete),
                       function(n){ is.numeric(data.complete[,n]) })],
  data.complete[,c("income")],
  "ellipse")

# mosaicplot으로 factor 데이터 시각화
str(data)
mosaicplot(income ~ race + sex, data=data, color=TRUE, main="race and sex")

# xtabs로 분할표 만들기
xtabs(~ race + sex, data=data)
xtabs(income == " >50K" ~ race + sex, data=data)
xtabs(income == " <=50K" ~ race + sex, data=data)
xtabs(income == " >50K" ~ race + sex, data=data) / xtabs( ~ race + sex, data=data)

# rpart의 교차 검증
library(rpart)
m <- rpart(
  income ~ age + workclass + education + marital.status
  + occupation + relationship + race + sex,
  data=data)
p <- predict(m, newdata=data, type="class")
head(p)

library(foreach)
folds <- create_ten_fold_cv()

rpart_result <- foreach(f=folds)%do%{
  model_rpart <- rpart(
    income ~ age + workclass + education + marital.status 
    + occupation + relationship + race + sex,
    data=f$train)
  predicted <- predict(model_rpart, newdata=f$validation,
                       type="class")
  return(list(actual=f$validation$income, predicted=predicted))
}
head(rpart_result)

#ctree의 교차 검증 
install.packages("party")
library(party)
ctree_result <- foreach(f=folds) %do% {
  model_ctree <- ctree(
    income ~ age + workclass + education + marital.status 
    + occupation + relationship + race + sex,
    data=f$train)
  predicted <- predict(model_ctree, newdata=f$validation,
                       type="response")
  return(list(actual=f$validation$income, predicted=predicted))
}
head(ctree_result)

# 정확도 평가
evaluation <- function(lst) {
  accuracy <- sapply(lst, function(one_result) {
    return(sum(one_result$predicted == one_result$actual)
           /NROW(one_result$actual))
  })
  print(sprintf("정확도 : %.3f  오차범위 : %.3f",
                mean(accuracy), sd(accuracy)))
  return(accuracy)
}

# 성능과 표준편차 출력
rpart_accuracy <- evaluation(rpart_result)
ctree_accuracy <- evaluation(ctree_result)
plot(density(rpart_accuracy), main="rpart model")
plot(density(ctree_accuracy), main="ctree model")

# 두 개의 정확도 밀도그래프 비교
plot(density(rpart_accuracy), main="rpart VS ctree")
lines(density(ctree_accuracy), col="red", lty="dashed")

# 추가 예측 시도

# 나이값 범주데이터로 바꾸기
money <- transform(money,
                   age = ifelse(age < 20, "15",
                                ifelse(age >=20 & age < 30, "25",
                                       ifelse(age >= 30 & age < 40, "35",
                                              ifelse(age >= 40 & age < 50, "45",
                                                     ifelse(age >= 50 & age < 60, "55",
                                                            ifelse(age >= 60 & age < 70, "65","75")))))))

money$age <- as.factor(money$age)
head(money)
str(money)

# 근로시간 범주데이터로 바꾸기
money <- transform(money,
                   hours.per.week = ifelse(hours.per.week <= 30, "30",
                                           ifelse(hours.per.week > 30 & hours.per.week <= 40, "40",
                                                  ifelse(hours.per.week > 40 & hours.per.week <= 50, "50", "60"))))

money$hours.per.week <- as.factor(money$hours.per.week)
head(money)
str(money)

View(money)

# 근로지역 비공개값 제거
money <- money[!money$workclass == " Private",]

#######################################################################
# 발표 후 추가 된 부분
#######################################################################

# 1
# 피어슨 상관계수로 상관관계 찾기
# 수치형 데이터 2개로 계산한 결과 나이와 근로시간은 상관관계를 갖지 않음
# 데이터에 수치형 데이터보다 팩터데이터가 많아서 활용하기 어려움 
str(money)
cor(money$age,money$hours.per.week)

# 2
# ?값을 기존데이터에 합치지말고 다른 값으로 구분하고 예측하기
# 확인 결과 rpart모델은 기존과 동일, ctree모델은 오차범위안에서 0.1퍼센트 증가
# 범주 확인
levels(money$workclass)
table(money$workclass)

levels(money$occupation)
table(money$occupation)

levels(money$native.country)
table(money$native.country)

# ?값 변경 
levels(money$workclass)[1] <- " etc"
levels(money$occupation)[1] <- " etc"
levels(money$native.country)[1] <- " etc"

# 3
# 테스트 데이터의 분리비율 바꾸기
# 테스트 데이터의 비율을 40퍼센트로 증가한 후 예측 정확도 확인
# 확인 결과 rpart는 오차범위내에서 0.2퍼센트 하락
# ctree모델은 오차범위내에서 기존 대비 0.3퍼센트 증가
# 정확도의 큰 향상은 없었지만 간단한 편집으로도 정확도가 변하는 것을 확인
# 데이터 분리하기
install.packages('caret')
library(caret)
set.seed(137)
test_idx <- createDataPartition(money$income, p=0.4)$Resample1
money.test <- money[test_idx,]
nrow(money.test)
money.train <- money[-test_idx,]
nrow(money.train)

# 4
# #2의 '?'값 분리와 #3의 테스트 데이터의 분리비율 증가를 동시 실행
# rpart모델의 경우 기존보다 하락한 것을 확인
# ctree모델의 경우 오차범위 내 증가지만 여러번의 시도 중 가장 큰 정확도 증가 확인

# ?값 변경 
levels(money$workclass)[1] <- " etc"
levels(money$occupation)[1] <- " etc"
levels(money$native.country)[1] <- " etc"

# 데이터 분리하기
install.packages('caret')
library(caret)
set.seed(137)
test_idx <- createDataPartition(money$income, p=0.4)$Resample1
money.test <- money[test_idx,]
nrow(money.test)
money.train <- money[-test_idx,]
nrow(money.train)