# �ҷ��� ���������� ��ġ ����
setwd("C:/Users/CS3-32/Desktop/�����ڷ�/�����ͱ��/R������Ʈ")
getwd()

# ������ �ҷ�����
money = read.csv("money.csv")
# �ʿ���� �� ����
money <- money[,!names(money)%in%c("fnlwgt","education_num","capital.gain","capital.loss")]

# �ҷ��� ������ Ȯ�� 
str(money)
head(money)
View(money)

# ���� Ȯ��
levels(money$workclass)
table(money$workclass)

levels(money$occupation)
table(money$occupation)

levels(money$native.country)
table(money$native.country)

# ?�� ���� 
levels(money$workclass)[1] <- " Private"
levels(money$occupation)[1] <- " etc"
levels(money$native.country)[1] <- " etc"

# ����ġ �� ���� �� Ȯ�� 
install.packages("dplyr")
library(dplyr)
money <- na.omit(money)
money
# ����ġ�� ���°����� Ȯ�� 

# �̻�ġ �����ϱ�
boxplot(money$hours.per.week)
boxplot(money$hours.per.week)$stats
money <- money[money$hours.per.week <= 60,]
money <- money[money$hours.per.week >= 25,]

# ������ �и��ϱ�
install.packages('caret')
library(caret)
set.seed(137)
test_idx <- createDataPartition(money$income, p=0.1)$Resample1
money.test <- money[test_idx,]
nrow(money.test)
money.train <- money[-test_idx,]
nrow(money.train)

# ���������غ�
createFolds(money.train$income, k=10)
# �Լ��� ����
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

# ������ ������ �����͸� �ϳ��� ���Ϸ� ����
save(money, money.test, money.train, file="money.RData")

# summary�Լ��� ������ ���� �м�
install.packages("Hmisc")
library(Hmisc)
data <- money.train
str(data)
summary(income ~ age + workclass + education + marital.status + 
          occupation + relationship + race + sex, method="reverse", data=data)

# featureplot���� numeric ������ �ð�ȭ
install.packages("ellipse")
library(ellipse)
data.complete <- data[complete.cases(data),]
featurePlot(
  data.complete[,
                sapply(names(data.complete),
                       function(n){ is.numeric(data.complete[,n]) })],
  data.complete[,c("income")],
  "ellipse")

# mosaicplot���� factor ������ �ð�ȭ
str(data)
mosaicplot(income ~ race + sex, data=data, color=TRUE, main="race and sex")

# xtabs�� ����ǥ �����
xtabs(~ race + sex, data=data)
xtabs(income == " >50K" ~ race + sex, data=data)
xtabs(income == " <=50K" ~ race + sex, data=data)
xtabs(income == " >50K" ~ race + sex, data=data) / xtabs( ~ race + sex, data=data)

# rpart�� ���� ����
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

#ctree�� ���� ���� 
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

# ��Ȯ�� ��
evaluation <- function(lst) {
  accuracy <- sapply(lst, function(one_result) {
    return(sum(one_result$predicted == one_result$actual)
           /NROW(one_result$actual))
  })
  print(sprintf("��Ȯ�� : %.3f  �������� : %.3f",
                mean(accuracy), sd(accuracy)))
  return(accuracy)
}

# ���ɰ� ǥ������ ���
rpart_accuracy <- evaluation(rpart_result)
ctree_accuracy <- evaluation(ctree_result)
plot(density(rpart_accuracy), main="rpart model")
plot(density(ctree_accuracy), main="ctree model")

# �� ���� ��Ȯ�� �е��׷��� ��
plot(density(rpart_accuracy), main="rpart VS ctree")
lines(density(ctree_accuracy), col="red", lty="dashed")

# �߰� ���� �õ�

# ���̰� ���ֵ����ͷ� �ٲٱ�
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

# �ٷνð� ���ֵ����ͷ� �ٲٱ�
money <- transform(money,
                   hours.per.week = ifelse(hours.per.week <= 30, "30",
                                           ifelse(hours.per.week > 30 & hours.per.week <= 40, "40",
                                                  ifelse(hours.per.week > 40 & hours.per.week <= 50, "50", "60"))))

money$hours.per.week <- as.factor(money$hours.per.week)
head(money)
str(money)

View(money)

# �ٷ����� ������� ����
money <- money[!money$workclass == " Private",]

#######################################################################
# ��ǥ �� �߰� �� �κ�
#######################################################################

# 1
# �Ǿ �������� ������� ã��
# ��ġ�� ������ 2���� ����� ��� ���̿� �ٷνð��� ������踦 ���� ����
# �����Ϳ� ��ġ�� �����ͺ��� ���͵����Ͱ� ���Ƽ� Ȱ���ϱ� ����� 
str(money)
cor(money$age,money$hours.per.week)

# 2
# ?���� ���������Ϳ� ��ġ������ �ٸ� ������ �����ϰ� �����ϱ�
# Ȯ�� ��� rpart���� ������ ����, ctree���� ���������ȿ��� 0.1�ۼ�Ʈ ����
# ���� Ȯ��
levels(money$workclass)
table(money$workclass)

levels(money$occupation)
table(money$occupation)

levels(money$native.country)
table(money$native.country)

# ?�� ���� 
levels(money$workclass)[1] <- " etc"
levels(money$occupation)[1] <- " etc"
levels(money$native.country)[1] <- " etc"

# 3
# �׽�Ʈ �������� �и����� �ٲٱ�
# �׽�Ʈ �������� ������ 40�ۼ�Ʈ�� ������ �� ���� ��Ȯ�� Ȯ��
# Ȯ�� ��� rpart�� �������������� 0.2�ۼ�Ʈ �϶�
# ctree���� �������������� ���� ��� 0.3�ۼ�Ʈ ����
# ��Ȯ���� ū ����� �������� ������ �������ε� ��Ȯ���� ���ϴ� ���� Ȯ��
# ������ �и��ϱ�
install.packages('caret')
library(caret)
set.seed(137)
test_idx <- createDataPartition(money$income, p=0.4)$Resample1
money.test <- money[test_idx,]
nrow(money.test)
money.train <- money[-test_idx,]
nrow(money.train)

# 4
# #2�� '?'�� �и��� #3�� �׽�Ʈ �������� �и����� ������ ���� ����
# rpart���� ��� �������� �϶��� ���� Ȯ��
# ctree���� ��� �������� �� �������� �������� �õ� �� ���� ū ��Ȯ�� ���� Ȯ��

# ?�� ���� 
levels(money$workclass)[1] <- " etc"
levels(money$occupation)[1] <- " etc"
levels(money$native.country)[1] <- " etc"

# ������ �и��ϱ�
install.packages('caret')
library(caret)
set.seed(137)
test_idx <- createDataPartition(money$income, p=0.4)$Resample1
money.test <- money[test_idx,]
nrow(money.test)
money.train <- money[-test_idx,]
nrow(money.train)