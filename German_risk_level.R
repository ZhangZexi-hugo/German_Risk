install.packages('ggplot2')
install.packages('dplyr')
install.packages('tidyverse')
install.packages('ggpubr')
install.packages('corrplot')
install.packages("ade4")
install.packages('bgm')
library(rpart)
library(rpart.plot)
install.packages("randomForest")
library(randomForest)
library('ade4')
library('ggpubr')
library('dplyr')
library('tidyverse')
library('ggplot2')
library('corrplot')
library(data.table)

# data import
# dataset is from kaggle, we only use the dataset for educational use. 

data<-read.table(file.choose(), header=(T), sep="," )

#Summary of dataset
#we have 10 columns x,Age,Sex,Job,Housing,Saving.accounts,Checking.account,Credit.amount,and Duration.
summary(data)
glimpse(data)

#Data clean, Check the missing value in the dataset 

# Missing value graph 
na.plot=function(data){
  missing1=sapply(data,function(x)sum(x == '')/nrow(data))
  missing2=sapply(data,function(x)sum(sum(is.null(x)), sum(is.na(x)))/nrow(data))
  if(sum(is.na(missing1))>0){
    missing1[is.na(missing1)] = 0
  }
  missing = missing1 + missing2
  print(missing)
  
  missing=missing[order(missing,decreasing = T)]
  nadata=missing[missing>0]
  na_df=data.frame(var=names(nadata),na=nadata,row.names = NULL)
  ggplot(na_df)+
    geom_bar(aes(x=reorder(var,na),y=na),stat='identity', fill='red')+
    labs(y='% Missing',x=NULL,title='Percent of Missing Data by Feature') +
    coord_flip(ylim = c(0,1))  
}
# filling the missing value by using mode, because the type of missing data is factor.
na.plot(data)
data_saving<-filter(data,!is.na(data$Saving.account))$Saving.account
data_checking<-filter(data,!is.na(data$Checking.account))$Checking.account
getmode<-function(b){
  uniqv<-unique(b)
  uniqv[which.max(tabulate(match(b,uniqv)))]
}
getmode(data_saving)
getmode(data_checking)
data$Checking.account[is.na(data$Checking.account)]=getmode(data_checking)
data$Saving.accounts[is.na(data$Saving.accounts)]=getmode(data_saving) 
sum(is.na(data))
ggplot(data,aes(x=data$Risk))+geom_bar()+labs(title='Frequency abr chart of Risk')


# EDA
# Age and Risk
ggplot(data = data,aes(x=data$Age,fill=factor(Risk)))+geom_bar(stat='count', position='dodge')+labs(title='Age and Risk')
summary(data$Age)
data_age=data
data_age<-as.data.frame(data_age)
data_age$Age[which(data_age$Age <28&data_age$Age>=18)]<-'Young'
data_age$Age[which(data_age$Age<38&data_age$Age>=28)]<-'Adult'
data_age$Age[which(data_age$Age<48&data_age$Age>=38)]<-'major workers'
data_age$Age[which(data_age$Age<58&data_age$Age>=48)]<-'5 year workers'
data_age$Age[which(data_age$Age<68&data_age$Age>=58)]<-'senior workers'
data_age$Age[which(data_age$Age<78&data_age$Age>=68)]<-'Senior'
ggplot(data = data_age,aes(x=data_age$Age,fill=factor(Risk)))+geom_bar(stat='count', position='dodge')+labs(title='Age and Risk')
data_age


# Credit.amount & Risk
data_Credit=data
data_Credit$Credit.amount[which(data_Credit$Credit.amount<=1366)]<-1
data_Credit$Credit.amount[which(data_Credit$Credit.amount>1366&data_Credit$Credit.amount<=2320)]<-2
data_Credit$Credit.amount[which(data_Credit$Credit.amount>2320&data_Credit$Credit.amount<=3972)]<-3
data_Credit$Credit.amount[which(data_Credit$Credit.amount>3972&data_Credit$Credit.amount<=18424)]<-4
ggplot(data = data_Credit,aes(x=Credit.amount,fill=factor(Risk)))+geom_bar(stat='count', position='dodge')+labs(title='Credit Amount & Risk')

#Job & Risk
ggplot(data = data,aes(x=data$Job,fill=factor(Risk)))+geom_bar(stat='count', position='dodge')+labs(title='Job & Risk')
#Housing & Risk
ggplot(data = data,aes(x=Housing,fill=factor(Risk)))+geom_bar(stat='count', position='dodge')+labs(title='Housing & Risk')
#Saving & Risk
ggplot(data = data,aes(x=data$Saving.accounts,fill=factor(Risk)))+geom_bar(stat='count', position='dodge')+labs(title='Saving account & Risk')
#Checking & Risk
ggplot(data = data,aes(x=data$Checking.account,fill=factor(Risk)))+geom_bar(stat='count', position='dodge')+labs(title='Checking account & Risk')
#Sex & Risk
ggplot(data = data,aes(x=data$Sex,fill=factor(Risk)))+geom_bar(stat='count', position='dodge')+labs(title='Gender & Risk')
#Duration & Risk
ggplot(data = data,aes(x=data$Duration,fill=factor(Risk)))+geom_bar(stat='count', position='dodge')+labs(title='Duration & Risk')

ggplot(data = data,aes(x=data$Purpose,fill=factor(Risk)))+geom_bar(stat='count', position='dodge')+labs(title=' Purpose & Risk')




# One_hot encoding(tested the lable encoding which has a lower model performance)
data_1hot<-data
data_1hot<-select(data_1hot,-c(10))
one_feat=c('Sex','Job','Housing','Saving.accounts','Checking.account','Risk')
for (f in one_feat){
  data_onehot = acm.disjonctif(data_1hot[f])
  data_1hot[f] = NULL
  data_1hot = cbind(data_1hot, data_onehot)
}
# setting the training and test set 8:2
data_1hot<-data_1hot[2:21]
test<-sample(1:nrow(data_1hot),size=200)
train<-(1:nrow(data_1hot))[-test]
data_train<-data_1hot[train,]
data_test<-data_1hot[test,]

xtrain=data_train[,1:19]
ytrain=data_train[,20]
xtest=data_test[,1:19]
ytest=data_test[,20]


#Logistic regression


glm_risk<-glm(Risk.bad~.,data = data_train,family=binomial("logit"))


prediction <-predict(glm_risk,newdata=xtest)
prediction
err1 <- mean(as.numeric(prediction > 0.45) != ytest)
print(paste('Model performance:',1-err1))
glm_risk
summary(glm_risk)

#tree model
TreeModel <- rpart(Risk.bad~., data = data_train)

prp(TreeModel, type = 2, extra = 1)

fitTree <- predict(TreeModel, newdata = xtest)
plot(fitTree)

err2 <- mean(as.numeric(fitTree > 0.45) != ytest)
print(paste('Model performance:',1-err2))
summary(TreeModel)
# random forest


#XGBoost 
library(xgboost)
xtrain=as.matrix(xtrain)
ytrain=as.matrix(ytrain)
xtest=as.matrix(xtest)
ytest=as.matrix(ytest)
xgb = xgboost(data = xtrain, 
              label = ytrain, 
              eta = 0.4,
              max_depth = 4, 
              gamma = 10,
              objective = "binary:logistic",
              nrounds = 30, 
              subsample = 0.8,
              colsample_bytree = 0.8,
              seed = 1,
              eval_metric = "error"
)
xgb
print(xgb)
xgbpredict = predict(xgb,xtest)
xgbpredict
plot(xgbpredict)

err3 <- mean(as.numeric(xgbpredict > 0.45) != ytest)
print(paste("Score=", 1-err3))
