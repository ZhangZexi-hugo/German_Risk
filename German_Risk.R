install.packages('ggplot2')
install.packages('dplyr')
install.packages('tidyverse')
install.packages('ggpubr')
install.packages('corrplot')
install.packages("ade4")
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


#Good credit and bad Credit
ggplot(data,aes(x=data$Risk))+geom_bar()+labs(title='Frequency abr chart of Risk')


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


one_feat=c('Sex','Job','Housing','Saving.accounts','Checking.account','Purpose','Risk')

for (f in one_feat){
  data_onehot = acm.disjonctif(data[f])
  data[f] = NULL
  data = cbind(data, data_onehot)
}



cor(data,data$Risk_good)
cor(data,data$Risk_bad)
corrplot(cor(data,data$Risk_good),tl.col="black",method="color",shade.lwd = 0.1,tl.cex=0.8)
corrplot(cor(data,data$Risk_bad),tl.col="black",method="color",type='lower',shade.lwd = 0.1,tl.cex=0.8,order='AOE')
corrplot(cor(data),tl.col="black",method="color",type='lower',shade.lwd = 0.1,tl.cex=0.8,order="hclust")


# ML predictive analysis
#1. Data clean (finished)
#2. Data Characteristics process
#3. Choosing Model 
#4. Scoring Models
#5. Making the prediction 
#6. Model testing(overfitting or not)
##### LR, Random Forst # 

