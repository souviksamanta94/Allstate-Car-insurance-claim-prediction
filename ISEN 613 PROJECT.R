library(ISLR)

#Read Data
setwd("C:\\Users\\vasuk\\Documents")
data=read.csv("train_set0506.csv", na.strings = "?")

#Summary
dim(data)
summary(data)
str(data)
sum(is.na(data))

#C_Claim Column Creation
C_Claim=c(rep(0,100000))
C_Claim[data$Claim_Amount>0]=1
summary(C_Claim)
C_CLaim=as.factor(C_Claim)
plot(C_Claim)
sum(C_Claim==1)
data=cbind(data,C_Claim)
summary(data)
data$C_Claim=as.factor(data$C_Claim)
str(data)

#Data Imputation
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#Cat1(Replacing missing values with highest frequency factor)
plot(data$Cat1) #to understand distribution
summary(data$Cat1)
sum(is.na(data$Cat1))
getmode(data$Cat1)
data$Cat1[is.na(data$Cat1)]="B" 
plot(data$Cat1)
summary(data$Cat1)
sum(is.na(data$Cat1))

#Cat2 (Assigned a sperate D class since no similarity with Claim_Amount or any other columns)
plot(data$Cat2)
summary(data$Cat2)
sum(is.na(data$Cat2))
data$Cat2= as.character(data$Cat2)
data$Cat2=ifelse(is.na(data$Cat2),"D",data$Cat2)
data$Cat2= as.factor(data$Cat2) #Conversion to factor
summary(data$Cat2)
plot(data$C_Claim,data$Cat2)

#Cat3 (#Replacing missing values with highest frequency factor)
plot(data$Cat3)
data$Cat3 =as.character(data$Cat3)
getmode(data$Cat3)
data$Cat3[is.na(data$Cat3)] = "A"
data$Cat3=as.factor(data$Cat3)
summary(data$Cat3)
sum(is.na(data$Cat3))

#Cat4 (Assigned a sperate D class because of no pattern observation with respect to other variables)
plot(data$Cat4)
sum(is.na(data$Cat4))
data$Cat4=as.character(data$Cat4)
data$Cat4=ifelse(is.na(data$Cat4),"D",data$Cat4)
data$Cat4=as.factor(data$Cat4)
summary(data$Cat4)
sum(is.na(data$Cat4))

#Cat5 (Assigned a sperate A class based on avegrage similarity in Claim_Amount)
plot(data$Cat5)
sum(is.na(data$Cat5))
data$Cat5=as.character(data$Cat5)
data$Cat5=ifelse(is.na(data$Cat5),"A",data$Cat5)
data$Cat5=as.factor(data$Cat5)
summary(data$Cat5)
sum(is.na(data$Cat5))

#Cat6 based on 1:0 count ratio similarity for all categories
plot(data$Cat6)
sum(is.na(data$Cat6))
data$Cat6[is.na(data$Cat6)] = "E"
summary(data$Cat6)
sum(is.na(data$Cat6))

#Cat7 (Assigned C class category based on per claim average value comparison)
plot(data$Cat7)
sum(is.na(data$Cat7))
data$Cat7[is.na(data$Cat7)]="C"
summary(data$Cat7)
sum(is.na(data$Cat7))

#Cat8 (Replacing missing values with highest frequency factor)
plot(data$Cat8)
sum(is.na(data$Cat8))
data$Cat8 <- as.character(data$Cat8)
getmode(data$Cat8)
data$Cat8[is.na(data$Cat8)] = "A"
data$Cat8 <- as.factor(data$Cat8)
summary(data$Cat8)

#Cat9(No missing values)
#Cat10 (Replacing missing values with highest frequency factor)
plot(data$Cat10)
data$Cat10 <- as.character(data$Cat10)
getmode(data$Cat10)
data$Cat10[is.na(data$Cat10)] = "A"
data$Cat10 <- as.factor(data$Cat10)
summary(data$Cat10)

#Cat11 (Assigning to Class A as the differece between A)
plot(data$Cat11)
summary (data$Cat11)
data$Cat11[is.na(data$Cat11)] = "A"
summary(data$Cat11)

#Ord Cat(Replacing missing values with highest frequency factor)
hist(data$OrdCat)
sum(is.na(data$OrdCat))
getmode(data$OrdCat)
sum(data$OrdCat == "4")
data$OrdCat[is.na(data$OrdCat)] = 4
plot(data$OrdCat)

#Cat12(Replacing missing values with highest frequency factor)
getmode(data$Cat12)
summary(data$Cat12)
data$Cat12[data$Cat12==""]="B"
sum(is.na(data$Cat12))

#Blind Make(Replacing missing values with highest frequency factor)
plot(data$Blind_Make)
summary(data$Blind_Make)
sum(is.na(data$Blind_Make))
getmode(data$Blind_Make)
data$Blind_Make[is.na(data$Blind_Make)]="K"
plot(data$Blind_Make)
summary(data$Blind_Make)
sum(is.na(data$Blind_Make))
data$Blind_Make=as.integer(data$Blind_Make)


#Blind Model(Replacing missing values with highest frequency factor)
plot(data$Blind_Model)
getmode(data$Blind_Model)
data$Blind_Model[is.na(data$Blind_Model)]="K.7"
summary(data$Blind_Model)
plot(data$Blind_Model,data$Claim_Amount)
data$Blind_Model=as.integer(data$Blind_Model)
sum(is.na(data$Blind_Model))

#Blind_Submodel(Replacing missing values with highest frequency factor)
plot(data$Blind_Submodel)
sum(is.na(data$Blind_Submodel))
getmode(data$Blind_Submodel)
data$Blind_Submodel[is.na(data$Blind_Submodel)] = "K.7.3"
data$Blind_Submodel = as.factor(data$Blind_Submodel)
data$Blind_Submodel=as.integer(data$Blind_Model)
str(data$Blind_Submodel)

#data split  in 70:30 ratio
set.seed(1)
train=sample(1:nrow(data), nrow(data)*.70)
data.train=data[train, ]
data.test=data[-train, ]
dim(data.train)

#Correlation Marix for correlation values and assessing dimension reduction variables
v1_8Nv1_4CA=data[,c(22:29,31:35)]
cor(v1_8Nv1_4CA)#Var1-Var 5,Var2-Var4,Var-Var3,Var2-Var6,Var3-Var6 significant correlation

#Logistic Regression with step selection_AIC
#stepwise slection
logistic.fit=glm(C_Claim ~ -X-Household_ID-Claim_Amount, family=binomial, data=data.train)
step.log=step(logistic.fit)
summary(step.log)
logistic.fit=glm(C_Claim ~ Cat3 + Var2 + Var4 + NVVar2 + NVVar3, family=binomial, data=data.train)
summary(logistic.fit)

#Test Prediction
logistic.probs=predict(logistic.fit, data.test,type="response")  #predict of 3% test split data
summary(logistic.probs)
logistic.pred=rep(0, length(logistic.probs)) #Creating vector

#Setting threshold value
#Confusion matrix TP Rate= 35%, Accuracy=73% for adjusted threshold
threshold=seq(from=0.006, to=0.008, by=0.00001)
accuracy=vector()
TP=vector()
FP=vector()
for(i in 1:length(threshold)-1){
  logistic.pred=rep(0,nrow(data.test))
  logistic.pred[logistic.probs>threshold[i]]=1
  if(sum(logistic.pred)>0 & sum(logistic.pred)!=nrow(data.test)){
    cm=table( logistic.pred,data.test$C_Claim)
    accuracy[i]=(cm[1,1]+cm[2,2])/sum(cm)
    TP[i]=cm[2,2]/(cm[2,2]+cm[1,2])
    FP[i]=cm[2,1]/(cm[1,1]+cm[2,1])
  }
}
plot(threshold[1:length(TP)],TP,type="l",ylab="rate",xlab="threshold",col=3)
lines(threshold[1:length(TP)],accuracy,type = "l",col=2)
lines(threshold[1:length(TP)],FP,type = "l",col=4)
abline(h=0.35,col=1)
loc=which(TP-0.35<=0.01)[1]
####Best threshold
TS=threshold[loc]
abline(v=TS)
logistic.pred=rep(0,nrow(data.test))
logistic.pred[logistic.probs>TS]=1
table( logistic.pred,data.test$C_Claim)
accuracy[loc]
TP[loc]

#Training Prediction
trainpred=predict(logistic.fit,data.train,type="response")
predtrain=rep(0, nrow(data.train)) #Creating vector
predtrain[trainpred>0.00754]=1 #Seting threshold value
table(predtrain, data.train$C_Claim) #Confusion matrix TP Rate= 35%, Accuracy=73% for adjusted threshold

#Result Vizualization
library(ROCR) #ROC Curve
logproc=prediction(logistic.probs,data.test$C_Claim)
roc=performance(logproc,'tpr','fpr')
plot(roc, main='ROC Curve',colorize=TRUE) #ROC Curve for analysis
abline(a=0,b=1)
d=density(logistic.probs)
plot(d,main="Kernel Density of C_Claim") #probability density curve for C_Claim
polygon(d, col="red", border="blue")

