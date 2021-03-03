library(tidyverse)
library(fastDummies)
library(lubridate)
library(dmm)
library(ROCR)
library(MLmetrics)
library(dplyr)
library(tidyr)
library(caret)
library(funModeling)
library(ROSE)

df <- read.csv("C:\\Users\\aniru\\Desktop\\Courses\\Sem 2\\SYST 568\\H-1B_Disclosure_RAW_Data_FY18.csv")
USADf = subset(df, EMPLOYER_COUNTRY == "UNITED STATES OF AMERICA" & VISA_CLASS == 'H-1B', select=ï..CASE_NUMBER:ORIGINAL_CERT_DATE)
USADf = subset(USADf, select = -c(ï..CASE_NUMBER,EMPLOYER_BUSINESS_DBA,EMPLOYER_COUNTRY,EMPLOYER_ADDRESS,EMPLOYER_CITY,EMPLOYER_COUNTRY,
                                   EMPLOYER_POSTAL_CODE,EMPLOYER_PROVINCE,EMPLOYER_PHONE,EMPLOYER_PHONE_EXT,AGENT_ATTORNEY_NAME,
                                   AGENT_ATTORNEY_CITY,AGENT_ATTORNEY_STATE,SUPPORT_H1B,LABOR_CON_AGREE,PUBLIC_DISCLOSURE_LOCATION,
                                   ORIGINAL_CERT_DATE,PW_SOURCE_YEAR,PW_SOURCE_OTHER,PW_SOURCE,WORKSITE_CITY,WORKSITE_COUNTY,
                                   PW_WAGE_LEVEL,WORKSITE_POSTAL_CODE,WAGE_RATE_OF_PAY_FROM,WAGE_RATE_OF_PAY_TO,VISA_CLASS,
                                   WAGE_UNIT_OF_PAY,SOC_CODE,NAICS_CODE))


#Detecting near zero variance
x = nearZeroVar(USADf, saveMetrics = TRUE)
str(x, vec.len=2)
x[x[,"zeroVar"] > 0, ]
x[x[,"zeroVar"] + x[,"nzv"] > 0, ]


#Include original case_status if needed
#To find the NA's and missing values
summary(USADf)
str(USADf)
USADf[USADf==""]<-NA
sapply(USADf, function(x) sum(is.na(x)))

USADf = na.omit(USADf)
sapply(USADf, function(x) sum(is.na(x)))

head(USADf)
dim(USADf)


#Adding new column Duration of work and dropping employment start and end dates.
USADf$EMPLOYMENT_END_DATE = as.Date(USADf$EMPLOYMENT_END_DATE, "%m/%d/%Y")
USADf$EMPLOYMENT_START_DATE = as.Date(USADf$EMPLOYMENT_START_DATE, "%m/%d/%Y")
USADf$EMPLOYMENT_DURATION <- as.numeric(USADf$EMPLOYMENT_END_DATE-USADf$EMPLOYMENT_START_DATE)
USADf$EMPLOYMENT_DURATION <- round(USADf$EMPLOYMENT_DURATION*0.033)

USADf = subset(USADf, select = -c(EMPLOYMENT_END_DATE,EMPLOYMENT_START_DATE))

#Considering only months
USADf$DECISION_DATE = as.Date(USADf$DECISION_DATE, "%m/%d/%Y")
USADf$CASE_SUBMITTED = as.Date(USADf$CASE_SUBMITTED, "%m/%d/%Y")

USADf$CASE_SUBMITTED <- format(USADf$CASE_SUBMITTED,'%B')
USADf$CASE_SUBMITTED <- as.factor(USADf$CASE_SUBMITTED) 

USADf$DECISION_DATE <- format(USADf$DECISION_DATE,'%B')
USADf$DECISION_DATE <- as.factor(USADf$DECISION_DATE) 

#adding binary Response variable 
USADf$CASE_STATUS_NEW = ifelse(USADf$CASE_STATUS=='CERTIFIED',1,0)
USADf$CASE_STATUS_NEW <- as.factor(USADf$CASE_STATUS_NEW)

USADf = subset(USADf, select = -c(CASE_STATUS)) 

#Converting the pay to anually
PREVAILING_WAGE <- as.numeric(gsub(",", "",as.character(USADf$PREVAILING_WAGE)))
USADf$PREVAILING_WAGE = ifelse(USADf$PW_UNIT_OF_PAY=='Bi-Weekly',USADf$PREVAILING_WAGE <- (PREVAILING_WAGE/2)*52.143,
                               ifelse(USADf$PW_UNIT_OF_PAY=='Hour',USADf$PREVAILING_WAGE <- (PREVAILING_WAGE*40)*52.143,
                                      ifelse(USADf$PW_UNIT_OF_PAY=='Month',USADf$PREVAILING_WAGE <- PREVAILING_WAGE*12,
                                             ifelse(USADf$PW_UNIT_OF_PAY=='Week',USADf$PREVAILING_WAGE <- PREVAILING_WAGE*52.143,
                                                    ifelse(USADf$PW_UNIT_OF_PAY=='Year',USADf$PREVAILING_WAGE <- PREVAILING_WAGE,NA
                                                    )))))
#Dropping PW unit Unit of pay variables
USADf = subset(USADf, select = -c(PW_UNIT_OF_PAY))

summary(USADf)
##Refactor the below columns to 50 levels
x = freq(data=USADf$JOB_TITLE)
job = x$var[1:50]

USADf$JOB_TITLE = ifelse(USADf$JOB_TITLE %in% c(job),as.character(USADf$JOB_TITLE),"OTHERS")
USADf$JOB_TITLE = as.factor(USADf$JOB_TITLE)

y = freq(data=USADf$SOC_NAME)
socname = y$var[1:50]

USADf$SOC_NAME = ifelse(USADf$SOC_NAME %in% c(socname),as.character(USADf$SOC_NAME),"OTHERS")
USADf$SOC_NAME = as.factor(USADf$SOC_NAME)

z = freq(data=USADf$EMPLOYER_NAME)
emp = z$var[1:50]

USADf$EMPLOYER_NAME = ifelse(USADf$EMPLOYER_NAME %in% c(emp),as.character(USADf$EMPLOYER_NAME),"OTHERS")
USADf$EMPLOYER_NAME = as.factor(USADf$EMPLOYER_NAME)

summary(USADf)
str(USADf)
#####################################################################################
#onehot encoding
encode = dummyVars(~CASE_SUBMITTED+DECISION_DATE+EMPLOYER_NAME+EMPLOYER_STATE+JOB_TITLE+SOC_NAME+WORKSITE_STATE+
                     AGENT_REPRESENTING_EMPLOYER+FULL_TIME_POSITION+H1B_DEPENDENT+WILLFUL_VIOLATOR, data = USADf)
USADf_oh = as.data.frame(predict(encode, newdata = USADf))

USADf = subset(USADf, select = -c(CASE_SUBMITTED,DECISION_DATE,EMPLOYER_NAME,EMPLOYER_STATE,JOB_TITLE,SOC_NAME,
                                  WORKSITE_STATE,AGENT_REPRESENTING_EMPLOYER,FULL_TIME_POSITION,H1B_DEPENDENT,
                                  WILLFUL_VIOLATOR))
final <- cbind(USADf,USADf_oh)

final = subset(final, select = -c(WORKSITE_STATE.,EMPLOYER_STATE.,AGENT_REPRESENTING_EMPLOYER.,FULL_TIME_POSITION.,
                                  H1B_DEPENDENT.,WILLFUL_VIOLATOR.))

#Remove spaces from column names
names(final) = make.names(names(final))
dim(final)
summary(final)
head(final)
#####################################################################################################################################
class(final)
summary(final$CASE_STATUS_NEW)

#random subset of around 10000 observations
set.seed(123)
index = sample(nrow(final),nrow(final)*.017)
final_test = final[index,]
summary(final_test$CASE_STATUS_NEW)
dim(final_test)

#Train Test Split for unsampled data
set.seed(123)
trainIndex = sample(nrow(final_test),nrow(final_test)*.8)
X_train = final_test[trainIndex,]
X_test = final_test[-trainIndex,]


#Cross validation
train = trainControl(method="cv", number=5)
lrcv = train(CASE_STATUS_NEW ~.,data=X_train,trControl=train,method="glm")
rfcv = train(CASE_STATUS_NEW ~.,data=X_train,trControl=train,method="rf")
svmcv = train(CASE_STATUS_NEW ~.,data=X_train,trControl=train,method="svmRadial",scale=FALSE)
lrcv
plot(rfcv)
plot(svmcv)

summary(X_train$CASE_STATUS_NEW)
summary(X_test$CASE_STATUS_NEW)
#Over sampling(run the models and capture the details after and before over sampling)
balanced_over = ovun.sample(CASE_STATUS_NEW ~ .,data = X_train, method = "over", N = 15476, seed = 1)$data
table(balanced_over$CASE_STATUS_NEW)
dim(balanced_over)
X_train_sample = balanced_over


#Modelling
#Logistic Regression
logit <- glm(CASE_STATUS_NEW ~., data = X_train, family = binomial)
summary(logit)
predlr_probs = predict(logit, X_test[-10],type ='response')
predlr = ifelse(predlr_probs>0.5,1,0)
confusionMatrix(as.factor(predlr),X_test$CASE_STATUS_NEW)
roc.curve(X_test$CASE_STATUS_NEW, predlr_probs)


logit1 <- glm(CASE_STATUS_NEW ~., data = X_train_sample, family = binomial)
summary(logit1)
predlr1_probs = predict(logit1, X_test[-10],type ='response')
predlr1 = ifelse(predlr1_probs>0.5,1,0)
confusionMatrix(as.factor(predlr1),X_test$CASE_STATUS_NEW)
roc.curve(X_test$CASE_STATUS_NEW, predlr1_probs)

#Modelling in RF (Our aim is to decrease the number of false positives as we dont want to give false hope 
#to a person saying they will get H1B Visa but eventually they won't.)
library(doParallel)
detectCores()
c <- makePSOCKcluster(6)
registerDoParallel(c)
stopCluster(c)


library(randomForest)
rf <- randomForest(CASE_STATUS_NEW ~.,data=X_train)
summary(rf)
pred = predict(rf, X_test[-10])
pred_prob = predict(rf, X_test[-10], type="prob")
confusionMatrix(pred,X_test$CASE_STATUS_NEW)
#a$overall
varImpPlot(rf)
importance(rf)
plot(rf)
#F1_Score(X_test$CASE_STATUS_NEW,pred)
roc.curve(X_test$CASE_STATUS_NEW, pred_prob[,2])

rf1 <- randomForest(CASE_STATUS_NEW ~.,data=X_train_sample)
summary(rf1)
pred1 = predict(rf1, X_test[-10])
pred1_prob = predict(rf1, X_test[-10], type="prob")
confusionMatrix(pred1,X_test$CASE_STATUS_NEW)
varImpPlot(rf1)
importance(rf1)
plot(rf1)
roc.curve(X_test$CASE_STATUS_NEW, pred1_prob[,1])

#SVM
library(e1071)

rocplot=function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)}

svmfit=svm(CASE_STATUS_NEW ~.,data = X_train, kernel = "radial", scale = FALSE, cost= 1, gamma=2,
           decision.values=T) 
summary(svmfit)
ypred=predict(svmfit, X_test[-10])
fitted=attributes(predict(svmfit, X_test[-10],decision.values=TRUE))$decision.values
confusionMatrix(ypred,X_test$CASE_STATUS_NEW)
rocplot(fitted,X_test$CASE_STATUS_NEW,main="Training Data")

svmfit1=svm(CASE_STATUS_NEW ~.,data = X_train_sample, kernel = "radial", scale = FALSE,cost= 1, gamma=2,
            decision.values=T)
summary(svmfit1)
ypred1=predict(svmfit1, X_test[-10])
fitted1=attributes(predict(svmfit1, X_test[-10],decision.values=TRUE))$decision.values
confusionMatrix(ypred1,X_test$CASE_STATUS_NEW)
rocplot(fitted1,X_test$CASE_STATUS_NEW,main="Training Data")


#XGBoost
library(xgboost)
X_train_new = subset(X_train, select = -c(CASE_STATUS_NEW))
X_test_new = subset(X_test, select = -c(CASE_STATUS_NEW))
Y_train = as.numeric(X_train$CASE_STATUS_NEW) - 1
Y_test = X_test$CASE_STATUS_NEW

xgb = xgboost(data = as.matrix(X_train_new),label=as.matrix(Y_train),max.dclassepth=5,eta = 1,
              nthread=6,nrounds=250,objective = "binary:logistic")
xgb_pred_probs = predict(xgb,as.matrix(X_test_new))
xgb_pred = ifelse(xgb_pred_probs > 0.5,1,0)
confusionMatrix(as.factor(xgb_pred), as.factor(Y_test))
roc.curve(Y_test, xgb_pred_probs)


X_train_sample_new = subset(X_train_sample, select = -c(CASE_STATUS_NEW))
Y_train_sample = as.numeric(X_train_sample$CASE_STATUS_NEW) - 1
#X_train_sample_new = subset(X_train_sample, select = -c(CASE_STATUS_NEW))
#Y_train_sample = as.numeric(X_train_sample$CASE_STATUS_NEW) - 1

xgb_sample = xgboost(data = as.matrix(X_train_sample_new),label=as.matrix(Y_train_sample),
                     max.dclassepth=5,eta = 1,nthread=6,nrounds=200,objective = "binary:logistic")
xgb_pred_probs1 = predict(xgb_sample,as.matrix(X_test_new))
xgb_pred1 = ifelse(xgb_pred_probs1 > 0.00001,1,0)
confusionMatrix(as.factor(xgb_pred1), as.factor(Y_test))
roc.curve(Y_test, xgb_pred_probs1)
summary(xgb_pred_probs1)




#Feature selection
set.seed(100)
rPartMod <- train(CASE_STATUS_NEW ~.,data=USADf, method="rpart")
varImp(rPartMod)
