credit_score<-read.csv(file.choose())
# Obtain the structure of the data set.
str(credit_score) # 21 variables of 1000 observations

# summary stats of the data set
summary(credit_score)

# Missing values in the data set
colSums(is.na(credit_score))

# Exclude absolute numeric data set.AGe, credit Duration of credit, mount, 
s<-c(1,2,4,5,7,8,9,10,11,12,13,15,16,17,18,19,20,21)
typeof(s)


credit_score<-credit_score[,s]
# Convert integers into factors
for(i in 1:ncol(credit_score)) credit_score[,i]<-as.factor(credit_score[,i])

# store the results under a new name
credit_score_new<-credit_score
str(credit_score_new)

#################### Training and testing data
# Sample indexes
# used for data partitioning
set.seed(123)
indexes<-sample(1:nrow(credit_score_new),size=0.3*nrow(credit_score_new))

# Split the data set
Credit_train<-credit_score_new[-indexes,]
Credit_test<-credit_score_new[indexes,]



#################################################################### 3 Training the model

########## 1. Logistic Model
set.seed(1)
logistic_Model<-glm(Creditability~., data=Credit_train, family=binomial())

# look in to the model
logistic_Model
predictions_values<-predict(logistic_Model,newdata = Credit_test, type="response")
# Plot the predictions
plot(predictions_values)
predictions<-factor(ifelse(predictions_values>0.5,1,0))

#### Model Performance 1
# Confusion Matrix()
library(class)
library(caret)# confusion Matrix
library(ROCR)# Prediction()
confusionMatrix(predictions,Credit_test$Creditability)


########  Model Performance 2
# Create a performance object
pred<-prediction(predictions_values,Credit_test$Creditability)
roc.perf<-performance(pred,measure = "tpr",x.measure = "fpr")
plot(roc.perf)
abline(a=0,b=1)
# Obtain the area under the curve (auc)
auc.perf<-performance(pred, measure = "auc")
auc.perf@y.values
# Obtain the accuracy
acc.perf<-performance(pred, measure = "acc")
plot(acc.perf)

# Extract the minimum accuracy and the corresponding cut off.
ind<-which.max(slot(acc.perf,"y.values")[[1]])
acc=slot(acc.perf,"y.values")[[1]][ind]
cutoff<-slot(acc.perf,"x.values")[[1]][ind]


# Print the actual acccuracy numbers
print(c(accuracy=acc,cutoff=cutoff))


################### 2. Naive Bayes
library(caret)
library(klaR)
library(gains)
library(pROC)
myCtrl <- trainControl(method="cv", number=10)
set.seed(1)
nb_fit <- train(Creditability ~., data = Credit_train, method = "nb", trControl=myCtrl)
nb_fit
nb_class <- predict(nb_fit, newdata=Credit_test,type="raw")
confusionMatrix(nb_class, Credit_test$Creditability, positive = '1')


######################  3.  Knn
myCtrl <- trainControl(method="cv", number=10)
myGrid <- expand.grid(.k=c(1:10))
set.seed(1)
KNN_fit <- train(Creditability ~., data = Credit_train, method = "knn", trControl=myCtrl, tuneGrid = myGrid)
KNN_fit
KNN_Class <- predict(KNN_fit, newdata = Credit_test)
confusionMatrix(KNN_Class, Credit_test$Creditability, positive = '1')

#################### 4. Support Vector Machine (SVM)
library(e1071)
# Define the control function for cross-validation
myCtrl <- trainControl(method="cv", number=10)
# Train the SVM model
set.seed(1)
SVM_fit <- train(Creditability ~., data = Credit_train, method = "svmRadial", trControl=myCtrl)
SVM_fit
# Predict on the test data
SVM_Class <- predict(SVM_fit, newdata = Credit_test)
# Evaluate performance
confusionMatrix(SVM_Class, Credit_test$Creditability, positive = '1')



################## 5. Decision Tree
# Load necessary library
library(rpart)  # for Decision Tree
# Train the Decision Tree model
set.seed(1)
DT_fit <- train(Creditability ~., data = Credit_train, method = "rpart", trControl=myCtrl)
DT_fit
# Predict on the test data
DT_Class <- predict(DT_fit, newdata = Credit_test)
# Evaluate performance
confusionMatrix(DT_Class, Credit_test$Creditability, positive = '1')




############### 6.Random Forest
# Load necessary library
library(randomForest)  # for Random Forest
# Train the Random Forest model
set.seed(1)
RF_fit <- train(Creditability ~., data = Credit_train, method = "rf", trControl=myCtrl)
RF_fit
# Predict on the test data
RF_Class <- predict(RF_fit, newdata = Credit_test)
# Evaluate performance
confusionMatrix(RF_Class, Credit_test$Creditability, positive = '1')

####################### 7. Neural networks

# Load necessary library
library(nnet)  # for Neural Networks
# Train the Neural Network model
set.seed(1)
NN_fit <- train(Creditability ~., data = Credit_train, method = "nnet", trControl=myCtrl, tuneGrid = expand.grid(size = 5, decay = 0.1))
NN_fit
# Predict on the test data
NN_Class <- predict(NN_fit, newdata = Credit_test)
# Evaluate performance
confusionMatrix(NN_Class, Credit_test$Creditability, positive = '1')


#################8. Boosted Tree
# Load necessary library
library(xgboost)  # for Boosted Trees
df<-read.csv("D:/Tasks/R - programmiong Assignments/Credit Risk Modelling/PD_Data.csv")
indexe<-sample(1:nrow(df),size=0.3*nrow(df))

# Split the data set
train<-df[-indexe,]
test<-df[indexe,]
params <- list(
  objective = "binary:logistic",  # For binary classification
  eval_metric = "logloss"         # Evaluation metric
)
params$eval_metric






############################################# Expected loss

# Default- event that the borrower will default during the payment of the loan. 
# Probability of Default=PD
# Exposure at default- total value the bank is exposed to at the time of default
# Loss Given Default(LGD)- losses inccured by the bank when an obligor defaults

#Expected loss= probability of default * Exposure at default * Loss given default

loandata<-read.csv("D:/Tasks/R - programmiong Assignments/Credit Risk Modelling/loan_data_PD.csv",stringsAsFactors = FALSE)

# Analyse the structure of the data set
str(loandata)
# Unique values for loan statuses
unique(loandata$loan_status)
table(loandata$loan_status)

# We should consider 2 factors defaults and fully paid loans.
loandata<-subset(loandata,loan_status!='In Grace Period')
loandata<-subset(loandata,loan_status!='Late (16-30 days)')
loandata<-subset(loandata,loan_status!='Current')

# Omit missing observations from the loan status
loandata<-subset(loandata,loan_status!='')
library(dplyr)
loandata %>% group_by(loan_status) %>% count()
loandata %>% group_by(loan_status) %>% summarise(count=n())
# The goal is to simplify the loan status into  categories, default and no default
library(stringr)
loandata$loan_status<-ifelse(str_detect(loandata$loan_status,"Paid"),loandata$loan_status,"Default")
loandata %>% group_by(loan_status) %>% count()


# Plot loan status
library(ggplot2)
g<-ggplot(data=loandata,aes(loan_status,fill=loan_status))+geom_bar()
g
g1<-ggplot(data=loandata,aes(grade,fill=loan_status))+geom_bar()
g1
# Default rate for each Loan Grade
# grade 1== 
grade1<-loandata %>% filter(loan_status=="Default") %>% group_by(grade) %>% summarise(default_count=n())
grade1

# calculate the default rate in each grade
grade2<-loandata %>% group_by(grade) %>% summarise(count=n())
grade2

grade3<-grade2 %>% left_join(grade1) %>% mutate(default_rate=100*default_count/count)%>% select(grade,count,default_count,default_rate)
grade3
ggplot(grade3, aes(x=grade,y=default_rate,fill=grade))+geom_bar(stat="identity")+ggtitle("Default rate for all the grades")
# Loan Grade vs Interest Rate
loandata$int_rate<-(as.numeric(gsub(pattern="%",replacement = "",x=loandata$int_rate)))


# Group data based on grade and theor mean interest rate
x12<-loandata %>% filter(loan_status=='Default') %>% group_by(grade) %>% summarise(int_rate=mean(int_rate))
ggplot(x12,aes(x=grade,y=int_rate,fill=grade))+geom_bar(stat = "identity",position = "dodge")


######################### R packages

# dplyr - group_by(, summarise(, mutate, filter, select and arrange)
library(dplyr)
# stringr- Character Manipulation, Pattern matching function. 
library(stringr)
# ggplot2 for data visualization
library(ggplot2)
library(tidyverse)
# Caret- classification and regression training
library(caret)
# lubridate- working with dates
library(lubridate)
# corrplot
library(corrplot)

# Rms-  Regression Modelling strategies

# doMc
library(doMC)
# proc package
library(pROC)
# e1071
library(e1071)
# Kernlab
library(kernlab)
# Xgboost
library(xgboost)




#########################################3 Training and testing the data.
# Sample index
set.seed(123)
indexes=sample(1:nrow(loandata),0.3*nrow(loandata))
data_train<-loandata[-indexes,]
data_test<-loandata[indexes,]


# cleaning the Dataset
# discard Attributes that are irrelevant in our data
#discard_column<-c("collection_recovery_fee","emp_title","funded_amnt_inv","id","installment","last_credit_pull_d","last_fico_range_high","last_fico_range_low","last_pymt_amnt","last_paymt_d",
 # "loan_amt","member_id","next_pymnt_d","num_tl_120dpd_2m","num_tl_30dpd","out_prncp","out_prncp_inv","recoveries",
  #"total_pymnt","total_pymnt_inv","total_rec_int","total_rec_late_fee","total_rec_prncp","url","zip_code")
discard_column =c(
  "collection_recovery_fee",
  "emp_title",
  "funded_amnt_inv",
  "id",
  "installment",
  "last_credit_pull_d",
  "last_fico_range_high",
  "last_fico_range_low",
  "last_pymnt_amnt",
  "last_pymnt_d",
  "loan_amnt",
  "member_id",
  "next_pymnt_d",
  "num_tl_120dpd_2m",
  "num_tl_30dpd",
  "out_prncp",
  "out_prncp_inv",
  "recoveries",
  "total_pymnt",
  "total_pymnt_inv",
  "total_rec_int",
  "total_rec_late_fee",
  "total_rec_prncp",
  "url",
  "zip_code"
)

data_train<-data_train[,!names(data_train)  %in% discard_column]
dim(data_train)


# Discared the grade attribute since the information is available in the subgrade column
data_train$grade<-NULL
# DIscard the columns with Many NA's
data_train<-data_train[,-which(colMeans(is.na(data_train))>0.5)]

## Missing Variable Imputation - using the mean.
discard_column = c(
  "hardship_flag",
  "hardship_type",
  "hardship_reason",
  "hardship_status",
  "hardship_start_date",
  "hardship_end_date",
  "payment_plan_start_date",
  "hardship_loan_status",
  "disbursement_method",
  "debt_settlement_flag",
  "debt_settlement_flag_date",
  "settlement_status",
  "settlement_date"
)

data_train = data_train[, !(names(data_train) %in% discard_column)]

# Convert revol_util to numeric data type
data_train$revol_util<-(as.numeric(gsub(pattern= "%", replacement="",x=data_train$int_rate)))


# Earliest_cr_line is transformed to the number of days before the loan is issued
data_train$earliest_cr_line = parse_date_time(str_c("01", data_train$issue_d), "dmy") - 
  parse_date_time(str_c("01", data_train$earliest_cr_line), "dmy")

data_train$earliest_cr_line = as.numeric(data_train$earliest_cr_line, units = "days")

data_train$issue_m<-sapply(data_train$issue_d,function(x){str_split(x,"-")[[1]][1]})
# Step 1: Filter rows where loan_status is "Default" and group by issue_m, then count defaults
tmp <- data_train %>%
  filter(loan_status == "Default") %>%
  group_by(issue_m) %>%
  summarise(default_count = n())

# Step 2: Group the entire dataset by issue_m and count total loans
tmp2 <- data_train %>%
  group_by(issue_m) %>%
  summarise(count = n())

# Step 3: Join the two datasets on issue_m and calculate the default rate
tmp2 <- tmp2 %>%
  left_join(tmp, by = "issue_m") %>%
  mutate(default_rate = default_count / count)
tmp2

# We can drop the default rate that doesn't vary much by the month that is issued.
data_train$issue_d<-NULL
data_train$issue_m<-NULL


############################# Attribues with Zero Variance.- they have lower predictor power

#define generic functions
# return the charcater columns from the data set
getNumericColumns<-function(t){
  tn=sapply(t,function(x){is.numeric(x)})
  return(names(tn)[which(tn)])
}

# Return the charactercolumns in the data set

getCharacterColumns<-function(t){
  tn=sapply(t,function(x){is.character(x)})
  return(names(tn)[which(tn)])
}

# Return the Factorcolumns in the data set

getFactorColumns<-function(t){
  tn=sapply(t,function(x){is.factor(x)})
  return(names(tn)[which(tn)])
}

#Return the index of columns along with the column Names
getIndexsOfColumns<-function(t,column_names){
  return(match(column_names,colnames(t)))
}


### Find caharcter columns with the same value and numeric column swith zero values

tmp<-apply(data_train[getCharacterColumns(data_train)],2,function(x){length(unique(x))})
tmp<-tmp[tmp==1]
tmp2<-apply(data_train[getNumericColumns(data_train)],2,function(x){(sd(x))})
tmp2<-tmp[tmp2==0]

discard_column<-c(names(tmp),names(tmp2))
discard_column

# Based on my calculations no obsercation found
data_train<-data_train[,!(names(data_train) %in% discard_column)]

# Attribute title and purpose
table(data_train$purpose)
table(data_train$title)
