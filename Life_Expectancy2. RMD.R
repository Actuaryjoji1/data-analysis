---
  output:
  word_document: default
html_document: default
---
life expectancy is the crucial metric in evaluating population health. The task for this project is to determine what are the various factors that determine life expectancy.

Data source
The data that will be used in the analysis was extracted from Global Health Observatory(GHO) data repository under WHO, and it keeps track of the health status. 

Loading the data 
```{r}
Life_Expectancy_Data<-read.csv("Life_Expectancy_Data.csv")
# The Life expectancy data contain 2938 observations of 22 variables of integer, character and numerical data types.


```
Task 1
```{r}
options(scipen = 999)# Turn off scientic notification such as 1e+48
Lifestyle<-Life_Expectancy_Data$Alcohol*Life_Expectancy_Data$BMI
Economy<-Life_Expectancy_Data$Population*Life_Expectancy_Data$GDP
cbind(Life_Expectancy_Data,Lifestyle,Economy)->Life_Expectancy_Data1

# Death ratio between infant and adult mortality
Death_ratio<-sum(na.omit(Life_Expectancy_Data$Adult.Mortality))/sum(na.omit(Life_Expectancy_Data$infant.deaths))
# This means that in 1 death of infant, there are 5 deaths of adults.


```

Task  2
Data cleaning and scatterplots in Rstudio
```{r}

colSums(is.na(Life_Expectancy_Data1))
# Since the data set contains missing values, we choose to omit them,
Life_Expectancy_Data2<-na.omit(Life_Expectancy_Data1)
attach(Life_Expectancy_Data2)
# Scatter plots between each predictor and target variable
library(ggplot2)
ggplot(data = Life_Expectancy_Data2,aes(Life.expectancy,Lifestyle))+geom_point(col="yellow")+geom_smooth(col="green")+ggtitle(" Relationship between Life.expectancy and Lifestyle")
ggplot(data = Life_Expectancy_Data2,aes(Life.expectancy,Economy))+geom_point(col="purple")+geom_smooth(col="red")+ggtitle(" Relationship between Life.expectancy and Economy")
ggplot(data = Life_Expectancy_Data2,aes(Life.expectancy,Adult.Mortality))+geom_point(col="green")+geom_smooth(col="blue")+ggtitle(" Relationship between Life.expectancy and Adult Mortality")

ggplot(data = Life_Expectancy_Data2,aes(Life.expectancy,percentage.expenditure))+geom_point(col="green")+geom_smooth(col="blue")+ggtitle(" Relationship between Life.expectancy and percentage.expenditure")
ggplot(data = Life_Expectancy_Data2,aes(Life.expectancy,infant.deaths))+geom_point(col="purple")+geom_smooth(col="red")+ggtitle(" Relationship between Life.expectancy and infant deaths")

```

Task 3
Correlation Heat Map
```{r}
# compute the correlation matrix
cor_matrix<-round(cor(na.omit(Life_Expectancy_Data2[,-c(1:3)])))
library(reshape2)
melt<-melt(cor_matrix)
ggplot(data = melt,aes(Var1,Var2,fill=value))+geom_tile() +ggtitle("Heat Map")
```

Task4
```{r}
# Checking and correcting the outliers present in population variable.
ggplot(data = Life_Expectancy_Data2,aes(Population))+geom_boxplot(col="purple")+ggtitle("outliers in Population variable ")
outliers::chisq.out.test(Population)
ggplot(data = Life_Expectancy_Data2,aes(Population))+geom_boxplot(col="green",outlier.shape = NA)+ggtitle("Population variable without outliers ")
# The value above is and outlier and needs to be ommited in our data.

# Checking and correcting the outliers present in Economy variable.
outliers::chisq.out.test(Economy)
ggplot(data = Life_Expectancy_Data2,aes(Economy))+geom_boxplot(col="purple")+ggtitle("outliers in Economy variable ")
# Ommit outlier in data
ggplot(data = Life_Expectancy_Data2,aes(Economy))+geom_boxplot(col="green",outlier.shape = NA)+ggtitle("Ommiting outliers in Econonmy variable ")

# Outliers in life expectancy
ggplot(data = Life_Expectancy_Data2,aes(Life.expectancy))+geom_boxplot(col="purple")+ggtitle("OUTLIERS IN Life Expectancy ")
ggplot(data = Life_Expectancy_Data2,aes(Life.expectancy))+geom_boxplot(outlier.shape=NA,col="purple")+ggtitle("OUTLIERS ABSENT IN Life Expectancy ")
```

Task 5
```{r}
library(caret)

# Relationship between schooling and lifespan of humans
cor(Schooling,Life.expectancy)
ggplot(data = Life_Expectancy_Data2,aes(Schooling,Life.expectancy))+geom_point()+geom_smooth()+ggtitle("Relationship between schooling and lifespan of humans")
# There is a positive relationship between schooling and life expectancy. People who have schooled for many years are expected to have high expectancy levels and vice versa.

# Relationship between alcohol and life expectancy
cor.test(Alcohol,Life.expectancy)
# There is a weak positive linear relationship between alcohol and life expectancy.
ggplot(data = Life_Expectancy_Data2,aes(Alcohol,Life.expectancy))+geom_point()+geom_smooth()+ggtitle("Relationship between alcohol and life expectancy")

ggplot(data = Life_Expectancy_Data2,aes(Total.expenditure,Life.expectancy))+geom_point()+geom_smooth()+ggtitle("Relationship between total expenditure and life expectancy")
cor(Life.expectancy,Total.expenditure)
# Based on the above chart, there is a weak relationship between the 2 variables. therefore total expenditure cannot determine the behavior of life expectancy in different countries.

# Training and testing the data 
library(caTools)
sample<-sample.split(Life_Expectancy_Data2,SplitRatio = 0.75)
train<-Life_Expectancy_Data2[sample==T,]
test<-Life_Expectancy_Data2[sample==F,]
# Model to predict life expectancy
model<-lm(Life.expectancy~., train[,-c(1,2,3)])
summary(model)
# Checking on the models accuracy
# 83.29% of life expectancy is explained by the model.
predicted_values<-predict(model,test)
RMSE(predicted_values,test$Life.expectancy)
plot(test$Life.expectancy,predicted_values,main = "relationship between actual and predicted")
hist(x=model$residuals,main = "histogram of residuals")


# Cross validation in r
train_Control<-trainControl(method = "cv",number = 5)
model1<-train(Life.expectancy~., data=train[,-c(1,2,3)],method="lm",trControl=train_Control)
# This shows the mean of the overal perfomance
print(model1)
```