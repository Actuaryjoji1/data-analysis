

#Logisitc Regression 
library(ISLR2)
library(MASS)
library(car)
View(Default)

#disable scientific notation
options(scipen=999)

#simple logistic
glm.fit <- glm(Direction~Volume, family=binomial, data=Weekly)
summary(glm.fit)

#multiple logistic
glm.fit9 <- glm(Direction~Volume+Today, family=binomial, data=Weekly)
summary(glm.fit9)


#descriptives
names(Weekly)
summary(Weekly)
View(Weekly)
dim(Weekly)
plot(Weekly$Volume)
cor(Smarket[,-c(1,9)])

#fit a logistic model
glm.fit8<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly, family=binomial)
summary(glm.fit8)
# Aic of the model is 1500.4
#predict the probability of stock market movement, type = "response" tells R to compute probabilities
glm.probs<-predict(glm.fit8,type="response")
glm.probs[1:10]


#constrasts () indicates that R has created a dummy for up/down
contrasts(Weekly$Direction)

#Get predictions on whether market will go up or down
# First code everything as Down
glm.pred = rep("Down",1250)

# Recode probabilities greater than .5 as up
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Weekly$Direction)
View(glm.pred)

Smarket<-cbind(Smarket,glm.pred)

#Accuracy is True Positive+True Negative/Total Cases
mean(glm.pred==Weekly$Direction)

#Segregating training and test datasets
train<-(Weekly$Year<2009)
weekly.2008=Smarket[!train,]
dim(weekly.2008)

#Fit a logistic model on training dataset
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Weekly,family=binomial, subset=train)
summary(glm.fit)
# The Aic of the model is 381.32
glm.probs<-predict(glm.fit,weekly.2008,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,weekly.2008$Direction)
mean(glm.pred==weekly.2008$Direction)

#Backward selection-get rid of vars with high p values
glm.fit<-glm(Direction~Lag1+Lag2, data=Weekly, family=binomial, subset=train)
glm.probs<-predict(glm.fit,weekly.2008,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,weekly.2008$Direction)
mean(glm.pred==weekly.2008$Direction)

#Alternative test train
sample.size <- floor(0.75*nrow(Weekly))
train.index <- sample(seq_len(nrow(Weekly)),size=sample.size)
train <- Weekly[train.index,]
test <- Weekly[-train.index,]

contrasts(test$Direction)
glm.fit <- glm(Direction~Lag1+Lag5,data=test, family=binomial)
glm.probs <- predict(glm.fit,test,type="response")
glm.pred <- rep("Down",313)
glm.pred[glm.probs>.5]="Up"
mean(glm.pred==test$Direction)

