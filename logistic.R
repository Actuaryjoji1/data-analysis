#Logisitc Regression 
library(ISLR2)
library(MASS)
library(car)
View(Default)

#disable scientific notation
options(scipen=999)

#simple logistic
glm.fit <- glm(default~balance, family=binomial, data=Default)
summary(glm.fit)

#making predictions, solve for $1000
exp(-10.6513306+0.0054989*1000)/(1+exp(-10.6513306+0.0054989*1000))

# solve for $2000
exp(-10.6513306+0.0054989*2000)/(1+exp(-10.6513306+0.0054989*2000))

#simple logistic
glm.fit <- glm(default~student, family=binomial, data=Default)
summary(glm.fit)

#multiple logistic
glm.fit <- glm(default~student+income+balance, family=binomial, data=Default)
summary(glm.fit)

#Stock Market data - Smarket in ISLR
#Dataset contains percentage of returns for S&P 500 over 1250 days
#from 2001 to 2005. For each date we have percentage returns for 5 previous days - Lag1 through
#Lag 5. We also have Volume, percentage return of data in question and Direction (Up or Down)

#descriptives
names(Smarket)
summary(Smarket)
View(Smarket)
dim(Smarket)
plot(Smarket$Volume)
cor(Smarket[,-9])

#fit a logistic model
glm.fit<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fit)

#predict the probability of stock market movement, type = "response" tells R to compute probabilities
glm.probs<-predict(glm.fit,type="response")
glm.probs[1:10]

#confirm manually
View(Smarket)
summary(glm.fit)
exp(-0.126-0.073*.381-0.0423*-0.192+0.011085*-2.624+0.00936*-1.055+0.010313*5.010+0.135441*1.191)
1.02873/(1+1.02873)

#constrasts () indicates that R has created a dummy for up/down
contrasts(Smarket$Direction)

#Get predictions on whether market will go up or down
# First code everything as Down
glm.pred = rep("Down",1250)

# Recode probabilities greater than .5 as up
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Smarket$Direction)
View(glm.pred)

Smarket<-cbind(Smarket,glm.pred)

#Accuracy is True Positive+True Negative/Total Cases
(145+507)/(145+141+457+507)
mean(glm.pred==Smarket$Direction)

#Segregating training and test datasets
train<-(Smarket$Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)

#Fit a logistic model on training dataset
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial, subset=train)
glm.probs<-predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Smarket.2005$Direction)
mean(glm.pred==Smarket.2005$Direction)

#Backward selection-get rid of vars with high p values
glm.fit<-glm(Direction~Lag1+Lag2, data=Smarket, family=binomial, subset=train)
glm.probs<-predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Smarket.2005$Direction)
mean(glm.pred==Smarket.2005$Direction)

#Alternative test train
sample.size <- floor(0.75*nrow(Smarket))
train.index <- sample(seq_len(nrow(Smarket)),size=sample.size)
train <- Smarket[train.index,]
test <- Smarket[-train.index,]

contrasts(test$Direction)
glm.fit <- glm(Direction~Lag1+Lag5,data=test, family=binomial)
glm.probs <- predict(glm.fit,test,type="response")
glm.pred <- rep("Down",313)
glm.pred[glm.probs>.5]="Up"
mean(glm.pred==test$Direction)

