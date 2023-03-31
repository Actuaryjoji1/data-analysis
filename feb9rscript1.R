#lets reference a few libraries
library(MASS)
library(ISLR2)

dim(Carseats)
names(Carseats)

#simple linear regression
lm.fit1 <- lm(Sales~CompPrice, data=Carseats)
summary(lm.fit1)

#multiple regression, 2 independent variables
lm.fit2 <- lm(Sales~CompPrice+Income, data=Carseats)
summary(lm.fit2)

#multiple regression, include all the regressors
lm.fit3 <- lm(Sales~., data=Carseats)
summary(lm.fit3)
# Our model is good fit. 87.34% of sales is explained by the model
#interaction effects
lm.fit4 <- lm(Sales~CompPrice*Income, data=Carseats)
summary(lm.fit4)
lm.fit4 <- lm(Sales~CompPrice+Income, data=Carseats)

#regression post estimation (looking for nonlinearities & heteroskedasticity)

summary(lm.fit1)
plot(lm.fit1)

#non-linear terms
lm.fit5 <- lm(Sales~Income+I(Income^2), data=Carseats)
summary(lm.fit5)

lm.fit6 <- lm(Sales~poly(Income,5), data=Carseats)
summary(lm.fit6)
plot(lm.fit6)

lm.fit5 <- lm(Sales~log(Income), data = Carseats)
summary(lm.fit5)

hist(Carseats$Income)

#checking for multicolinearity
library(car)
lm.fit7 <- lm(Sales~., data=Carseats)
vif(lm.fit7)

?Carseats
View(Carseats)

#Dummy Independent variables
names(Carseats)
lm.fit9 <- lm(Sales~., data=Carseats)
summary(lm.fit9)

#add to  line 59, interaction of price and age; income and advertising
lm.fit10 <- lm(Sales~.+Price:Age+Income:Advertising, data=Carseats)
summary(lm.fit10)

#test train splits on ceosal data
sample.size <- floor(.80*nrow(Carseats))
train.index <- sample(seq_len(nrow(Carseats)), size=sample.size)
train <- Carseats[train.index,]
test <- Carseats[-train.index,]

#test train simple regression
ceosal.fit1 <- lm(Sales~CompPrice, data=Carseats)
pred1 <- predict(ceosal.fit1, newdata=test)
rmse1 <- sqrt(sum((pred1-test$Sales)^2)/length(test$Sales))
rmse1
#test train multiple regression
ceosal.fit2 <- lm(Sales~.,data=train)
pred2 <- predict(ceosal.fit2,newdata=test)
rmse2 <- sqrt(sum((pred2-test$Sales)^2)/length(test$Sales))

c(rmse.simple=rmse1,rmse.multiple=rmse2,r2.simple=summary(ceosal.fit1)$r.squared,r2.multiple=summary(ceosal.fit2)$r.squared)




