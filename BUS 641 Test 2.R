# Loading the packages 

library(ggplot2)
library(dplyr)
library(readxl)
library(stargazer)


test2_data<-read_excel("D:/data/Big_Data_Files.xlsx",sheet = "TechSales_Reps")
# first 6 rows of the data
head (test2_data)

# Part A
## Question 2
#Fit 7 simple linear regression models 

age_model <- lm(Salary ~ Age, data = test2_data)
summary(age_model)
female_model <- lm(Salary ~ Female, data = test2_data)
summary(female_model)
years_model <- lm(Salary ~ Years, data = test2_data)
summary(years_model)
college_model <- lm(Salary ~ College, data = test2_data)
summary(college_model)
personality_model <- lm(Salary ~ Personality, data = test2_data)
summary(personality_model)
certificates_model <- lm(Salary ~ Certficates, data = test2_data)
summary(certificates_model)
feedback_model <- lm(Salary ~ Feedback, data = test2_data)
summary(feedback_model)

## Question 4
models <- list(age_model, female_model, years_model, college_model, personality_model, certificates_model,feedback_model)
summary_models <- lapply(models, summary)
adj_r2 <- sapply(summary_models, function(x) x$adj.r.squared)
se <- sapply(summary_models, function(x) x$sigma)

model_adj_r2_se <- data.frame(Adjusted_R2 = adj_r2, Se = se)
model_adj_r2_se<- model_adj_r2_se[order(-model_adj_r2_se$Adjusted_R2), ]
best_models <- head(model_adj_r2_se, 4)
best_models


## Q 5
# Fit multiple regression model with the best predictors
multi_model <- lm(Salary ~ Certficates + Personality + Feedback + Age, data = test2_data)
summary(multi_model)


### Q 7
# Predict salary for an employee with specified attributes
new_data <- data.frame(Certficates = 5, Personality = "Diplomat", Feedback = 2.25, Age = 35)
predicted_salary <- predict(multi_model, newdata = new_data)
predicted_salary



###Q 8
# Make residuals plot for the multiple regression model
plot(multi_model, which = 1)
