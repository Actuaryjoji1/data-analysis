#############################Question 1
# Impoting the data set and loading neccesary packages.

library(ggplot2)
library(dplyr)
library(readxl)
library(psych)
df<-read_excel("D:/Big_Data_Files.xlsx",sheet = "TechSales_Reps")
# Structure of the data
str(df)


######### Part 2
#Fit 7 simple linear regression models 
# Fit simple linear regression models
model_age <- lm(Salary ~ Age, data = df)
model_female <- lm(Salary ~ Female, data = df)
model_years <- lm(Salary ~ Years, data = df)
model_college <- lm(Salary ~ College, data = df)
model_personality <- lm(Salary ~ Personality, data = df)
model_certificates <- lm(Salary ~ Certficates, data = df)
model_feedback <- lm(Salary ~ Feedback, data = df)

# Summarize the models
summary(model_age)
summary(model_female)
summary(model_years)
summary(model_college)
summary(model_personality)
summary(model_certificates)
summary(model_feedback)


## Part 4
# Identify 4 best simple linear models (4 points)

# Based on Adjusted R2 and Residual Std Error (Se)
# Identify the 4 best models
models <- list(model_age, model_female, model_years, model_college, model_personality, model_certificates, model_feedback)
model_summaries <- lapply(models, summary)
adjusted_r2 <- sapply(model_summaries, function(x) x$adj.r.squared)
se <- sapply(model_summaries, function(x) x$sigma)

# Combine and sort to identify the best models
model_metrics <- data.frame(Adjusted_R2 = adjusted_r2, Se = se)
model_metrics <- model_metrics[order(-model_metrics$Adjusted_R2), ]
best_models <- head(model_metrics, 4)
best_models


## Part 5
# Fit multiple regression model with the best predictors
multi_model <- lm(Salary ~ Certficates + Personality + Feedback + Age, data = df)
summary(multi_model)


### Part 7
# Predict salary for an employee with specified attributes
new_data <- data.frame(Certficates = 5, Personality = "Diplomat", Feedback = 2.25, Age = 35)
predicted_salary <- predict(multi_model, newdata = new_data)
predicted_salary



### Part 8
# Make residuals plot for the multiple regression model
plot(multi_model, which = 1)


### Part 10
# Fit exponential regression model
log_salary_model <- lm(log(Salary) ~ Certficates + Personality + Feedback + Age, data = df)
summary(log_salary_model)


## Part 11
# Make residuals plot for the exponential regression model
plot(log_salary_model, which = 1)


### Part  13: 
#Fit 9 simple logistic regression models (18 points)
# Define NPSclass
df$NPSclass <- ifelse(df$NPS > 7, 1, 0)

# Fit simple logistic regression models
logit_business <- glm(NPSclass ~ Business, family = binomial, data = df)
logit_age <- glm(NPSclass ~ Age, family = binomial, data = df)
logit_female <- glm(NPSclass ~ Female, family = binomial, data = df)
logit_years <- glm(NPSclass ~ Years, family = binomial, data = df)
logit_college <- glm(NPSclass ~ College, family = binomial, data = df)
logit_personality <- glm(NPSclass ~ Personality, family = binomial, data = df)
logit_certificates <- glm(NPSclass ~ Certficates, family = binomial, data = df)
logit_feedback <- glm(NPSclass ~ Feedback, family = binomial, data = df)
logit_salary <- glm(NPSclass ~ Salary, family = binomial, data = df)

# Summarize the models
summary(logit_business)
summary(logit_age)
summary(logit_female)
summary(logit_years)
summary(logit_college)
summary(logit_personality)
summary(logit_certificates)
summary(logit_feedback)
summary(logit_salary)


# Define a function to calculate accuracy
calculate_accuracy <- function(model, data, response_var) {
  predicted_prob <- predict(model, type = "response")
  predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)
  actual_class <- data[[response_var]]
  accuracy <- mean(predicted_class == actual_class)
  return(accuracy)
}

# Calculate accuracy for each logistic regression model
accuracy_business <- calculate_accuracy(logit_business, df, "NPSclass")
accuracy_age <- calculate_accuracy(logit_age, df, "NPSclass")
accuracy_female <- calculate_accuracy(logit_female, df, "NPSclass")
accuracy_years <- calculate_accuracy(logit_years, df, "NPSclass")
accuracy_college <- calculate_accuracy(logit_college, df, "NPSclass")
accuracy_personality <- calculate_accuracy(logit_personality, df, "NPSclass")
accuracy_certificates <- calculate_accuracy(logit_certificates, df, "NPSclass")
accuracy_feedback <- calculate_accuracy(logit_feedback, df, "NPSclass")
accuracy_salary <- calculate_accuracy(logit_salary, df, "NPSclass")

# Print accuracies
accuracy_business
accuracy_age
accuracy_female
accuracy_years
accuracy_college
accuracy_personality
accuracy_certificates
accuracy_feedback
accuracy_salary


## Part 14
# Coefficient interpretation for Certificates
summary(logit_certificates)


## Part 15
# Based on Accuracy and AIC
# Calculate AIC for each logistic regression model
aic_business <- AIC(logit_business)
aic_age <- AIC(logit_age)
aic_female <- AIC(logit_female)
aic_years <- AIC(logit_years)
aic_college <- AIC(logit_college)
aic_personality <- AIC(logit_personality)
aic_certificates <- AIC(logit_certificates)
aic_feedback <- AIC(logit_feedback)
aic_salary <- AIC(logit_salary)

# Combine accuracy and AIC into a data frame
model_metrics <- data.frame(
  Model = c("Business", "Age", "Female", "Years", "College", "Personality", "Certificates", "Feedback", "Salary"),
  Accuracy = c(accuracy_business, accuracy_age, accuracy_female, accuracy_years, accuracy_college, accuracy_personality, accuracy_certificates, accuracy_feedback, accuracy_salary),
  AIC = c(aic_business, aic_age, aic_female, aic_years, aic_college, aic_personality, aic_certificates, aic_feedback, aic_salary)
)
# Sort the models by Accuracy and AIC (lower AIC is better)
sorted_models <- model_metrics[order(-model_metrics$Accuracy, model_metrics$AIC), ]
# Identify the top 5 models
top_5_models <- head(sorted_models, 5)
top_5_models


## Part 16
# Fit multiple logistic regression model with the best predictors
multi_logit_model <- glm(NPSclass ~ Certficates + Feedback + Salary + Years + Personality, family = binomial, data = df)
summary(multi_logit_model)
calculate_accuracy(multi_logit_model, df, "NPSclass")


### Part 18
# Predict NPSClass for an employee with specified attributes
new_data_logit <- data.frame(Certficates = 5, Feedback = 2.25, Salary = 75000, 
                             Years = 10, Personality = "Diplomat")
predicted_npsclass <- predict(multi_logit_model, newdata = new_data_logit, type = "response")
predicted_npsclass
