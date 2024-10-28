# Load necessary libraries
library(readxl)
library(ggplot2)
library(tidyverse)
# Read the Excel file
data <- read_excel("D:/Big_Data_Files-2.xlsx", sheet = "TechSales_Reps")
# Ommiting the Sales_Rep variable 
data<-data %>% select(-Sales_Rep)
# Structure of the data set 
str(data)

# Principal Component Analysis (PCA) and Principal Components Regression (PCR):
# a. Performing PCA:
# Load necessary libraries for PCA
library(stats)
# Select numerical columns for PCA
data_num <- data %>% select(Age, Years, Certificates, Feedback, Salary)
# Perform PCA
pca_result <- prcomp(data_num, center = TRUE, scale. = TRUE)

# Display PCA summary
summary(pca_result)
# Display PCA rotation (weights)
pca_result$rotation


# b. Performing PCR:
# Extract the principal components that explain at least 85% of the variance
cumulative_variance <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2))
num_pcs <- which(cumulative_variance >= 0.85)[1]
pc_scores <- as.data.frame(pca_result$x[, 1:num_pcs])

# Add NPS as a column
pc_df <- cbind(pc_scores, NPS = data$NPS)

# Perform linear regression using the principal components as predictors
pcr_model <- lm(NPS ~ ., data = pc_df)
# Display the summary of the regression model
summary(pcr_model)



# c



# d. Analysis of PCA and PCR usefulness:



######################### Question 3
# Predicting High NPS using kNN: a. Using kNN Technique:
# Load necessary libraries for kNN
library(caret)
library(class)

# Create a binary variable for high NPS
data$High_NPS <-factor(ifelse(data$NPS > 7, 1, 0)) 


# Omit NPS from the data
data1<-data %>% select(-NPS)
# Partition the data into training and validation sets
set.seed(1)
trainIndex <- createDataPartition(data1$High_NPS, p = 0.6, list = FALSE)
data_train <- data1[trainIndex, ]
data_valid <- data1[-trainIndex, ]

# Train control for cross-validation
train_control <- trainControl(method = "cv", number = 10)
tune_grid <- expand.grid(k = 1:25)
set.seed(123)
# Train the kNN model
knn_model <- train(High_NPS ~ ., data = data_train, method = "knn", trControl = train_control, tuneGrid = tune_grid)

# Display the results
knn_model


# b. Optimal k value, accuracy, and error rate:
# Extract the optimal k value
optimal_k <- knn_model$bestTune$k
accuracy <- max(knn_model$results$Accuracy)
misclassification_rate <- 1 - accuracy

# Print the results
cat("K =", optimal_k, "\n")
cat("Accuracy =", accuracy, "\n")
cat("Misclassification rate =", misclassification_rate, "\n")

# c. Confusion Matrix for kNN:
# Predict on the validation set
knn_predictions <- predict(knn_model, newdata = data_valid)
# Confusion matrix
confusionMatrix(knn_predictions, data_valid$High_NPS)


# d. Naïve Rule Performance:
# Calculate the naïve rule accuracy
naive_accuracy <- max(table(data_valid$High_NPS)) / nrow(data_valid)

# Print the result
cat("Naïve rule accuracy =", naive_accuracy, "\n")



# e

############### Question 4

# a. Building a Decision Tree:
# Load necessary libraries for decision tree
library(rpart)
library(rpart.plot)
# Omit the High NPs Variable
data2<-data %>% select(-High_NPS)

# Partition the data into training and validation sets
set.seed(1)
trainIndex <- createDataPartition(data2$NPS, p = 0.7, list = FALSE)
data_train <- data2[trainIndex, ]
data_valid <- data2[-trainIndex, ]

# Build a default regression tree
tree_model <- rpart(NPS ~ ., data = data_train, method = "anova")

# Plot the tree
rpart.plot(tree_model)


# b. Number of Leaf Nodes/Splits:
# Display the number of splits
length(tree_model$frame$var) - 1


# c. Important Predictors:
# Display the variable importance
tree_model$variable.importance



# d. Validation Set RMSE and MAPE:
# Predictions on the validation set
tree_predictions <- predict(tree_model, newdata = data_valid)

# Calculate RMSE
rmse <- sqrt(mean((tree_predictions - data_valid$NPS)^2))

# Calculate MAPE
mape <- mean(abs((tree_predictions - data_valid$NPS) / data_valid$NPS)) * 100

# Print the results
cat("RMSE =", rmse, "\n")
cat("MAPE =", mape, "\n")


# e. Building a Full-Grown Tree:
# Build a full-grown regression tree
full_tree_model <- rpart(NPS ~ ., data = data_train, method = "anova", cp = 0)

# Print the complexity parameter table
printcp(full_tree_model)


# f. Finding the Best Pruned Tree:
# Find the best pruned tree
best_tree <- prune(full_tree_model, cp = full_tree_model$cptable[which.min(full_tree_model$cptable[,"xerror"]), "CP"])

# Display the best pruned tree's complexity parameter table
best_tree$cptable

# g. Pruning the Tree:
# Prune the full-grown tree to the best pruned tree
pruned_tree <- prune(full_tree_model, cp = best_tree$cptable[which.min(best_tree$cptable[,"xerror"]), "CP"])

# Display the pruned tree's variable importance
pruned_tree$variable.importance


# h. RMSE and MAPE for Pruned Tree:
# Predictions on the validation set for pruned tree
pruned_tree_predictions <- predict(pruned_tree, newdata = data_valid)

# Calculate RMSE
pruned_rmse <- sqrt(mean((pruned_tree_predictions - data_valid$NPS)^2))

# Calculate MAPE
pruned_mape <- mean(abs((pruned_tree_predictions - data_valid$NPS) / data_valid$NPS)) * 100

# Print the results
cat("RMSE =", pruned_rmse, "\n")
cat("MAPE =", pruned_mape, "\n")




# i. Comparing Trees:



# j. Analysis of Tree Complexity:


# k. Bagging Strategy:
# Load necessary libraries for random forest
library(randomForest)

# Build an ensemble regression tree model using the bagging strategy
bagging_model <- randomForest(NPS ~ ., data = data_train, ntree = 100, mtry = ncol(data_train) - 1)
# Importance plot
varImpPlot(bagging_model)



# l. Validation Set RMSE and MAPE for Bagging:
# Predictions on the validation set for bagging model
bagging_predictions <- predict(bagging_model, newdata = data_valid)

# Calculate RMSE
bagging_rmse <- sqrt(mean((bagging_predictions - data_valid$NPS)^2))

# Calculate MAPE
bagging_mape <- mean(abs((bagging_predictions - data_valid$NPS)) / data_valid$NPS) * 100

# Print the results
cat("RMSE =", bagging_rmse, "\n")
cat("MAPE =", bagging_mape, "\n")



# m. Random Forest Strategy:
# Build an ensemble regression tree model using the random forest strategy
rf_model <- randomForest(NPS ~ ., data = data_train, ntree = 100, mtry = 3)

# Importance plot
varImpPlot(rf_model)


# n. Validation Set RMSE and MAPE for Random Forest:
# Predictions on the validation set for random forest model
rf_predictions <- predict(rf_model, newdata = data_valid)

# Calculate RMSE
rf_rmse <- sqrt(mean((rf_predictions - data_valid$NPS)^2))

# Calculate MAPE
rf_mape <- mean(abs((rf_predictions - data_valid$NPS)) / data_valid$NPS) * 100

# Print the results
cat("RMSE =", rf_rmse, "\n")
cat("MAPE =", rf_mape, "\n")



# o. Comparing Results:


















