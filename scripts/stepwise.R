##### STEPWISE METHOD TO SEE ACCURACIES IN DIFFEREN MODELS 

# libraries

library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(car)
library(gridExtra)
library(reshape2)
library(ggcorrplot)
library(pROC)
library(MASS)  # for stepAIC
library(car)   # for vif (variance inflation factor) if needed later
library(pROC)
# data for the model 

data <- read.csv("/Users/asus/Desktop/R/Liver_disease_data.csv")
head(data)
summary(data)

# put categorical variables as factors for R to handle

data$Diagnosis <- as.factor(data$Diagnosis)
data$Gender <- as.factor(data$Gender)
data$Smoking <- as.factor(data$Smoking)
data$Diabetes <- as.factor(data$Diabetes)
data$Hypertension <- as.factor(data$Hypertension)

# divide data into test and train to evaluate the model later on

set.seed(123) # generate reproducible psudorandom numbers 
trainIndex <- createDataPartition(data$Diagnosis, p = 0.7, list = FALSE) # divide 70% data to train and 30% to test
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

try_out_model <- glm(formula = Diagnosis ~ Age + Gender + BMI + AlcoholConsumption + 
                       Smoking + GeneticRisk + PhysicalActivity + LiverFunctionTest, family = binomial, data = train_data)


# Fit the full model (logistic regression with all predictors)
full.model <- glm(Diagnosis ~ ., data = train_data, family = binomial)

# Stepwise regression (both directions: forward and backward)
step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
summary(step.model)

# Show the selected model formula after stepwise selection
cat("Selected Variables after Stepwise Regression:\n")
print(step.model$call)

# Evaluate model performance on the test data
pred_probs <- predict(step.model, newdata = test_data, type = "response")  # predicted probabilities
pred_class <- ifelse(pred_probs > 0.5, 1, 0)  # predicted class (0 or 1)

# Confusion matrix to see accuracy
conf_matrix <- table(Predicted = pred_class, Actual = test_data$Diagnosis)
cat("Confusion Matrix:\n")
print(conf_matrix)

# Calculate accuracy, precision, recall, and F1 score
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
recall <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("\nPerformance Metrics:\n")
cat("Accuracy: ", round(accuracy * 100, 2), "%\n")
cat("Precision: ", round(precision * 100, 2), "%\n")
cat("Recall: ", round(recall * 100, 2), "%\n")
cat("F1 Score: ", round(f1_score * 100, 2), "%\n")

# Cross-validation to assess model's performance (Repeated 10-fold CV)
train.control <- trainControl(method = "cv", number = 10, repeats = 3)  # 3 repeats for robustness
cv_model <- train(Diagnosis ~ ., data = train_data, 
                  method = "glm", family = "binomial", 
                  trControl = train.control)

# Cross-validation results
cat("\nCross-validation Results:\n")
print(cv_model$results)

# ROC Curve and AUC (for performance evaluation)
roc_curve <- roc(test_data$Diagnosis, pred_probs)
cat("\nAUC: ", auc(roc_curve), "\n")
plot(roc_curve, main = "ROC Curve for Logistic Regression Model")







# Fit the full logistic regression model with all predictors
full_model <- glm(Diagnosis ~ ., data = train_data, family = binomial)

# Perform stepwise regression with both directions (forward and backward)
stepwise_model <- step(full_model, direction = "backward", trace = 1)

# View the final model's summary after stepwise selection
summary(stepwise_model)

# Evaluate the model on test data
pred_probs <- predict(backward_model, newdata = test_data, type = "response")  # predicted probabilities
pred_class <- ifelse(pred_probs > 0.5, 1, 0)  # predicted class (0 or 1)

# Confusion matrix
conf_matrix <- table(Predicted = pred_class, Actual = test_data$Diagnosis)
cat("Confusion Matrix:\n")
print(conf_matrix)

# Calculate accuracy, precision, recall, and F1 score
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
recall <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("\nPerformance Metrics:\n")
cat("Accuracy: ", round(accuracy * 100, 2), "%\n")
cat("Precision: ", round(precision * 100, 2), "%\n")
cat("Recall: ", round(recall * 100, 2), "%\n")
cat("F1 Score: ", round(f1_score * 100, 2), "%\n")

# ROC Curve and AUC (for performance evaluation)
library(pROC)
roc_curve <- roc(test_data$Diagnosis, pred_probs)
cat("\nAUC: ", auc(roc_curve), "\n")
plot(roc_curve, main = "ROC Curve for Logistic Regression Model")




##############################################################

# Custom model: Specify the variables manually
selected_variables <- c("AlcoholConsumption", "LiverFunctionTest", "Gender", "Smoking")  # You can customize this

# Create the formula dynamically
formula <- as.formula(paste("Diagnosis ~", paste(selected_variables, collapse = " + ")))

# Fit the logistic regression model with the selected variables
model <- glm(formula, data = train_data, family = binomial)

# View the model summary
summary(model)

# Evaluate the model on the test set
pred_probs <- predict(model, newdata = test_data, type = "response")  # Predicted probabilities
pred_class <- ifelse(pred_probs > 0.5, 1, 0)  # Predicted class (0 or 1)

# Confusion matrix
conf_matrix <- table(Predicted = pred_class, Actual = test_data$Diagnosis)
cat("Confusion Matrix:\n")
print(conf_matrix)

# Calculate accuracy, precision, recall, and F1 score
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
recall <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("\nPerformance Metrics:\n")
cat("Accuracy: ", round(accuracy * 100, 2), "%\n")
cat("Precision: ", round(precision * 100, 2), "%\n")
cat("Recall: ", round(recall * 100, 2), "%\n")
cat("F1 Score: ", round(f1_score * 100, 2), "%\n")

# ROC Curve and AUC (for performance evaluation)
library(pROC)
roc_curve <- roc(test_data$Diagnosis, pred_probs)
cat("\nAUC: ", auc(roc_curve), "\n")
plot(roc_curve, main = "ROC Curve for Logistic Regression Model")

















