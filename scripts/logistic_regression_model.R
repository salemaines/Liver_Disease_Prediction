library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(car)
library(gridExtra)
library(reshape2)
library(ggcorrplot)
library(pROC)

data <- read.csv("/Users/asus/Desktop/R/Liver_disease_data.csv")
head(data)
summary(data)


data$Diagnosis <- as.factor(data$Diagnosis)
data$Gender <- as.factor(data$Gender)
data$Smoking <- as.factor(data$Smoking)
data$Diabetes <- as.factor(data$Diabetes)
data$Hypertension <- as.factor(data$Hypertension)

set.seed(123) # generate reproducible psudorandom numbers 
trainIndex <- createDataPartition(data$Diagnosis, p = 0.7, list = FALSE) # divide 70% data to train and 30% to test
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

model <- glm(Diagnosis ~ ., data = train_data, family = binomial)
summary(model)


pred_probs <- predict(model, newdata = test_data, type = "response")

# Convert probabilities to class labels (0 or 1)
pred_class <- ifelse(pred_probs > 0.5, 1, 0)

conf_matrix <- table(Predicted = pred_class, Actual = test_data$Diagnosis)
print(conf_matrix)

precision <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
recall <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
f1_score <- 2 * (precision * recall) / (precision + recall)

print(paste("Precision: ", precision))
print(paste("Recall: ", recall))
print(paste("F1 Score: ", f1_score))

library(pROC)
roc_curve <- roc(test_data$Diagnosis, pred_probs)
plot(roc_curve, col = "blue", main = "ROC Curve")
auc_value <- auc(roc_curve)
print(paste("AUC: ", auc_value))


cooks_distance <- cooks.distance(model)
plot(cooks_distance, main = "Cook's Distance")
abline(h = 4 / nrow(train_data), col = "red", lty = 2)

# Null model (only the intercept)
null_model <- glm(Diagnosis ~ 1, data = train_data, family = binomial)

# Full model with all predictors
full_model <- glm(Diagnosis ~ ., data = train_data, family = binomial)

# Forward selection using AIC as the criterion
stepwise_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")

# Summary of the final model
summary(stepwise_model)


# Custom forward selection with performance metrics (fix for multiple rows)
forward_selection_with_verbose_metrics <- function(train_data, test_data, target_var) {
  # Null model (intercept only)
  null_model <- glm(as.formula(paste(target_var, "~ 1")), data = train_data, family = binomial)
  
  # Full model with all predictors
  predictors <- setdiff(names(train_data), target_var) # Get all predictors
  
  # Initialize variables
  current_model <- null_model
  stepwise_results <- data.frame(Step = integer(), Variables = character(), 
                                 AIC = numeric(), Accuracy = numeric(), 
                                 Precision = numeric(), Recall = numeric(), F1 = numeric())
  
  step <- 1
  while (TRUE) {
    # Get remaining predictors
    remaining_predictors <- setdiff(predictors, names(coef(current_model))[-1])
    
    # If no predictors remain, stop
    if (length(remaining_predictors) == 0) break
    
    # Test adding each remaining predictor
    aic_scores <- sapply(remaining_predictors, function(pred) {
      formula <- as.formula(paste(target_var, "~", paste(c(names(coef(current_model))[-1], pred), collapse = " + ")))
      model <- glm(formula, data = train_data, family = binomial)
      AIC(model)
    })
    
    # Select the predictor with the lowest AIC
    best_predictor <- names(aic_scores)[which.min(aic_scores)]
    best_aic <- min(aic_scores)
    
    # Update the model
    new_formula <- as.formula(paste(target_var, "~", paste(c(names(coef(current_model))[-1], best_predictor), collapse = " + ")))
    new_model <- glm(new_formula, data = train_data, family = binomial)
    current_model <- new_model
    
    # Predict on test data
    pred_probs <- predict(current_model, newdata = test_data, type = "response")
    pred_class <- ifelse(pred_probs > 0.5, 1, 0)
    
    # Confusion matrix
    conf_matrix <- table(Predicted = pred_class, Actual = test_data[[target_var]])
    
    # Calculate metrics
    accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
    precision <- ifelse(ncol(conf_matrix) > 1 && sum(conf_matrix[2, ]) > 0, 
                        conf_matrix[2, 2] / sum(conf_matrix[2, ]), 0)
    recall <- ifelse(nrow(conf_matrix) > 1 && sum(conf_matrix[, 2]) > 0, 
                     conf_matrix[2, 2] / sum(conf_matrix[, 2]), 0)
    f1_score <- ifelse((precision + recall) > 0, 
                       2 * (precision * recall) / (precision + recall), 0)
    
    # Print step results for each model
    cat("\nStep:", step, "\n")
    cat("Variables in Model:", paste(names(coef(current_model))[-1], collapse = ", "), "\n")
    cat("AIC:", best_aic, "\n")
    cat("Accuracy:", round(accuracy * 100, 2), "%\n")
    cat("Precision:", round(precision * 100, 2), "%\n")
    cat("Recall:", round(recall * 100, 2), "%\n")
    cat("F1 Score:", round(f1_score * 100, 2), "%\n")
    
    # Store results
    stepwise_results <- rbind(stepwise_results, data.frame(
      Step = step,
      Variables = paste(names(coef(current_model))[-1], collapse = ", "),
      AIC = best_aic,
      Accuracy = accuracy,
      Precision = precision,
      Recall = recall,
      F1 = f1_score
    ))
    
    # Increment step
    step <- step + 1
    
    # Stop if no further improvement in AIC or if we have added all predictors
    if (best_aic >= AIC(current_model)) break
  }
  
  # Return final model and results
  list(FinalModel = current_model, StepwiseResults = stepwise_results)
}

# Usage
results <- forward_selection_with_verbose_metrics(train_data, test_data, "Diagnosis")
final_model <- results$FinalModel
stepwise_results <- results$StepwiseResults

# Print final stepwise results in tabular format
print(stepwise_results)

# Summary of the final model
summary(final_model)

saveRDS(final_model, "/Users/asus/Desktop/IFB_project/api/logistic_regression_model.rds")

