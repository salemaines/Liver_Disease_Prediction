### DIFFERENT ML MODELS

# Load necessary libraries
library(caret)
library(randomForest)
library(e1071)
library(gbm)
library(xgboost)
library(nnet)

# Set seed for reproducibility
set.seed(42)

# Load the data
data <- read.csv("/Users/asus/Desktop/R/Liver_disease_data.csv")

# Convert categorical variables to factors
data$Diagnosis <- as.factor(data$Diagnosis)
data$Gender <- as.factor(data$Gender)
data$Smoking <- as.factor(data$Smoking)
data$Diabetes <- as.factor(data$Diabetes)
data$Hypertension <- as.factor(data$Hypertension)

levels(data$Diagnosis) <- c("Class0", "Class1")

# Split data into training and testing sets
trainIndex <- createDataPartition(data$Diagnosis, p = 0.7, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

models <- list(
  RandomForest = list(method = "rf", tuneGrid = expand.grid(.mtry = c(2, 3, 4))),
  SVM = list(method = "svmRadial", tuneGrid = expand.grid(.C = c(0.1, 1, 10), .sigma = c(0.01, 0.1))),
  GradientBoosting = list(method = "gbm", tuneGrid = expand.grid(.n.trees = c(50, 100), 
                                                                 .interaction.depth = c(1, 3),
                                                                 .shrinkage = c(0.01, 0.1), 
                                                                 .n.minobsinnode = c(10))),
  XGBoost = list(method = "xgbTree", tuneGrid = expand.grid(.nrounds = c(50, 100), 
                                                            .max_depth = c(3, 5),
                                                            .eta = c(0.01, 0.1), 
                                                            .gamma = c(0), 
                                                            .colsample_bytree = c(0.8), 
                                                            .min_child_weight = c(1),
                                                            .subsample = c(0.8)))
)


# Initialize an empty list to store results
model_results <- list()

# Train and evaluate each model
for (model_name in names(models)) {
  
  cat("\nTraining", model_name, "...\n")
  
  # Extract the method and grid for the current model
  method <- models[[model_name]]$method
  grid <- models[[model_name]]$tuneGrid
  
  # Train the model using caret::train()
  train_control <- trainControl(method = "cv", number = 3, classProbs = TRUE, summaryFunction = twoClassSummary)
  
  model <- train(Diagnosis ~ ., 
                 data = train_data, 
                 method = method, 
                 tuneGrid = grid, 
                 metric = "Accuracy", 
                 trControl = train_control)
  
  # Make predictions on the test data
  predictions <- predict(model, newdata = test_data)
  
  # Compute evaluation metrics
  conf_matrix <- confusionMatrix(predictions, test_data$Diagnosis)
  accuracy <- conf_matrix$overall["Accuracy"]
  precision <- conf_matrix$byClass["Pos Pred Value"]
  recall <- conf_matrix$byClass["Sensitivity"]
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # Store the results
  model_results[[model_name]] <- list(
    Model = model,
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    F1_Score = f1_score,
    ConfusionMatrix = conf_matrix
  )
  
  # Print the results
  cat(sprintf("%s Results:\n", model_name))
  cat(sprintf("  Accuracy: %.2f%%\n", accuracy * 100))
  cat(sprintf("  Precision: %.2f\n", precision))
  cat(sprintf("  Recall: %.2f\n", recall))
  cat(sprintf("  F1 Score: %.2f\n", f1_score))
}

# Identify the best model based on accuracy
best_model_name <- names(model_results)[which.max(sapply(model_results, function(x) x$Accuracy))]
best_model <- model_results[[best_model_name]]

cat("\nBest Model:", best_model_name, "\n")
cat(sprintf("Accuracy: %.2f%%\n", best_model$Accuracy * 100))



# Directory to save the models and metrics
save_dir <- "/Users/asus/Desktop/IFB_project/API"

# Save each model and its metrics
for (model_name in names(model_results)) {
  # Save the model itself
  saveRDS(model_results[[model_name]]$Model, file = file.path(save_dir, paste0(model_name, "_model.rds")))
  
  # Save the metrics separately
  metrics <- list(
    Accuracy = model_results[[model_name]]$Accuracy,
    Precision = model_results[[model_name]]$Precision,
    Recall = model_results[[model_name]]$Recall,
    F1_Score = model_results[[model_name]]$F1_Score
  )
  saveRDS(metrics, file = file.path(save_dir, paste0(model_name, "_metrics.rds")))
}


















