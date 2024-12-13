# Load necessary libraries
library(caret)
library(randomForest)
library(e1071)
library(gbm)
library(xgboost)
library(nnet)
library(naivebayes)

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
  XGBoost = list(method = "xgbTree", 
                 tuneGrid = expand.grid(
                   nrounds = c(50, 100),         # number of boosting rounds
                   max_depth = c(3, 5),          # max depth of the trees
                   eta = c(0.01, 0.1),           # learning rate
                   gamma = c(0),                 # minimum loss reduction
                   colsample_bytree = c(0.8),    # feature subsample
                   min_child_weight = c(1),      # minimum child weight
                   subsample = c(0.8)
                 )),
  KNN = list(method = "knn", tuneGrid = expand.grid(k = c(3, 5, 7))),
  NaiveBayes = list(method = "naive_bayes")
)

model_results <- list()


for (model_name in names(models)) {
  
  cat("\nTraining", model_name, "...\n")
  
  # Extract the method and grid for the current model
  method <- models[[model_name]]$method
  grid <- models[[model_name]]$tuneGrid
  
  # Train the model using caret::train()
  train_control <- trainControl(method = "cv", number = 3, classProbs = TRUE, summaryFunction = defaultSummary, verboseIter = FALSE)
  
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
  
  # store the results
  model_results[[model_name]] <- list(
    Model = model,
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    F1_Score = f1_score,
    ConfusionMatrix = conf_matrix
  )
  
  # Print only the final evaluation metrics for each model
  cat(sprintf("%s Results:\n", model_name))
  print(conf_matrix)
  cat(sprintf("  Accuracy: %.2f%%\n", accuracy * 100))
  cat(sprintf("  Precision: %.2f\n", precision))
  cat(sprintf("  Recall: %.2f\n", recall))
  cat(sprintf("  F1 Score: %.2f\n", f1_score))
}


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




















