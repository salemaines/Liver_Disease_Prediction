# Load necessary libraries
library(plumber)
library(MASS)
library(caret)
library(pROC)

# Define a custom function to train logistic regression and evaluate metrics
train_and_evaluate_model <- function(selected_variables) {
  
  # Load the dataset (adjust the path to your dataset)
  data <- read.csv("/Users/asus/Desktop/R/Liver_disease_data.csv")
  
  # Convert categorical variables to factors
  data$Diagnosis <- as.factor(data$Diagnosis)
  data$Gender <- as.factor(data$Gender)
  data$Smoking <- as.factor(data$Smoking)
  data$Diabetes <- as.factor(data$Diabetes)
  data$Hypertension <- as.factor(data$Hypertension)
  
  # Split data into train and test sets (70% train, 30% test)
  set.seed(123)
  trainIndex <- createDataPartition(data$Diagnosis, p = 0.7, list = FALSE)
  train_data <- data[trainIndex, ]
  test_data <- data[-trainIndex, ]
  
  # Create the formula dynamically from the selected variables
  formula <- as.formula(paste("Diagnosis ~", paste(selected_variables, collapse = " + ")))
  
  # Fit the logistic regression model with the selected variables
  model <- glm(formula, data = train_data, family = binomial)
  
  # Predict on the test set
  pred_probs <- predict(model, newdata = test_data, type = "response")
  pred_class <- ifelse(pred_probs > 0.5, 1, 0)  # Predict classes (0 or 1)
  
  # Confusion matrix
  conf_matrix <- table(Predicted = pred_class, Actual = test_data$Diagnosis)
  
  # Calculate performance metrics
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  precision <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
  recall <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # Calculate ROC and AUC
  roc_curve <- roc(test_data$Diagnosis, pred_probs)
  auc_value <- auc(roc_curve)
  
  # Return model summary and performance metrics
  return(list(
    model_summary = summary(model),
    confusion_matrix = as.table(conf_matrix),
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    f1_score = f1_score,
    auc = auc_value
  ))
}

# Plumber API definition
# Define a GET endpoint to accept the variable names for the model

#* @apiTitle Liver Disease Prediction API
#* @apiDescription This API allows users to train a logistic regression model with custom variables and evaluate its performance.

# Endpoint to get the model and predictions based on selected variables
#* @param variables A comma-separated list of variables to use for the logistic regression model
#* @get /train_model
#* @response 200 Returns the model summary and performance metrics
function(variables) {
  
  # Convert the input string to a vector
  selected_variables <- unlist(strsplit(variables, ","))
  
  # Train the model with the selected variables and return the result
  result <- train_and_evaluate_model(selected_variables)
  
  return(result)
}

# Endpoint to predict on new data
#* @param variables A comma-separated list of variables to use for the logistic regression model
#* @param new_data A JSON object containing the new data to predict on
#* @post /predict
#* @response 200 Returns the prediction for the new data
function(variables, new_data) {
  
  # Convert the input string to a vector for variables
  selected_variables <- unlist(strsplit(variables, ","))
  
  # Convert new_data to a data frame (assuming it's sent as a JSON object)
  new_data <- as.data.frame(fromJSON(new_data))
  
  # Load the dataset again
  data <- read.csv("/Users/asus/Desktop/R/Liver_disease_data.csv")
  data$Diagnosis <- as.factor(data$Diagnosis)
  
  # Create the formula dynamically from the selected variables
  formula <- as.formula(paste("Diagnosis ~", paste(selected_variables, collapse = " + ")))
  
  # Fit the logistic regression model with the selected variables
  model <- glm(formula, data = data, family = binomial)
  
  # Predict on the new data
  pred_probs <- predict(model, newdata = new_data, type = "response")
  pred_class <- ifelse(pred_probs > 0.5, 1, 0)  # Predict classes (0 or 1)
  
  # Return the predicted class (0 or 1)
  return(list(prediction = pred_class))
}

# Run the Plumber API
# This will start the API server
# To run, save this file as api.R and run: plumber::plumb("api.R")$run()

