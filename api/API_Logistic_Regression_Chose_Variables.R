library(plumber)
library(ggplot2)
library(dplyr)
library(caret)
library(pROC)

# dataset
data <- read.csv("/Users/asus/Desktop/R/Liver_disease_data.csv")
data$Diagnosis <- as.factor(data$Diagnosis)
data$Gender <- as.factor(data$Gender)
data$Smoking <- as.factor(data$Smoking)
data$Diabetes <- as.factor(data$Diabetes)
data$Hypertension <- as.factor(data$Hypertension)

# split data test and train
set.seed(123)
trainIndex <- createDataPartition(data$Diagnosis, p = 0.7, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# no variables to initialize
model <- NULL
selected_vars <- NULL

#' @apiTitle Liver Disease Prediction API
#' @apiDescription This API allows training a logistic regression model with chosen variables, viewing model metrics, and making predictions based on new input data.

#* Train a logistic regression model with the variables chosen and return evaluation metrics of that model
#* @param variables A comma-separated string of variables to use in the model (example: "Age,Gender,AlcoholConsumption") The variables available are: "Age", "Gender", "BMI", "AlcoholConsumption" "Smoking", "GeneticRisk", "PhysicalActivity", "Diabetes", "Hypertension", "LiverFunctionTest".
#* @get /train_model
function(variables = "") {
  if (variables == "") {
    return(list(error = "Please specify variables for the model (example, Age, Gender)."))
  }
  
  # Prepare the formula for logistic regression
  selected_vars <<- strsplit(variables, ",")[[1]]
  selected_vars <<- trimws(selected_vars) # Remove any extra spaces
  selected_formula <- paste("Diagnosis ~", paste(selected_vars, collapse = " + "))
  
  # logistic regression model
  model <<- glm(as.formula(selected_formula), data = train_data, family = binomial)
  
  # predictions
  pred_probs <- predict(model, newdata = test_data, type = "response")
  pred_class <- ifelse(pred_probs > 0.5, 1, 0)
  
  # confusion matrix and metrics
  conf_matrix <- table(Predicted = pred_class, Actual = test_data$Diagnosis)
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  precision <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
  recall <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # ROC and AUC
  roc_curve <- roc(test_data$Diagnosis, pred_probs)
  auc_value <- auc(roc_curve)
  
  # evaluation metrics
  return(list(
    accuracy = round(accuracy * 100, 2),
    precision = round(precision, 2),
    recall = round(recall, 2),
    f1_score = round(f1_score, 2),
    auc = round(auc_value, 2)
  ))
}

#* Predict using the trained logistic regression model using new data
#* @post /predict_logreg
#* @param data JSON data with values for the logistic regression model's variables (example: {"Age": 44, "Gender": 0, "PhysicalActivity": 50}).
#* @description Predict using the trained logistic regression model
function(data) {
  if (is.null(data)) {
    return(list(error = "Please provide values for the variables in JSON format."))
  }
  
  # Parse JSON input
  input_data <- tryCatch(jsonlite::fromJSON(data), 
                         error = function(e) {
                           return(list(error = paste("JSON parsing error:", e$message)))
                         })
  
  # Check for parsing errors
  if (is.list(input_data) && "error" %in% names(input_data)) {
    return(input_data)
  }
  
  # Convert input to data frame
  new_data <- as.data.frame(input_data)
  
  # Ensure all columns are correctly typed
  numeric_vars <- c("Age", "BMI", "AlcoholConsumption", "PhysicalActivity")
  factor_vars <- c("Gender", "Smoking", "Diabetes", "Hypertension")
  
  for (var in numeric_vars) {
    if (var %in% names(new_data)) {
      new_data[[var]] <- as.numeric(new_data[[var]])
    }
  }
  for (var in factor_vars) {
    if (var %in% names(new_data)) {
      new_data[[var]] <- as.factor(new_data[[var]])
    }
  }
  
  # Remaining logic for prediction
  pred_prob <- predict(model, newdata = new_data, type = "response")
  pred_class <- ifelse(pred_prob > 0.5, 1, 0)
  
  return(list(
    prediction = pred_class,
    probability = pred_prob
  ))
}


