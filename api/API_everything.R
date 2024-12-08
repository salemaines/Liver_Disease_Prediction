### API with everything

library(plumber)
library(caret)
library(pROC)
library(jsonlite)

# Dataset for training
data <- read.csv("/Users/asus/Desktop/R/Liver_disease_data.csv")
data$Diagnosis <- as.factor(data$Diagnosis)
data$Gender <- as.factor(data$Gender)
data$Smoking <- as.factor(data$Smoking)
data$Diabetes <- as.factor(data$Diabetes)
data$Hypertension <- as.factor(data$Hypertension)

# Split data into training and test sets
set.seed(123)
trainIndex <- createDataPartition(data$Diagnosis, p = 0.7, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Initialize model and selected variables
model <- NULL
selected_vars <- NULL

# Directory where models and metrics are stored
model_dir <- "/Users/asus/Desktop/IFB_project/API"

#' @apiTitle Liver Disease Prediction API with ML models and Logistic Regression
#' @apiDescription This API allows training a logistic regression model, using pre-trained ML models to see metrics and make predictions and see EDA.


#* Execute all the available ML models and see its accuracy
#* @get /models
#* @description Execute all available ML models and see their metrics
function() {
  # Load metrics for all models
  models <- list.files(path = model_dir, pattern = "_metrics\\.rds$", full.names = TRUE)
  model_metrics <- lapply(models, function(file) {
    metrics <- readRDS(file)
    list(
      Model = gsub("_metrics\\.rds", "", basename(file)),
      Accuracy = metrics$Accuracy
    )
  })
  return(model_metrics)
}

#* Chose one ML model and see its metrics. Models available: KNN, NaiveBayes, RandomForest, SVM, XGBoost
#* @get /metrics
#* @param model_name The name of the model you want metrics for
#* @description View the metrics of a specific model
function(model_name) {
  metrics_file <- file.path(model_dir, paste0(model_name, "_metrics.rds"))
  if (!file.exists(metrics_file)) {
    return(list(error = "Model not found."))
  }
  return(readRDS(metrics_file))
}

#* See the prediction made by a choosen ML model for a new data. 
#* @post /predict
#* @param model_name The name of the model to use
#* @param new_data JSON data with the new input for prediction. It should be in this format: {   "Age": 25,   "Gender": "0",   "Diabetes": "0",   "PhysicalActivity": 10,   "Hypertension": "0",   "LiverFunctionTest": 77,   "GeneticRisk": 2,   "Smoking": "0",   "BMI": 22.5,   "AlcoholConsumption": 100 }
#* @description See the prediction made by the model for a new data
function(model_name, new_data) {
  # Load the model from the saved directory
  model_file <- file.path(model_dir, paste0(model_name, "_model.rds"))
  if (!file.exists(model_file)) {
    return(list(error = "Model not found."))
  }
  
  # Load the model
  model <- readRDS(model_file)
  
  # Convert new_data from JSON to data frame
  new_data <- as.data.frame(fromJSON(new_data))
  
  # Ensure the data matches the model's expected format (e.g., factors)
  factor_columns <- c("Gender", "Smoking", "Diabetes", "Hypertension")  # Categorical columns
  for (col in factor_columns) {
    if (col %in% names(new_data)) {
      new_data[[col]] <- as.factor(new_data[[col]])  # Convert to factor if not already
    }
  }
  
  # Make prediction using the model
  prediction <- predict(model, newdata = new_data)
  
  return(list(prediction = prediction))
}

#* Train a logistic regression model with the variables chosen and return evaluation metrics of that model
#* @get /train_model
#* @param variables A comma-separated string of variables to use in the logistic regression model (e.g., "Age, Gender, BMI"). The variables available are: "Age", "Gender", "BMI", "AlcoholConsumption" "Smoking", "GeneticRisk", "PhysicalActivity", "Diabetes", "Hypertension", "LiverFunctionTest".
#* @description Train a logistic regression model with the chosen variables and return the evaluation metrics
function(variables = "") {
  if (variables == "") {
    return(list(error = "Please specify variables for the model (example, Age, Gender)."))
  }
  
  # Prepare the formula for logistic regression
  selected_vars <<- strsplit(variables, ",")[[1]]
  selected_vars <<- trimws(selected_vars)  # Remove any extra spaces
  selected_formula <- paste("Diagnosis ~", paste(selected_vars, collapse = " + "))
  
  # Train logistic regression model
  model <<- glm(as.formula(selected_formula), data = train_data, family = binomial)
  
  # Predictions
  pred_probs <- predict(model, newdata = test_data, type = "response")
  pred_class <- ifelse(pred_probs > 0.5, 1, 0)
  
  # Confusion matrix and metrics
  conf_matrix <- table(Predicted = pred_class, Actual = test_data$Diagnosis)
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  precision <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
  recall <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # ROC and AUC
  roc_curve <- roc(test_data$Diagnosis, pred_probs)
  auc_value <- auc(roc_curve)
  
  # Return evaluation metrics
  return(list(
    accuracy = round(accuracy * 100, 2),
    precision = round(precision, 2),
    recall = round(recall, 2),
    f1_score = round(f1_score, 2),
    auc = round(auc_value, 2)
  ))
}

#* Predict using the trained logistic regression model using a new data
#* @get /predict_logreg
#* @param data JSON data with values for the logistic regression model's variables. A comma-separated string with values for the variables used in the model (example: if the variables choosen were Age, Gender and PhysicalActivity, you should put "44,0,50" for example)
#* @description Predict using the trained logistic regression model
function(data = "") {
  if (is.null(model)) {
    return(list(error = "No model has been trained. Please train the model first using /train_model"))
  }
  
  if (data == "") {
    return(list(error = "Please provide values for the variables (example: age, gender)."))
  }
  
  # Ensure the number of values in 'data' matches the number of variables
  input_values <- strsplit(data, ",")[[1]]
  input_values <- trimws(input_values)  # Remove extra spaces
  
  if (length(input_values) != length(selected_vars)) {
    return(list(error = paste("Expected", length(selected_vars), "values, but received", length(input_values), "values.")))
  }
  
  # Create a new data frame for the input values
  new_data <- data.frame(matrix(as.numeric(input_values), nrow = 1))
  colnames(new_data) <- selected_vars
  
  # Ensure categorical columns are factors
  factor_columns <- c("Gender", "Smoking", "Diabetes", "Hypertension")  # List of factor variables
  for (col in factor_columns) {
    if (col %in% names(new_data)) {
      new_data[[col]] <- as.factor(new_data[[col]])  # Convert to factor if not already
    }
  }
  
  # Make prediction
  pred_prob <- predict(model, newdata = new_data, type = "response")
  pred_class <- ifelse(pred_prob > 0.5, 1, 0)
  
  # Return prediction result
  return(list(
    prediction = pred_class,
    probability = pred_prob
  ))
}
