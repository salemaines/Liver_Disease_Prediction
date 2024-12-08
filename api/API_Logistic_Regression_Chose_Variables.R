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

#* Predict using the trained model
#* @param data A comma-separated string with values for the variables used in the model (example: "Age": 25, "Gender": "0", "Diabetes": "0", "PhysicalActivity": 10, "Hypertension": "0", "LiverFunctionTest": 77, "GeneticRisk": 2, "Smoking": "0", "BMI": 22.5, "AlcoholConsumption": 100 ).
#* @get /predict
function(data = "") {
  if (is.null(model)) {
    return(list(error = "No model has been trained. Please train the model first using /train_model"))
  }
  
  if (data == "") {
    return(list(error = "Please provide values for the variables (example: age, gender)."))
  }
  
  # ensure the number of values in 'data' matches the number of variables
  input_values <- strsplit(data, ",")[[1]]
  input_values <- trimws(input_values) # Remove extra spaces
  
  if (length(input_values) != length(selected_vars)) {
    return(list(error = paste("Expected", length(selected_vars), "values, but received", length(input_values), "values.")))
  }
  
  # create a new data frame for the input values
  new_data <- data.frame(matrix(as.numeric(input_values), nrow = 1))
  colnames(new_data) <- selected_vars
  
  # ensure categorical columns are factors
  factor_columns <- c("Gender", "Smoking", "Diabetes", "Hypertension")  # List of factor variables
  for (col in factor_columns) {
    if (col %in% names(new_data)) {
      new_data[[col]] <- as.factor(new_data[[col]])  # Convert to factor if not already
    }
  }
  
  # make prediction
  pred_prob <- predict(model, newdata = new_data, type = "response")
  pred_class <- ifelse(pred_prob > 0.5, 1, 0)
  
  # prediction result
  return(list(
    prediction = pred_class,
    probability = pred_prob
  ))
}


