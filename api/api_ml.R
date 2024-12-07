# Directory where models and metrics are stored
model_dir <- "/Users/asus/Desktop/IFB_project/API"

#* @get /models
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

#* @get /metrics
function(model_name) {
  metrics_file <- file.path(model_dir, paste0(model_name, "_metrics.rds"))
  if (!file.exists(metrics_file)) {
    return(list(error = "Model not found."))
  }
  return(readRDS(metrics_file))
}

#* @post /predict
#* @param model_name The name of the model to use
#* @param new_data The new data for prediction
function(model_name, new_data) {
  # Load the model from the saved directory
  model_file <- file.path(model_dir, paste0(model_name, "_model.rds"))
  if (!file.exists(model_file)) {
    return(list(error = "Model not found."))
  }
  
  # Load the model
  model <- readRDS(model_file)
  
  # Convert new_data from JSON to data frame
  new_data <- as.data.frame(jsonlite::fromJSON(new_data))
  
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




