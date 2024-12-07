# Load Plumber
library(plumber)

# Load the model
model <- readRDS("logistic_regression_model.rds")

#* @apiTitle Liver Disease Prediction API
#* @apiDescription This API provides endpoints to predict liver disease based on input features.

#* Predict liver disease
#* @param Age:int Age of the patient (20-80).
#* @param Gender:int Gender of the patient (0 for Male, 1 for Female).
#* @param BMI:float Body Mass Index (15-40).
#* @param AlcoholConsumption:float Weekly alcohol consumption (0-20).
#* @param Smoking:int Smoking status (0 for No, 1 for Yes).
#* @param GeneticRisk:int Genetic risk (0 for Low, 1 for Medium, 2 for High).
#* @param PhysicalActivity:float Hours of physical activity per week (0-10).
#* @param Diabetes:int Diabetes status (0 for No, 1 for Yes).
#* @param Hypertension:int Hypertension status (0 for No, 1 for Yes).
#* @param LiverFunctionTest:float Liver function test result (20-100).
#* @post /predict
function(Age, Gender, BMI, AlcoholConsumption, Smoking, GeneticRisk, 
         PhysicalActivity, Diabetes, Hypertension, LiverFunctionTest) {
  
  # Convert input to a data frame
  input_data <- data.frame(
    Age = as.numeric(Age),
    Gender = as.factor(Gender),
    BMI = as.numeric(BMI),
    AlcoholConsumption = as.numeric(AlcoholConsumption),
    Smoking = as.factor(Smoking),
    GeneticRisk = as.factor(GeneticRisk),
    PhysicalActivity = as.numeric(PhysicalActivity),
    Diabetes = as.factor(Diabetes),
    Hypertension = as.factor(Hypertension),
    LiverFunctionTest = as.numeric(LiverFunctionTest)
  )
  
  # Predict
  prediction <- predict(model, input_data, type = "response")
  predicted_class <- ifelse(prediction > 0.5, 1, 0)
  
  # Return the result
  list(
    Probability = round(prediction, 3),
    PredictedClass = predicted_class
  )
}

