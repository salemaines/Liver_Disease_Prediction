library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(car)
library(gridExtra)
library(reshape2)
library(ggcorrplot)
library(pROC)
library(pROC)

data <- read.csv("/Users/asus/Desktop/R/Liver_disease_data.csv")
head(data)
summary(data)

# transform categorical variables into factors so R can handle them

data$Diagnosis <- as.factor(data$Diagnosis)
data$Gender <- as.factor(data$Gender)
data$Smoking <- as.factor(data$Smoking)
data$Diabetes <- as.factor(data$Diabetes)
data$Hypertension <- as.factor(data$Hypertension)

# data for train and for test

set.seed(123) # generate reproducible psudorandom numbers 
trainIndex <- createDataPartition(data$Diagnosis, p = 0.7, list = FALSE) # divide 70% data to train and 30% to test
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]


# available variables for modeling

cat("Available variables for modeling:\n")
all_vars <- colnames(train_data)
all_vars <- all_vars[all_vars != "Diagnosis"] # exclude the clas
print(all_vars)

# select variables for the model

cat("\nEnter the variables you want to include in the model, separated by commas (e.g., Age, BMI, Gender):\n")
selected_vars <- scan(what = "character", sep = ",")
selected_formula <- paste("Diagnosis ~", paste(selected_vars, collapse = " + "))

# Build the logistic regression model with selected variables
cat("\nBuilding model with selected variables:", selected_formula, "\n")
model <- glm(as.formula(selected_formula), data = train_data, family = binomial)
summary(model)


pred_probs <- predict(model, newdata = test_data, type = "response")
pred_class <- ifelse(pred_probs > 0.5, 1, 0)

# confusion matrix

conf_matrix <- table(Predicted = pred_class, Actual = test_data$Diagnosis)
print(conf_matrix)

# evaluation metrics

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy: ", round(accuracy * 100, 2), "%"))

precision <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
recall <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
f1_score <- 2 * (precision * recall) / (precision + recall)

print(paste("Precision: ", precision))
print(paste("Recall: ", recall))
print(paste("F1 Score: ", f1_score))

# ROC curve and AUC value

roc_curve <- roc(test_data$Diagnosis, pred_probs)
plot(roc_curve, col = "blue", main = "ROC Curve")
auc_value <- auc(roc_curve)
print(paste("AUC: ", auc_value))


# cooks distance

cooks_distance <- cooks.distance(model)
plot(cooks_distance, main = "Cook's Distance")
abline(h = 4 / nrow(train_data), col = "red", lty = 2)

save_path <- "/Users/asus/Desktop/IFB_project/API"

# Save the model to the specified path
model_file <- file.path(save_path, "liver_model.rds")
saveRDS(model, file = model_file)

