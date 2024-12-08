library(MASS) 
library(caret)

data <- read.csv("/Users/asus/Desktop/R/Liver_disease_data.csv")

data$Diagnosis <- as.factor(data$Diagnosis)
data$Gender <- as.factor(data$Gender)
data$Smoking <- as.factor(data$Smoking)
data$Diabetes <- as.factor(data$Diabetes)
data$Hypertension <- as.factor(data$Hypertension)

# data into train and test
set.seed(123)
trainIndex <- createDataPartition(data$Diagnosis, p = 0.7, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]


# full model for stepwise regression
full_model <- glm(Diagnosis ~ ., data = train_data, family = binomial)

# stepwise regression
final_model <- stepAIC(full_model, direction = "forward", trace = FALSE)

# evaluate the final model
pred_probs <- predict(final_model, newdata = test_data, type = "response")
pred_class <- ifelse(pred_probs > 0.5, 1, 0)

#metrics
conf_matrix <- table(Predicted = pred_class, Actual = test_data$Diagnosis)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix) * 100

# print everything
print(summary(final_model))
cat(sprintf("\nAccuracy of the final model: %.2f%%\n", accuracy))
conf_matrix <- table(Predicted = pred_class, Actual = test_data$Diagnosis)
print(conf_matrix)


roc_curve <- roc(test_data$Diagnosis, pred_probs)
auc_value <- auc(roc_curve)

print(summary(final_model))
cat(sprintf("\nAccuracy of the final model: %.2f%%\n", accuracy))
cat(sprintf("Precision: %.2f\n", precision))
cat(sprintf("Recall: %.2f\n", recall))
cat(sprintf("F1 Score: %.2f\n", f1_score))
cat(sprintf("AUC: %.2f\n", auc_value))
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

