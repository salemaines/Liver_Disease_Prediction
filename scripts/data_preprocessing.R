library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(car)

# Load dataset
data <- read.csv("/Users/asus/Desktop/Universidade/Liver_disease_data.csv")

# inspect the data
head(data)
str(data)
summary(data)

# check if exists missing values
colSums(is.na(data))

#######################################################
# function to check for outliers in all columns using IQR

detect_outliers <- function(column) {
  # check if the column is numeric and not binary
  if (is.numeric(column) && length(unique(column)) > 2) {
    Q1 <- quantile(column, 0.25, na.rm = TRUE)
    Q3 <- quantile(column, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    outliers <- which(column < lower_bound | column > upper_bound)
    return(outliers)
  } else {
    return(NULL)  # Skip binary or non-numeric columns
  }
}

# apply the function to all columns of a dataset
check_outliers <- function(data) {
  outlier_report <- list()
  
  for (colname in names(data)) {
    outliers <- detect_outliers(data[[colname]])
    if (!is.null(outliers)) {
      outlier_report[[colname]] <- outliers
    }
  }
  
  return(outlier_report)
}

outlier_results <- check_outliers(data)

# print results
for (col in names(outlier_results)) {
  cat("Outliers detected in column:", col, "\n")
  print(outlier_results[[col]])
  cat("\n")
}

######################
# check for duplicates

duplicates <- data[duplicated(data), ]
print(duplicates)

# check for irrelevant or redundant features
low_variance <- nearZeroVar(data, saveMetrics = TRUE)
print(low_variance)

#check for highly correlated features that can lead to multicollinearity 
cor_matrix <- cor(data[, sapply(data, is.numeric)])
high_cor <- findCorrelation(cor_matrix, cutoff = 0.8)
print(high_cor)

#feature importance
data$Diagnosis <- as.factor(data$Diagnosis)
rf_model <- randomForest(Diagnosis ~ ., data = data)
importance(rf_model)

# Plotting feature importance
feature_importance <- importance(rf_model)
feature_importance_df <- data.frame(
  Feature = rownames(feature_importance),
  Importance = feature_importance[, 1]
)

# Sort features by importance
feature_importance_df <- feature_importance_df[order(-feature_importance_df$Importance), ]

# Plot top 10 features
ggplot(feature_importance_df[1:10, ], aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Important Features", x = "Feature", y = "Importance")










