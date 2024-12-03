library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(car)
library(gridExtra)

data <- read.csv("/Users/asus/Desktop/IFB project/data/Liver_disease_data.csv")
head(data)
summary(data)

categorical_vars <- c("Gender", "Smoking", "GeneticRisk", "Diabetes", "Hypertension", "Diagnosis")

# Create individual count plots
plot_list <- lapply(categorical_vars, function(var) {
  ggplot(data, aes_string(x = var)) +
    geom_bar(fill = "coral") +
    theme_minimal() +
    labs(title = paste("Count Plot of", var), x = var, y = "Count")
})

# Arrange plots in a grid
grid.arrange(grobs = plot_list, ncol = 2)

continuous_vars <- c("Age", "BMI", "AlcoholConsumption", "PhysicalActivity", "LiverFunctionTest")

# Create individual histograms
hist_list <- lapply(continuous_vars, function(var) {
  ggplot(data, aes_string(x = var)) +
    geom_histogram(binwidth = 5, fill = "coral", color = "black", alpha = 0.7) +
    theme_minimal() +
    labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
    theme(plot.title = element_text(hjust = 0.5))
})

# Arrange histograms in a grid
grid.arrange(grobs = hist_list, ncol = 2)

#Correlation matrix
correlation_matrix <- cor(data[, sapply(data, is.numeric)])

ggcorrplot(
  correlation_matrix,
  type = "full",              
  lab = TRUE,                 # correlation coefficients
  lab_size = 3,               
  tl.cex = 10,                
  colors = c("blue", "white", "red")  
) +
  ggtitle("Correlation Matrix of Variables") +
  theme(plot.title = element_text(hjust = 0.5, size = 14))


# plot each variable against the Diagnostic class

#colors
coral_color <- "#FF7F50"  
light_blue <- "#00BFFF"  

# plotting categorical variables against Diagnosis
for (cat_var in categorical_vars) {
  p <- ggplot(data, aes_string(x = cat_var, fill = "factor(Diagnosis)")) +
    geom_bar(position = "dodge") +
    labs(
      title = paste("Distribution of", cat_var, "by Diagnosis"),
      x = cat_var,
      y = "Count",
      fill = "Diagnosis"
    ) +
    scale_fill_manual(values = c(coral_color, light_blue)) + 
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14))
  print(p)  
}

# plotting continuous variables against Diagnosis using boxplots
for (cont_var in continuous_vars) {
  p <- ggplot(data, aes_string(x = "factor(Diagnosis)", y = cont_var, fill = "factor(Diagnosis)")) +
    geom_boxplot() +
    labs(
      title = paste(cont_var, "Distribution by Diagnosis"),
      x = "Diagnosis",
      y = cont_var,
      fill = "Diagnosis"
    ) +
    scale_fill_manual(values = c(coral_color, light_blue)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14))
  print(p)  
}





