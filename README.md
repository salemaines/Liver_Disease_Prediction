# Liver Disease Prediction Project

This project aims to predict liver disease based on a dataset containing various health-related variables using logistic regression and machine learning models.

## Project Overview

The project analyzes a dataset to predict liver disease using machine learning models. The dataset contains several health parameters like age, gender, BMI, alcohol consumption, smoking status, and liver function tests. Multiple models are created and evaluated, including logistic regression and other machine learning classifiers. We created a logistic regression model which allows for the user to choose which variables to include in the model (with the respective API), do predictions with that model and see evaluation metrics. Also, performed a stepwise method to understand the best logistic regression model. Then, we did a script which includes several ML models and an API which allows to choose the ML model and do a prediction with that model as well as have some metrics.

## Dataset

The dataset used in this project is the [Liver Disease Dataset from Kaggle](https://www.kaggle.com/datasets/rabieelkharoua/predict-liver-disease-1700-records-dataset). 

The dataset includes the following columns:
- **Age**: Age of the individual (20 to 80 years)
- **Gender**: Male (0) or Female (1)
- **BMI**: Body Mass Index (15 to 40)
- **AlcoholConsumption**: Alcohol consumption (0 to 20 units per week)
- **Smoking**: Smoking status (0 = No, 1 = Yes)
- **GeneticRisk**: Genetic risk (0 = Low, 1 = Medium, 2 = High)
- **PhysicalActivity**: Physical activity (0 to 10 hours per week)
- **Diabetes**: Diabetes status (0 = No, 1 = Yes)
- **Hypertension**: Hypertension status (0 = No, 1 = Yes)
- **LiverFunctionTest**: Liver function test score (20 to 100)
- **Diagnosis**: Binary indicator for liver disease (0 = No, 1 = Yes)

## Folder Structure

- **data/**: Contains the liver disease dataset.
- **scripts/**: R scripts for data preprocessing, EDA, model training, and evaluation.
- **notebooks/**: R Markdown notebooks with some explanations of the methods and results.
- **figures/**: Contains plots and figures generated during analysis.
- **api/**: Contains different APIs for logistic regression and machine learning models.
- **requirements/**: Contains dependency files to ensure the environment is set up correctly.

## Example of usage 

The file API_everything.R inside the api folder has the API with ML models for selection, to analyse some metrics and also to do a prediction giving a new data. Then, also allows to choose variables to include in the logistic regression model, test that model with new data and see some evaluation metrics.

## Installation and Setup

To get started with this project, you need to have R and the necessary R packages installed.

Also, for example, regarding dataset location in the computer, might need to be changed. 

1. Clone the repository:

```bash
git clone https://github.com/salemaines/Liver_Disease_Prediction.git
cd Liver_Disease_Prediction


