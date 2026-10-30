# KWIGIRA: Rwanda Agricultural Productivity Explorer and Yield Predictor

## 1. Project Overview

KWIGIRA is an interactive Shiny application developed for the NISR Big Data Hackathon 2026. The application explores agricultural productivity in Rwanda and provides data-driven insights into crop production, yield patterns, and agricultural practices.

The application is designed to help users explore agricultural data, compare productivity across geographical areas, understand factors associated with crop yield, and obtain estimated yield predictions using a machine learning model.

**deployed public application link:** https://kwigira-agriculture.shinyapps.io/kwigira_shiny_app/

## 2. Problem Statement

Agricultural productivity is important for food security, farmers' livelihoods, and economic development in Rwanda. However, agricultural datasets can be difficult to interpret without appropriate analytical tools.

KWIGIRA addresses this challenge by providing an interactive platform for exploring agricultural production data, comparing productivity, and presenting model-based yield predictions based on the current datasets.

## 3. Data Sources and Preparation

The project uses agricultural microdata associated with the 2025 Season A agricultural production data and agricultural practice data.

Data preparation involves importing the source files, cleaning and organizing variables, preparing the analytical dataset, and combining relevant production and agricultural practice information where appropriate.

The analytical dataset used by the application is stored in `analysis_data.rds`. Agricultural practice data are provided through `Rwa_raw_SeasonA2025_Agricultural_practice.csv`.

Data availability and permitted use should follow the requirements established by the data provider and hackathon organizers.

## 4. Application Features

KWIGIRA provides the following features:

- **Interactive filters:** Explore data using available crop and geographical filters.
- **Summary statistics:** Review key agricultural production and productivity summaries.
- **Yield distribution:** Examine the distribution of crop yield.
- **Geographical comparison:** Compare productivity across available districts and other geographical areas.
- **Yield prediction:** Estimate yield using a Random Forest model.
- **Variable importance:** Explore variables that contribute to the fitted model's predictions.
- **Productivity assessment:** Review productivity patterns and comparisons.
- **Agricultural Assistant:** Access application-based guidance for interpreting agricultural information.

The information displayed depends on the variables available in the analytical dataset.

## 5. Statistical Analysis and Machine Learning

The project combines descriptive statistical analysis, data visualization, and machine learning.

Descriptive analysis is used to summarize production and productivity patterns. Graphs and tables help users compare observations across selected categories and geographical areas.

A Random Forest model implemented using the `ranger` package is used for yield prediction. The model learns relationships between the selected predictor variables and the target variable from the available training data.

Variable importance is used to help interpret which predictors contribute to the model's predictions. These results describe statistical associations within the fitted model and should not be interpreted as proof of causation.

## 6. Model Evaluation

Model performance should be evaluated using a separate test dataset and appropriate prediction metrics, including:

- **Root Mean Squared Error (RMSE):** Measures the typical magnitude of prediction errors, with greater penalties for larger errors.
- **Mean Absolute Error (MAE):** Measures the average absolute difference between observed and predicted values.
- **R-squared:** Describes the proportion of variation in the outcome explained by the model, as defined for the evaluation procedure.

Final evaluation values should be reported from the current version of the trained model and test dataset. Model performance may vary depending on the data preparation procedure, predictor variables, and training configuration.

## 7. Technologies Used

The project uses:

- **R:** Data analysis and statistical computing.
- **Shiny:** Interactive web application development.
- **dplyr:** Data manipulation.
- **readr:** Data import.
- **ranger:** Random Forest modelling.
- **RDS and CSV files:** Analytical data storage and exchange.
- **shinyapps.io:** Hosting the online application.
- **GitHub:** Source code management and project documentation.

## 8. Accessing the Application

The application is publicly available at:

https://kwigira-agriculture.shinyapps.io/kwigira_shiny_app/

Users can open the application in a web browser, select available filters, explore charts and summary statistics, compare geographical productivity, and use the yield prediction functionality.

## 9. Acknowledgment

KWIGIRA was developed for the NISR Big Data Hackathon 2026, with the aim of demonstrating how statistical analysis, machine learning, and interactive visualization can support the exploration of agricultural productivity in Rwanda.

So, my appreciation goes with our Lecturer who mentored us during this project till to the end.

