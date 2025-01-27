# 2025_Delivery_Hospitalizations

Segmented Regression Analysis on Hospitalization Episodes
This repository contains the code and materials for performing a segmented regression analysis using the Prais-Winsten method to evaluate the impact of the ICD-10 coding transition on the number of hospitalization episodes in Portugal between 2010 and 2018.

Key Features:
Data Preprocessing: The data is processed to define key variables such as time, ICD-10 intervention indicator, and time after intervention.
Prais-Winsten Segmented Regression: A Prais-Winsten regression model is used to analyze the time series data and estimate the effects of the ICD-10 transition.
Visualization: The results are visualized with a time-series plot highlighting the observed trend, counterfactual trend, and the intervention's impact.
Simulated Data: A script for generating simulated data is included to help test the analysis. The data features a stable trend with noise before the ICD-10 transition and a decrease afterward.
Structure:
Data Input: Load and preprocess the hospitalization data.
Modeling: Fit a Prais-Winsten segmented regression model to the data.
Visualization: Generate time-series plots to visualize the trends before and after the intervention.
Simulated Data: Simulate a dataset representing the number of episodes of hospitalization to be used with the analysis.
