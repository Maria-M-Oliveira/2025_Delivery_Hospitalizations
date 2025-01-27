# ============================================================
# Supplementary Material for:
# Trends in Delivery Hospitalizations and the Impact of ICD-9-CM to ICD-10-CM/PCS Transition in Portugal (2010-2018)
# Version: 1.0.0
# Last Updated: January 2025
#
# Authors: 
# - Catarina Camarinha
# - Maria Miguel Oliveira 
# - Cecília Elias
# - Miguel de Araújo Nobre
# - Leonor Bacelar-Nicolau
# - Cristina Furtado
# - Andreia Silva Costa
# - Paulo Jorge Nogueira
# 
# License: MIT License
# Repository: https://github.com/Maria-M-Oliveira/2025_Delivery_Hospitalizations
# ============================================================

# Session info:
# R version 4.4.1
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 10 x64 (build 19045) 

# Dependencies:
# - tidyverse (>= 2.0.0)
# - scales (>= 1.2.0)
# - prais (>= 1.1.1)
# - lmtest (>= 0.9-40)
# - sandwich (>= 3.0.2)

# ============================================================
# DESCRIPTION
# ============================================================

# This script performs a segmented regression analysis using the Prais-Winsten
# method to evaluate the impact of ICD-10 implementation on hospitalization 
# episodes in Portugal. The analysis uses simulated data to demonstrate the
# methodology.

# Usage:
# 1. Ensure all required packages are installed
# 2. Run the script in its entirety
# 3. Output will include regression results and visualization
#
# Notes:
# - All data is simulated and does not represent actual hospital records
# - Seed is set to 123 for reproducibility

# ============================================================
# 1. SETUP AND REQUIREMENTS
# ============================================================

library(tidyverse)
library(scales)
library(prais)
library(lmtest)
library(sandwich)

# ============================================================
# 2. DATA INPUT - SIMULATED DATA GENERATION
# ============================================================
# Generate simulated hospitalization data
# Parameters:
# - Base trend: Linear increase from 10,000 to 30,000 episodes
# - Intervention effect: Gradual decrease of up to -5,000 episodes after ICD-10
# - Random noise: Normal distribution with mean=0, sd=1,000

# Generate quarters from 2010.1 to 2018.4
years <- rep(2010:2018, each = 4)
quarters <- rep(1:4, times = length(2010:2018))
quarter <- paste(years, quarters, sep = ".")

set.seed(123) # For reproducibility

# Generate simulated data components
base_trend <- seq(10000, 30000, length.out = length(quarter))
intervention_effect <- c(rep(0, 28), 
                         seq(0, -5000, length.out = length(quarter) - 28))
random_noise <- rnorm(length(quarter), mean = 0, sd = 1000)

# Combine components
Number_of_episodes <- round(base_trend + intervention_effect + random_noise)

# Combine into a data frame
data <- data.frame(
  quarter = factor(quarter, levels = quarter),
  Number_of_episodes = Number_of_episodes
)

# ============================================================
# 3. DATA PREPROCESSING
# ============================================================

# Define variables:
# - time: Sequential time points
# - ICD10: Binary indicator for ICD-10 implementation (1 after implementation)
# - time_ICD10: Time points after ICD-10 implementation

data <- data %>%
  mutate(
    time = row_number(),
    ICD10 = if_else(time >=29,1,0), 
    time_ICD10 = case_when(ICD10 == 1 ~ row_number()-28, TRUE ~ 0)
    )

# ============================================================
# 4. PRAIS-WINSTEN SEGMENTED REGRESSION
# ============================================================

# Model specification:
# Number_of_episodes = β0 + β1*time + β2*ICD10 + β3*time_ICD10 + ε
# Where:
# - β0: Baseline level
# - β1: Baseline trend
# - β2: Level change after ICD-10
# - β3: Trend change after ICD-10

# Fit Prais-Winsten model
model_a <- prais::prais_winsten(Number_of_episodes ~ time + ICD10 + time_ICD10, index ='time', data = data)

# Summary of the defined model
summary(model_a)

# Model diagnostics
print("Model Summary:")
print(summary(model_a))

print("Robust Coefficient Tests:")
print(coeftest(model_a, vcov = vcovHC(model_a, "HC0")))


# ============================================================
# 5. VISUALIZATION
# ============================================================

# Calculate predicted trends
data<- data %>%
  mutate(
    observed_trend= model_a$coefficients[1] + model_a$coefficients[2]*time + model_a$coefficients[3]*ICD10 + model_a$coefficients[4]*time_ICD10
    ) %>%
  mutate(
    counterfactual_trend = model_a$coefficients[1]+ model_a$coefficients[2]*time
    )

# Create visualization

ggplot(data = data)+
  # Observed data points and line
  geom_point(aes(x = quarter, y = Number_of_episodes))+
  geom_line(
    aes(x = quarter, y = Number_of_episodes),
    group =1, 
    color ='azure4',
    size=0.5
    ) +
  # ICD-10 transition marker
  geom_vline(
    xintercept = '2017.1', 
    linetype="dotted", 
    color ='indianred',
    size=1.2
    ) +
  # Regression lines
  geom_line(
    data = data %>% filter(time>=29),
    aes(x = quarter, y = observed_trend),
    group =1, 
    linetype="73", 
    color ='blue4',
    size=1
    )+
  geom_line(
    data = data %>% filter(time<29),
    aes(x = quarter, y = counterfactual_trend),
    group =1, 
    linetype="73", 
    color ='royalblue1',
    size=1
    )+
  # Highlight impact gap
  annotate(
    "rect", 
    xmin = '2017.1', 
    xmax = '2018.4', 
    ymin = data[29,"observed_trend"], 
    ymax = data[28,"observed_trend"],
    fill = "lightgreen", 
    alpha = 0.5
    ) +
  # Add p-value annotation
  annotate(
    "text", 
    x = '2017.2', 
    y = (data[29,"observed_trend"]+data[28,"observed_trend"])/2,
    label = format.pval(coeftest(model_a, vcov = vcovHC(model_a, "HC0"))[[3,4]],3,scientific = FALSE),
    color = "red",
    angle = -25,
    hjust = 0
    ) +
  # Aesthetic adjustments
  scale_y_continuous(limits = c(0,30000),labels = comma)+
  xlab('Year')+
  ylab('Number of episodes')+
  labs(title = 'Time series of the number of episodes of hospitalization in Portugal from 2010 to 2018')+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5,size = 9),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16)
    )

# Save plot
ggsave("hospitalization_trends.png", p, width = 12, height = 8, dpi = 300)

# ============================================================
# END OF SCRIPT
# ============================================================