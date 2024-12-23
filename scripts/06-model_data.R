
#### Preamble ####
# Purpose: Fit and evaluate multiple models to analyze community crime rates, including static, dynamic, 
#          and random forest models, and compare their performance.
# Author: Weiyang Li
# Date: 23 December 2024
# Contact: weiyang.li@mail.utoronto.ca
# License: MIT
# Pre-requisites:
#   - The cleaned dataset ("analysis_data.parquet") must be available in the directory "data/02-analysis_data/"
#   - Required R packages: dplyr, arrow, caret, brms, randomForest, ggplot2
#   - Sufficient computational resources to run Bayesian models using the `brms` package
# Any other information needed? Ensure the dataset is validated, and the workspace is appropriately configured for parallel processing for Bayesian modeling.



#### Workspace setup ####

library(dplyr)
library(arrow) 
library(caret)
library(brms)
library(randomForest)


#### Read data ####

crime_data <- read_parquet("data/02-analysis_data/analysis_data.parquet")


### Model preparation ####

set.seed(154)

# Define the training set ratio
train_ratio <- 0.7

# Creating a dataset index
train_indices <- createDataPartition(crime_data$Crime_Count, p = train_ratio, list = FALSE)

# Segmentation of training and test sets based on indexing
train_data <- crime_data[train_indices, ]
test_data <- crime_data[-train_indices, ]



### Model Fitting ####

# 1. Static modeling: analysis of disparities in community crime rates

static_formula <- bf(
  Crime_Rate_per_1000 ~ Crime_Type + (1 | AREA_NAME),
  family = gaussian())

static_model <- brm(
  formula = static_formula,
  data = train_data,
  prior = c(
    prior(normal(0, 10), class = "b"),
    prior(cauchy(0, 2), class = "sd")),
  chains = 4,
  iter = 2000,
  cores = 4,
  seed = 154)

# 2. Dynamic modeling: temporal changes in community crime rates

dynamic_formula <- bf(
  Crime_Rate_per_1000 ~ Crime_Type * Year + (1 + Year | AREA_NAME),
  family = gaussian())

dynamic_model <- brm(
  formula = dynamic_formula,
  data = train_data,
  prior = c(
    prior(normal(0, 10), class = "b"),
    prior(cauchy(0, 2), class = "sd"),
    prior(cauchy(0, 2), class = "sigma")),
  chains = 4,
  iter = 2000,
  cores = 4,
  seed = 154)



### Model Visualization ####

summary(static_model)
plot(static_model)

summary(dynamic_model)
plot(dynamic_model)


### Validation ####

# 1. Posterior Predictive Check

pp_check(static_model)

pp_check(dynamic_model)


# 2. Test data evaluation

predictions <- posterior_predict(static_model, newdata = test_data)
static_rmse <- sqrt(mean((apply(predictions, 2, mean) - test_data$Crime_Rate_per_1000)^2))
cat("RMSE on test data:", static_rmse, "\n")

predictions2 <- posterior_predict(dynamic_model, newdata = test_data)
dynamic_rmse <- sqrt(mean((apply(predictions2, 2, mean) - test_data$Crime_Rate_per_1000)^2))
cat("RMSE on test data:", dynamic_rmse, "\n")



### Alternative Model ####

# Random Forest
rf_model <- randomForest(
  Crime_Rate_per_1000 ~ Crime_Type + Year + AREA_NAME,
  data = train_data,
  ntree = 500)

importance(rf_model)
varImpPlot(rf_model)

rf_predictions <- predict(rf_model, newdata = test_data)
rf_rmse <- sqrt(mean((rf_predictions - test_data$Crime_Rate_per_1000)^2))
cat("RF RMSE (Crime_Rate_per_1000):", rf_rmse, "\n")


#### Model Comparison ####

# RMSE
results <- data.frame(
  Model = c("Random Forest", "Static Model", "Dynamic Model"),
  RMSE = c(rf_rmse, static_rmse, dynamic_rmse))
print(results)

# Visualization
library(ggplot2)
ggplot(results, aes(x = Model, y = RMSE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Model Performance Comparison", y = "RMSE", x = "Model") +
  theme_minimal()



### Save Model ####

saveRDS(static_model, file = "models/static_model.rds")

saveRDS(dynamic_model, file = "models/dynamic_model.rds")

