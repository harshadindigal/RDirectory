# Load required packages
library(tidyverse)
library(ISLR)
library(kknn)

# Make Default and mpg available in the RStudio pane
Default <- as.tibble(Default)
mpg <- as.tibble(mpg)

# Recode default variable to a factor, making "Yes" the last level
Default <- Default %>% mutate(default = 
                                recode_factor(default, "No" = "No", "Yes" = "Yes"))

# Check if "Yes" is mapped to 1
contrasts(Default$default)

## Splitting Data Into a Training and Test Set Using sample()

# Always use set.seed with your favorite number so you can reproduce your results
set.seed(2711845)

# Create a training index with sample()
# Default data
index_default <- sample(x = 1:nrow(Default), size = floor(0.7*nrow(Default)))

Default_training <- Default[index_default, ]
Default_test <- Default[-index_default, ]

# mpg data
index_mpg <- sample(x = 1:nrow(mpg), size = floor(0.7*nrow(mpg)))

mpg_training <- mpg[index_mpg, ]
mpg_test <- mpg[-index_mpg, ]

# k-Nearest Neighbor (KNN)

## KNN Regression
### Fitting the Model For a Given *k*
# k = 2
knn_2 <-  kknn(hwy ~ cty, train = mpg, test = mpg, k = 2, distance = 2)

# Make a results data frame from mpg and add predictions
mpg_knn_results <- mpg %>%  select(hwy, cty) %>% 
                   mutate(knn_2_pred = knn_2$fitted.values)

# View first couple of rows 
mpg_knn_results %>% slice(1:5)

# Plot results
ggplot(data = mpg_knn_results, mapping = aes(x = cty, y = hwy)) +
  geom_point(color = "#006EA1", size = 3) +
  geom_point(aes(x = cty, y = knn_2_pred), color = "orange", size = 2) +
  labs(title = "Highway MPG vs City MPG (KNN 2 Predicted - Orange)",
       x = "City MPG",
       y = "Highway MPG") +
  theme_light()


### Building a KNN Regression Model with Cross Validation

# Find Optimal K
train.kknn(hwy ~ cty, 
           data = mpg_training, 
           kmax = 40)

# Predictions
knn_optimal <-  kknn(hwy ~ cty, train = mpg_training, test = mpg_test, 
                     k = 9, distance = 2)

# Results data frame on test data
mpg_knn_optimal <- mpg_test %>% select(hwy, cty) %>% 
  mutate(knn_optimal_pred = knn_optimal$fitted.values)

# View results
mpg_knn_optimal %>% slice(1:5)

# Calculate R2 on test data results
# Remember that R2 is the squared correlation 
# between actual and predicted values of a model
cor(mpg_knn_optimal$hwy, mpg_knn_optimal$knn_optimal_pred)^2

ggplot(data = mpg_knn_optimal, mapping = aes(x = hwy, y = knn_optimal_pred)) +
  geom_point(color = "#006EA1", size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "orange", size = 1.2) +
  labs(title = "Predicted hwy vs Actual hwy on Test Data, KNN with k = 9",
       x = "Actual hwy Value",
       y = "Predicted hwy Value") +
  theme_light()

# Compare to linear regression
linear_fit <- lm(hwy ~ cty, data = mpg_training)

# We use the predict function to obtain predicted values on test data
# interval = "none" returns a vector of only predicted values
linear_results <- mpg_test %>% select(hwy, cty) %>% 
                  mutate(lm_pred = predict(linear_fit, 
                                           newdata = mpg_test,
                                           interval = "none"))

# Calculate and visualize R2
cor(linear_results$hwy, linear_results$lm_pred)^2

ggplot(data = linear_results, mapping = aes(x = hwy, y = lm_pred)) +
geom_point(color = "#006EA1", size = 2) +
geom_abline(slope = 1, intercept = 0, color = "orange", size = 1.2) +
labs(title = "Predicted hwy vs Actual hwy on Test Data, Linear Regression",
     x = "Actual hwy Value",
     y = "Predicted hwy Value") +
theme_light()

### KNN Regression With Categorical Predictor Variables

# Find Optimal K
train.kknn(hwy ~ cty + class, 
data = mpg_training, 
kmax = 40)

# Predictions
knn_optimal_2 <-  kknn(hwy ~ cty + class, train = mpg_training, test = mpg_test, 
k = 4, distance = 2)

# Results data frame on test data
mpg_knn_optimal_2 <- mpg_test %>% select(hwy, cty, class) %>% 
mutate(knn_optimal_pred_2 = knn_optimal_2$fitted.values)

# View results
mpg_knn_optimal_2 %>% slice(1:5)

# Calculate R2
cor(mpg_knn_optimal_2$hwy, mpg_knn_optimal_2$knn_optimal_pred_2)^2

ggplot(data = mpg_knn_optimal_2, mapping = aes(x = hwy, y = knn_optimal_pred_2)) +
geom_point(color = "#006EA1", size = 2) +
geom_abline(slope = 1, intercept = 0, color = "orange", size = 1.2) +
labs(title = "Predicted vs Actual hwy on Test Data, KNN with k = 4, Two Predictors",
x = "Actual hwy Value",
y = "Predicted hwy Value") +
theme_light()

## KNN Classification
### Building a KNN Regression Model with Cross Validation

# Find Optimal K
train.kknn(default ~ ., 
          data = Default_training, 
          kmax = 40)

# Predictions and Posterior Probabilities
knn_default_optimal <-  kknn(default ~ ., train = Default_training, test = Default_test, 
                             k = 24, distance = 2)

# Results data frame on test data
# Since we are adding posterior probabilities, we must use data.frame()
knn_default_results <- data.frame(Default_test,
                        knn_pred = knn_default_optimal$fitted.values,
                       knn_default_optimal$prob)

# View results
knn_default_results %>% slice(1:5)

# Build the confusion matrix 
knn_confusion <- table(knn_default_results$default,
knn_default_results$knn_pred)

# Our confusion_matrix_results function
confusion_matrix_results <- function(table_matrix, positive_value) {
pos_row = which(rownames(table_matrix) == positive_value)
pos_col = which(colnames(table_matrix) == positive_value)

TP <- table_matrix[pos_row, pos_col]
FN <- table_matrix[pos_row, -pos_col]
FP <- table_matrix[-pos_row, pos_col]
TN <- table_matrix[-pos_row, -pos_col]

results <- 
data.frame(correct = c(TN + TP, round((TN + TP)/sum(table_matrix),3)),
misclassified = c(FN + FP, round((FN + FP)/sum(table_matrix),3)),
true_pos = c(TP, round(TP/sum(table_matrix[pos_row,]),3)),
false_neg = c(FN, round(FN/sum(table_matrix[pos_row,]),3)),
true_neg = c(TN, round(TN/sum(table_matrix[-pos_row,]),3)),
false_pos = c(FP, round(FP/sum(table_matrix[-pos_row,]),3)))

rownames(results) <-  c("Observations", "Rate")

return(results) }

# View results
# We get a great overall classification rate, but a very high 
# false negative rate
confusion_matrix_results(knn_confusion, "Yes")

# Adjusting the Cut-Off Value to Minimize Misclassification and False Negative Rates

# Vector of cut-off values of interest
cut_off <- seq(from = 0, to = 1, by = 0.025)

# Create data frame for results
cut_off_results <- data.frame(cut_off,
                              misclass_rate = numeric(length = length(cut_off)),
                              false_neg_rate = numeric(length = length(cut_off)))
# View first couple of rows
cut_off_results %>% slice(1:5)

for (row in 1:nrow(cut_off_results)) {
# Calculate new predictions
predictions <- ifelse(knn_default_results$Yes >= cut_off_results[row, "cut_off"], "Yes", "No")

# Add results to cut_off_results data frame
cut_off_results[row,"misclass_rate"] <- sum(!(knn_default_results$default == predictions))/
                                              nrow(knn_default_results)

cut_off_results[row,"false_neg_rate"] <- sum((knn_default_results$default == "Yes" &  predictions == "No"))/
                                        sum(knn_default_results$default == "Yes")
} # end of for() loop

cut_off_results %>% slice(1:20)

# Add to knn_default_result data frame
knn_default_results <- knn_default_results %>% 
                       mutate(knn_pred_0.025 = ifelse(Yes >= 0.025, "Yes", "No"))

knn_confusion_0.025 <- table(knn_default_results$default,
                             knn_default_results$knn_pred_0.025)

confusion_matrix_results(knn_confusion_0.025, "Yes")

# Cross Validation Example with Logistic Regression

logistic_fit <- glm(default ~ ., data = Default_training, family = "binomial")

logistic_results <- data.frame(Default_test,
                               logistic_prob = predict(logistic_fit,
                                                      newdata = Default_test,
                                                      type = "response"))

# Vector of cut-off values of interest
cut_off <- seq(from = 0, to = 1, by = 0.025)

# Create data frame for results
cut_off_results_logistic <- data.frame(cut_off,
                                       misclass_rate = numeric(length = length(cut_off)),
                                       false_neg_rate = numeric(length = length(cut_off)))

for (row in 1:nrow(cut_off_results_logistic)) {
  # Calculate new predictions
  predictions <- ifelse(logistic_results$logistic_prob >= 
                          cut_off_results_logistic[row, "cut_off"], "Yes", "No")
  
  # Add results to cut_off_results data frame
  cut_off_results_logistic[row,"misclass_rate"] <- sum(!(logistic_results$default == predictions))/
                                                   nrow(logistic_results)
  
  cut_off_results_logistic[row,"false_neg_rate"] <- 
    sum((logistic_results$default == "Yes" &  predictions == "No"))/
    sum(logistic_results$default == "Yes")
} # end of for() loop

# View results
cut_off_results_logistic %>% slice(1:10)

# Add to logistic_results
logistic_results <- logistic_results %>% 
                    mutate(logistic_pred_0.025 = ifelse(logistic_prob >= 0.025, 
                                                        "Yes", "No"))

logistic_confusion_0.025 <- table(logistic_results$default,
                                  logistic_results$logistic_pred_0.025)

# KNN
confusion_matrix_results(knn_confusion_0.025, "Yes")

# Logistic Regression
confusion_matrix_results(logistic_confusion_0.025, "Yes")










