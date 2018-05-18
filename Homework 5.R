##Harsha Dindigal 
#Homework 5

#note to instructor: library e10171 failed to install, code in 4 and 5 fail to actually create the data fram for some reason, however, when tested online R consoles, the code works. Not sure why I can't access the dataframes in the global environment.

library(MASS)
library(tidyverse)
library(readxl)
library(e1071)
library(ggplot2)
# Adjust the path as needed
# Telco Customer Churn
telco <- read_excel(path = "./data/telco customer churn.xlsx")
# Recode "Churn" variable for classification
telco <- telco %>% mutate(Churn = recode_factor(Churn, "No" = "No", "Yes" = "Yes"))
# Function for analyzing confusion matrices
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
  rownames(results) <- c("Observations", "Rate")
  return(results)}
  
#Problem 1 
#Part A
telco%>%group_by(PaymentMethod)%>%summarise(customer = n(),
avg_tenure = mean(TenureMonths), 
avg_monthly_changes = mean(MonthlyCharges), 
churn_yes_rate = (sum(Churn == "Yes")/customer))
#Part B
drake <-telco%>%group_by(PaymentMethod, Contract)%>%summarise(customer = n(),churn_rate = (sum(Churn == "Yes")/sum(customer)))%>%spread(key = Contract, value = churn_rate)

#Problem 2
#Part A
telco%>%filter(InternetService == "DSL"|InternetService == "Fiber optic")%>%ggplot(mapping = aes(x = Contract, y = TenureMonths, fill = Churn)) + geom_boxplot() + labs(title = "Tenure Months by Contract and Payment Type for Customers with Internet Service",x = "Contract", y = "Tenure Months") + facet_grid(PaymentMethod ~ InternetService)
#Part B
ggplot(data = telco, mapping = aes(x = MonthlyCharges,y = TenureMonths, color = Churn)) + geom_point() + facet_wrap(~PaymentMethod, nrow = 2)

#Problem 3
#Part A

telco_logistic <- glm(Churn ~ Contract + PaymentMethod + MonthlyCharges+ TenureMonths, # same as in lm()
                      data = telco,
                      family = "binomial")

telco <- telco %>%
  mutate(prob_yes_churn = predict(telco_logistic,
                                     newdata = telco,
                                     type = "response"))

scen1 <- telco%>%filter(MonthlyCharges > 80,PaymentMethod == "Electronic check",Contract == "Month-to-month", TenureMonths > 30)

scen2 <-telco%>%filter(MonthlyCharges > 80,PaymentMethod == "Mailed check",Contract == "Month-to-month", TenureMonths > 30)
scen1$prob_yes_churn
probscen1 <- mean(scen1$prob_yes_churn)
probscen2 <- mean(scen2$prob_yes_churn)

#According to our GLM model, there is a 0.1454899 or 14.45% higher chance that an electroic check payer will leave the company vs a ailed check payer while holding all esle equal.

#Part B
logistic_subset <- telco %>% dplyr::select(Churn,Contract, PaymentMethod, MonthlyCharges, TenureMonths)
logistic_model <- glm(Churn ~ Contract +PaymentMethod + MonthlyCharges + TenureMonths, 
                      data = logistic_subset, family = "binomial")
logistic_subset <- logistic_subset %>%
  mutate(estimated_prob = predict(logistic_model,
                                  newdata = logistic_subset,
                                  type = "response"),
         churn_predicted = ifelse(estimated_prob >= 0.4, "Yes", "No"))
logistic_subset %>% slice(1:6)

#Part 4
naive_subset <- telco %>%
  dplyr::select(Churn, Contract, PaymentMethod, MonthlyCharges, TenureMonths) %>%
  mutate(Churn = as.factor(Churn),
         Contract = as.factor(Contract),
         PaymentMethod = as.factor(PaymentMethod),
         MonthlyCharges = as.factor(MonthlyCharges),
         TenureMonths = as.factor(TenureMonths))
naive_subset_model <- naiveBayes(Churn ~ Contract + PaymentMethod + MonthlyCharges + TenureMonths,
                                 data = naive_subset)

naive_subset <- data.frame(naive_subset,
                           Naive_Predicted = predict(naive_subset_model,
                                                     newdata = naive_subset,
                                                     type = "class"), 
                           predict(naive_subset_model, newdata = naive_subset,
                                   type = "raw"))

#Part 5
naive_confusion <- table(naive_subset$Churn, 
                         naive_subset$Naive_Predicted)
logistic_confusion <- table(logistic_subset$Churn, 
                            logistic_subset$churn_predicted)

confusion_matrix_results(naive_confusion, "Yes")
confusion_matrix_results(logistic_confusion, "Yes")

#According to the Conufusion Matrix results, the logistic regression was more accurate than the Naive Bayes theorem