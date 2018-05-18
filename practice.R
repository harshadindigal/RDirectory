# libraries and data set for this exam.

library(tidyverse)
library(ISLR)
library(broom)

# Make data available in RStudio
College <- as.tibble(College)

# Information about the data set
?College

# Check if Private variable is coded correctly
# for logistic regression
# The code below indicates that Private = "Yes" 
# is mapped to 1
# No need to recode 
contrasts(College$Private)

# R Code for Problem One
College%>%group_by(Private)%>% 
  summarise(avg_enroll = signif(mean(Enroll),digits = 6),
            avg_accept_rate = signif(mean(Accept/Apps),digits = 6),
            avg_min_rate= signif(min(Accept/Apps),digits = 6))

#R Code for Problem two 
College%>%group_by(Private)%>%filter((Apps>1000))%>% summarise(avg_roomboard = mean(Room.Board))
#R Code for Problem Three
College%>%group_by(Private)%>% summarise(avg_enroll = mean(Enroll),avg_book_cost = mean(Books),avg_out_of_state_tution = mean(Outstate),avg_personal_spend = mean(Personal))%>%gather(
                                                                key = "Summary Statistic",
                                                                value = "Value",
                                                                ... = avg_enroll:avg_personal_spend,
                                                                na.rm = FALSE)
#Problem 4
linear_reg <- lm(Enroll ~ Apps, data = College)
augment_results <- augment(linear_reg) %>%
  as.tibble() 
augment_results
coeffs = coefficients(linear_reg)
apps = 300
duration = coeffs[1] + coeffs[2]*apps 
duration
#Problem 5 
full_model <- lm(Enroll ~ Apps + Private,data = College)
reduced_model <- lm(Enroll ~ Apps, data = College)
anova(reduced_model, full_model)
