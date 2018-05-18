## Harsha Dindigal
## Section: Your Section Here: 001  
## Homework 4
library(tidyverse)
library(broom)
install.packages("ISLR")
library(ISLR)
Carseats <- as.tibble(Carseats) # Turn Carseats into a tibble
Credit <- as.tibble(Credit) # Turn Credit into a tibble
Auto <- as.tibble(Auto) # Turn Auto into a tibble
# Information about these data sets
?Carseats
?Credit
?Auto
##Problem1
#Part A
Carseats %>% group_by(Urban, ShelveLoc) %>%
  summarise(AvgSales = mean(Sales)) %>% spread(key = Urban, value = AvgSales)
#Part B
Auto %>% group_by(cylinders)%>%filter((year<80))%>% summarise(mean_mpg = mean(mpg),
 mean_displacement = mean(displacement),
 mean_weight = mean(weight),
 mean_acceleration = mean(acceleration))%>%gather(
                                               key = "Summary Statistic",
                                               value = "Value",
                                               ... = mean_mpg:mean_acceleration,
                                               na.rm = FALSE)
##Problem 2
#PartA
ggplot(data = Auto, mapping = aes(x = weight, y = mpg)) +
  geom_point(color = "#006EA1", size = 1.3) +
  labs(title = "Highway MPG vs City MPG",
       x = "Weight",
       y = " MPG") +
  theme_light()
linear_reg <- lm(mpg ~ weight, data = Auto)
augment_results <- augment(linear_reg) %>%
  as.tibble() 
ggplot(data = augment_results, mapping = aes(x = .fitted, y = .resid)) +
                   geom_point(color = "#006EA1", size = 1.3) +
                   geom_hline(yintercept = 2*summary(linear_reg)$sigma, color = "orange") +
  geom_hline(yintercept = -2*summary(linear_reg)$sigma, color = "orange")  +
                   labs(title = "Residual Scatter Plot for Simple Linear Fit: mpg ~ weight",
                        x = "Predicted MPG Values",
                        y = "Residual") +
                   theme_light()          
#PartB
lm2 = lm(mpg ~ poly(weight,2), data = Auto)
ggplot(data = augment_results, mapping = aes(x = fitted(lm2), y = residuals(lm2))) +
  geom_point(color = "#006EA1", size = 1.3) +
  geom_hline(yintercept = 2*summary(lm2)$sigma, color = "orange") +
  geom_hline(yintercept = -2*summary(lm2)$sigma, color = "orange")  +
  labs(title = "Residual Scatter Plot for Simple Linear Fit: mpg ~ weight,weight^2",
       x = "Predicted MPG Values",
       y = "Residual") +
  theme_light()

##Problem 3
#Part A
full_model <- lm(Sales ~ CompPrice + Advertising + Income + Population + Education + Urban,data = Carseats)
reduced_model <- lm(Sales ~ CompPrice + Advertising + Income, data = Carseats)
anova(reduced_model, full_model)
summary(full_model)
##When conduting partial F-tests, the results conclude that Income is significant at the.01 alpha level when it comes to being linked to predicting sales. We have no evidence that other predictor varialbes add precision to the model.

#Part B
full_model1 <- lm(Balance ~ Cards + Student + Rating + Married,data = Credit)
reduced_model1 <- lm(Balance ~ Cards + Student ,data = Credit)
anova(reduced_model1, full_model1)
summary(full_model1)
##After looking at the ANOVA results, which resulted in a F-value of 796.54 and P-value of 2.2 e-16, it is clear that the additional variables Rating + Married help better predict Balance. Specifically, the partial F-tests reveal that Rating is a signifcant predictor even at a .0001 significane level.

##Problem 4
#Part A
tidy(lm(Sales ~ Income + ShelveLoc + Income:ShelveLoc, data = Carseats))
ggplot(data = Carseats, mapping = aes(x = Income, y = Sales, color = ShelveLoc)) +
  geom_point() +
  geom_abline(intercept = 4.101414819, slope = 0.019677603, color = "#F8766D") + #Bad 
  geom_abline(intercept = 4.101414819 + 1.762287171 , slope = 0.019677603 - 0.001745398, color = "#619CFF") + #Medium
geom_abline(intercept = 4.101414819 + 5.631363596  , slope =  0.019677603 - 0.012598366, color = "#00BA38") + #Good
  labs(title = "Sales ~ Income + ShelveLoc with Estimated Regression Lines") +
  theme_light()
#Part B
full_model2 <- lm(Sales ~ Income + ShelveLoc + Income:ShelveLoc,data = Carseats)
reduced_model2 <- lm(Sales ~ Income + ShelveLoc ,data = Carseats)
anova(reduced_model2, full_model2)
summary(full_model2)

##After looking at the ANOVA results, which resulted in a F-value of .981 and P-value of .3758, it is clear that the additional variable Income:Shelveloc does not more acuarley estimate Sales. Specifically, the partial F-tests reveal that Income:ShelvelocGood (P-value = 3104) and Income:ShelveLocMedium (P-value .8655) are rather large. Indicating that the interaction between Income and Shelveloc does not indicate any additional accuarcy of a multilinear regression of Sales.

##Problem 5
null_model1 <- lm(Balance ~ 1, data = Credit)

upper_model1 <- lm(Balance ~ ., data = Credit)

step(null_mode1l, 
     scope = list(lower = null_model1,
                  upper = upper_model1), 
     method = "both", trace = 0)

augment_result1 <- augment(upper_model1)

ggplot(data = augment_result1, mapping = aes(x = Balance, y = .fitted)) +
  geom_point(color = "#006EA1", size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "orange", size = 1.2) +
  labs(title = "Predicted Balance (Using Income, Student, Interaction) vs Actual",
       x = "Actual Balance Value",
       y = "Predicted Balance Value") +
  theme_light()