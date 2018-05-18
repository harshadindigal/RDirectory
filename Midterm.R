# Run the following code to import the required 
# libraries and datasets for this exam.

library(tidyverse)
library(ISLR)
library(broom)

# Make data available in RStudio
Carseats <- as.tibble(Carseats)
Default <- as.tibble(Default)
College <- as.tibble(College)

# Information about the data sets
?Carseats
?Default

# Check if default variable is coded correctly
# for logistic regression
# The code below indicates that default = "Yes" 
# is mapped to 1
# No need to recode 
contrasts(Default$default)

# R Code for Problem One
Carseats%>%group_by(US, Urban)%>%filter((Population>200),ShelveLoc == "Medium")%>%summarise(mean_sales = mean(Sales),
                                                                                         mean_price = mean(Price),
                                                                                          mean_comp_price= mean(CompPrice))
# R Code for Problem Two
Carseats%>%group_by(ShelveLoc)%>%filter(Age > 35 , Urban == "Yes")%>%summarise(mean_sales = mean(Sales))

#R Code for Problem Three
Carseats%>%group_by(Urban)%>%filter(Age >= 50 , US == "Yes")%>%summarise(mean_income = mean(Income))
#R Code for Problem Four
Default%>%group_by(default, student)%>% summarise(mean_balance = mean(balance),mean_income = mean(income))%>%gather(
  key = "Summary Statistic",
  value = "Value",
  ... = mean_balance:mean_income,
  na.rm = FALSE)
#R Code for Problem Five
Carseats%>%group_by(Urban,ShelveLoc)%>%summarise(AvgSales = mean(Sales))%>%spread(key = ShelveLoc, value = AvgSales)
#R Code for Problem Six
linear_reg <- lm(Price ~ CompPrice, data = Carseats)
summary(linear_reg)
mean(Carseats$Price)/19.23
# R Code for Problem Seven 
predict(linear_reg,
        newdata = data.frame(CompPrice = c(90)),
        interval = "confidence",
        level = 0.95)

#R Code for Problem Nine 
linear_reg1 <- lm(Sales ~ Price + CompPrice + Age, data = Carseats)
predict(linear_reg1,
        newdata = data.frame(Price =c(121),CompPrice = c(132), Age = c(40)))
#R Code for Problem Ten 
linear_reg2 <- lm(Sales ~ Urban + ShelveLoc, data = Carseats)
predict(linear_reg2,
        newdata = data.frame(Urban = c("Yes"),ShelveLoc = c("Bad")))

#R Code for Problem Eleven
full_model1 <- lm(Sales ~ Price + ShelveLoc + Age,data = Carseats)
full_model2<- lm(Sales ~ Price + ShelveLoc + CompPrice, data = Carseats)
reduced_model1 <- lm(Balance ~ Price + ShevlveLoc ,data = Carseats)
anova(reduced_model1, full_model1)
anova(reduced_model1,full_model2)
summary(full_model1)
summary(full_model2)

#According to the Partial - F test, there are significanly small P-values (<2e^16) between Age and CompPrice, suggesting they are helpful when lookiing at betting predicting Price. Additionally, the addition of CompPrice to the model creates an F-Statistic of 229 and the addition of Age to the model creates an F-Statistic of 163.7,both relatively large, affirming that both varialbes help better predict Price. 

#R Code for Problem Thirteen
logistic_model <- glm(default~income, data = Default, family = "binomial")
summary(logistic_model)
estimate = exp(-3.094 + (-.000008353)*50000)/(1+exp(-3.094 + (-.000008353)*50000))
estimate

#R Code for Problem Fourteen
odds = -3.094 + ((-.000008353)*10000)
logodds = exp(-3.094 + ((-.000008353)*10000))
odds
logodds
