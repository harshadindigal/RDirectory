## Name: Harsha Dindigal
## Section: Your Section Here: 001 (7:30am)
## Homework 3
library(tidyverse)
library(readxl)
heart <-
  read_excel(path = "./data/Heart Disease.xlsx")

##Problem 1
#Part A
heart %>% group_by(HeartDisease, ChestPain) %>%
         summarise(RestBP_ChestPain = mean(RestBP)) %>% spread(key = ChestPain, value = RestBP_ChestPain)

#Part B
heart$gender <- recode(heart$Sex , "0" = "Female", "1" = "Male")
heart%>%group_by(gender)%>%filter((Age>74))%>%
  gather(key = "predictor_variables", value = "value" , ... = 4:5)%>% select(gender, predictor_variables,value)

##Problem 2

#Part A
heart$gender <- recode(heart$Sex , "0" = "Female", "1" = "Male")
heart %>% group_by(gender, ChestPain) %>%
  summarise(patients = n()) %>%
  mutate(percent_of_all_patients = 100*(patients/297))

#Part B
ggplot(data = heart, mapping = aes(x = Oldpeak, y = MaxHR, color = gender)) +
  geom_point() +
  facet_grid(gender ~ ChestPain) +
  labs(title = "Olkpeak vs MaxHR by Heart Disease and Thallium Stress Test",
       x = "Old Peak",
       y = "Max HR")
#Problem 3

mpg <- mpg
View(mpg)
mpg$year <- recode(mpg$year, "1999" = "1999", "2008" = "2008")
ggplot(data = mpg, mapping = aes(x = displ, y = cty, color = year)) +
  geom_point() +
  facet_wrap(~ trans, nrow = 2)+
  geom_smooth(method ="lm" , se = FALSE)

#Problems 4

#Part a
mpg$year <- recode(mpg$year, "1999" = "1999", "2008" = "2008")
ggplot(data = mpg, mapping = aes(x = drv, y = cty, color = year)) +
  geom_boxplot() +
  labs(y = "Cty") + facet_wrap(~year, scales='free_y')
#Part b
mpg %>% group_by(class, fl) %>%
  summarise(maxhwy = max(hwy)) %>% spread(key = fl, value = maxhwy)
#Problem 5
ggplot(data = heart, mapping = aes( x = MaxHR)) +
  geom_histogram(aes(y = ..density..),fill = "#006EA1", color = "white" , bins = 25) + labs ( title = "Density Histogram of MaxHR with Normal Distribution Function", x = "MaxHR", y = "Proportion of Patients")+ stat_function (fun = dnorm , args = list(mean = mean(heart$MaxHR), sd = sd(heart$MaxHR)),color = "orange", size = 1)
         