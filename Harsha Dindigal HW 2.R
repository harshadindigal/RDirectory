## Harsha Dindigal
## Section: Your Section Here: 001  
## Homework 2
library(tidyverse)
library(readxl)
## Problem 1
my_list <- list(character_vector = c("Data", "Mining", "is", "Awesome"),
                my_data = data.frame(gender = c("M", "F", "F", "M", "F"),
                                     age = c(19, 21, 24, 18, 28),
                                     weight = c(194, 154, 135, 165, 172),
                                     stringsAsFactors = FALSE))

# Part (a)
mean(my_list$my_data$weight)

# Part (b)
my_list$character_vector[4] <- "Hard"
my_list$character_vector

# Part (c)
mean_dev <- function(x) {
  mean_x <- mean(x)
  return(x - mean_x) }
replacement <- data.frame(lapply(my_list$my_data[2:3],mean_dev))
my_list$my_data[2:3] <- replacement
my_list$my_data

## Problem 2
numeric_vector_1 <- c(2, 4, 7, 4, 9, 4, 12, 4)
numeric_vector_2 <- c(20, 23, 56, 34, 54, 10)

# Part (a)
for (i in (1:length(numeric_vector_1))) {
  numeric_vector_1[i] <- numeric_vector_1[i] + 2
}
numeric_vector_1

#Part (b)
if(numeric_vector_2[1] > mean(numeric_vector_2)){numeric_vector_2[1] <-1 }else {numeric_vector_2[1] <- 0}
numeric_vector_2

## Problem 3
patient_data <- data.frame(gender = c("Male", "Male", "Female", "Female",
                                      "Male", "Female", "Female", "Male",
                                      "Female", "Male"),
                           age = c(20, 54, 68, 34, 45, 42,
                                   43, 38, 50, 25),
                           cholesterol = c(110, 180, 173, 132,
                                           140, 120, 134, 140, 190, 95),
                           stress_test = c("High", "Low", "Medium", "Low",
                                           "Very High", "Medium", "Low",
                                           "High", "Very High", "Medium"),
                           stringsAsFactors = FALSE)

#Part A
patient_data$high_stress <- recode(patient_data$stress_test,"High" = 1, "Very High" = 1, "Medium" = 0 , "Low" = 0)
patient_data

#Part B
min_max <- function(x) {
  (x - min(x))/(max(x) - min(x)) }
replacey <- data.frame(lapply(patient_data[2:3],min_max))
patient_data[2:3] <- replacey  
patient_data

##Problem 4
heart <- read_excel(path = "./data/Heart Disease.xlsx")
heart %>% group_by(HeartDisease, ThalliumStressTest) %>% 
  summarise(patients_n = n(),
            avg_chol = signif(mean(Cholesterol),digits = 6),
            avg_rest = signif(mean(RestBP),digits = 6),
            avg_age = signif(mean(Age),digits = 6))
##Problem 5

#Part A
heart$age_category <- cut(x = heart$Age,
                                                 breaks = c(-Inf,39, 50,60, Inf),
                                                 labels = c("Less than 40", "40 -50", "51-60","61 and Older"),
                                                 right = TRUE)
heart %>% select(Age, age_category)

#Part B
heart %>% group_by(HeartDisease,age_category) %>% 
  summarise(patients_n = n(),
            avg_max_hr = signif(mean(MaxHR),digits = 6),
            avg_old_peak = signif(mean(Oldpeak),digits = 6),
            avg_calcium = signif(mean(Calcium),digits = 6))
