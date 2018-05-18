library(tidyverse)
library(readxl)
heart <-
  read_excel(path = "./data/Heart Disease.xlsx")

##Problem 1
#Part A
group_by(HeartDisease, ChestPain,RestBP) %>%
         summarise(RestBP_ChestPain = mean(RestBP) %>% spread(key = ChestPain, value = RestBP_ChestPain)

         