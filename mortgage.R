library(tidyverse)
library(readxl)
mortgage <- read_excel(path = "./data/Historical Mortgage Origination Estimates.xlsx")
names(mortgage)[2]<- paste("Single Family Orginiations")
names(mortgage)[3] <- paste ("Purchases")
names(mortgage)[4] <-("Refinances")
names(mortgage)[5] <- ("Refinance Percentage")
mortgage = mortgage[-1,]
mortgage = mortgage[-1,]
ggplot(data =  mortgage%>%filter(`Quarterly Mortgage Originations Estimates`), mapping = aes(x = mortgage$`Quarterly Mortgage Originations Estimates`, y = mortgage$`Single Family Orginiations`, color = "BLUE")) +
  geom_point() +
  labs(title = "Olkpeak vs MaxHR by Heart Disease and Thallium Stress Test",
       x = "Old Peak",
       y = "Max HR")

