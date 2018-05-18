# Run the following code to import the required 
# libraries and datasets for this exam.

library(tidyverse)
library(ISLR)

# Make data available in RStudio
College <- as.tibble(College)

# Turn the row names into a variable
College <- rownames_to_column(College, var = "University")

# Information about the data sets
?College


# Check if default variable is coded correctly
# for logistic regression
# The code below indicates that Private = "Yes" 
# is mapped to 1
# No need to recode 
contrasts(College$Private)

# R Code for Problem One