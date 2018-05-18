library(tidyverse)
library(readxl) # To read Excel files


# Read CSV file, adjust the file path as needed
boston <-  read_delim(file = "Macintosh HD/Users/harshadindigal/Desktop/R Directory/Data/Boston.csv",
                      delim = ",")  # comma-delimited

getwd() # Check if your directory is mapped correctly

boston <-  read_delim(file = "Macintosh HD/Users/harshadindigal/Desktop/R Directory/Data/Boston.csv", delim = ",")

# Specifying column types
boston <-  read.delim(file = "Macintosh HD/Users/harshadindigal/Desktop/R Directory/Data/Boston.csv",
                      delim = ",",
                      col_types = "dddiddddiidddd")

# We can check the results by printing the data frame (just hit enter)
boston

# Excluding columns
boston <-  read_delim(file = "Macintosh HD/Users/harshadindigal/Desktop/R Directory/Data/Boston.csv",
                      delim = ",",
                      col_types = "d------------d")

# Check results
boston

## Importing Tab Delimited Text Files (.txt or .dat)

mpg <-  read_delim(file = "Macintosh HD/Users/harshadindigal/Desktop/R Directory/Data/mpg.txt", 
                   delim = "\t", col_types = "ccdiiccddcc")

# Check results
mpg

## Importing Excel Files (.xlsx)

student_data <- read_excel(path = "Macintosh HD/Users/harshadindigal/Desktop/R Directory/Data/Student Performance.xlsx")

# View data
student_data

# Specifying columns types in read_excel
student_data <- read_excel(path="Macintosh HD/Users/harshadindigal/Desktop/R Directory/Data/Student Performance.xlsx",
                           col_types = c("numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric", 
                                         "numeric"))

student_data

## Importing Raw Text Data

clinton_first <- read_lines(file = "Macintosh HD/Users/harshadindigal/Desktop/R Directory/Data/clinton_1st.txt")

length(clinton_first)
nchar(clinton_first)

# Exporting Data and Saving R Data Frames
# Save an R data frame
# The code below saves the boston data frame that we previously loaded 
# to my desktop as "boston_data_frame"

save(boston, file = "Macintosh HD/Users/harshadindigal/Desktop/boston_data_frame_file") 

# To load R data frame, specify the file path 
load(file = "Macintosh HD/Users/harshadindigal/Desktop/boston_data_frame_file")

# Save as a CSV
write.table(boston,
            file = "Macintosh HD/Users/harshadindigal/Desktop/boston_csv_test.csv",
            sep = ",", row.names = FALSE)

# Save as tab delimited
# Same as above, just change .csv to .txt and sep = "\t"
write.table(boston,
            file = "Macintosh HD/Users/harshadindigal/Desktop/boston_txt_test.txt",
            sep = "\t", row.names = FALSE)

# This file is too large and will give you an error
write.table(boston, file = "clipboard", row.names = FALSE, sep = "\t")

# Increase the memory capacity of your clipboard
write.table(boston, file = "clipboard-64000", row.names = FALSE, sep = "\t")

# Common Data Preprocessing Tasks in R

survey <- tibble(gender_male = c(1, 1, 0, 0, 0, 1),
                 age = c(24, 31, 28, 42, 18, 37),
                 occupation = c("Biotech", "Statistics", "Data Science",
                                "Marketing","Biotech", "Data Science"),
                 job_level = c("Entry", "Mid", "Mid", "Senior", "Entry", "Mid"))


# View data
survey

## Recoding Variables Categorical Variables

# Creating a factor with recode_factor
# Syntax: value on left is original value in the vector
#         value on right is what you want it to be
# numeric values must be surrounded by ` if appearing on the left
survey$gender_factor <- recode_factor(survey$gender_male,
                                      `0` = "Female",
                                      `1` = "Male")

# Creating a character vector with recode
survey$gender_char <- recode(survey$gender_male,
                             `0` = "Female",
                             `1` = "Male")

# View results
survey[ , c("gender_male", "gender_factor", "gender_char")]

## Creating Indicator Variables
# Logical subsetting
# We use the following condition, which results in a logical vector
survey$occupation == "Data Science"

# Now we use the property that when we convert a logical vector to
# numeric, TRUE becomes 1 and FALSE becomes 0
as.numeric(survey$occupation == "Data Science")

# Putting these steps together and
# assigning the results to a vector named
# data_science within the survey data frame

survey$data_science <- as.numeric(survey$occupation == "Data Science")

# Check results
survey[ , c("occupation", "data_science")]

# Indicator with multiple conditions
survey$entry_mid <- recode(survey$job_level,
                           "Entry" = 1,
                           "Mid" = 1,
                           "Senior" = 0)

# An alternative way is to code "Senior" and specify 
# that missing values should be 1
# This is done with the .default option
survey$entry_mid_2 <- recode(survey$job_level,
                             "Senior" = 0,
                             .default = 1)

# View results
survey[ , c("job_level", "entry_mid", "entry_mid_2")]

## Recoding Numeric Variables
# Using cut()
survey$age_category <- cut(x = survey$age,
                           breaks = c(-Inf, 24, 35, Inf),
                           labels = c("Less than 25", "25 - 35", "36 and older"),
                           right = TRUE)

# Returns a factor
class(survey$age_category)

# Obtain counts
summary(survey$age_category)

# Check results
survey[ , c("age", "age_category")]

## Transforming Numeric Variables : Standardizing Data and Min-Max Transformations

# Standarizing data
age_vector <- c(23, 18, 34, 52, 43, 24)

age_vector_stand <- (age_vector - mean(age_vector))/sd(age_vector)

# Check results original
age_vector

# Standardized
age_vector_stand

# Writing a function
standardize <- function(x) {
  (x - mean(x))/sd(x) }

# Let's check to see if we get the same result
standardize(age_vector)


min_max <- function(x) {
  (x - min(x))/(max(x) - min(x)) }

# Perform transformation on age_vector
age_vec_min_max <- min_max(age_vector)

# Check results
age_vec_min_max

# lapply
sample_data <- data.frame(vector_1 = c(4, 1, 6, 23, 15, 5),
                          vector_2 = c(58, 45, 68, 35, 67, 54),
                          vector_3 = c(24, 12, 45, 24, 64, 76))

# Here we apply the standardize function to each vector in sample_data
sample_data_stand <- lapply(sample_data, 
                            standardize)

# Check results
sample_data_stand

sample_data_stand <- data.frame(lapply(sample_data, 
                                       standardize))

# Check results
sample_data_stand

# Is it a data frame?
class(sample_data_stand)

# Standarize only the fisrt two vectors
sample_data[ , c(1,2)] <- lapply(sample_data[ , c(1,2)], 
                                 standardize)

# View results - still a data frame
sample_data

# Introduction to dyplr
heart <-  read_excel(path = "Macintosh HD/Users/harshadindigal/Desktop/Data/Heart Disease.xlsx")

# Let's take a look at the data
heart

## filter()

# View only males (Sex = 1)
filter(heart, Sex == 1)

# Men over 68 years old
filter(heart, Sex == 1, Age > 68)

# Women with typical OR nontypical chest pain
filter(heart, Sex == 0, ChestPain == "typical" | ChestPain == "nontypical")

## select()
# Select the first three columns of Heart
select(heart, Age, Sex, ChestPain)

# Select the first three columns with a numeric vector specifying column positions
select(heart, c(1, 2, 3))

# Select the first three columns with raw numbers
select(heart, 1, 2, 3)

# Select all but the last 4 variables
select(heart, -11, -12, -13, -14)

# Select everything but the first 12 columns
select(heart, -(1:12))

# Select all variables that contain the word Rest
# This uses the match function in dplyr
# NOT case sensitive
select(heart,contains("Rest"))

# Select all variables that start with "s" (or "S")
select(heart, starts_with("s"))

## arrange()
# Let's arrange the heart data by Age, then by RestBP, both
# in ascending order (the default)
arrange(heart, Age, RestBP)

# Now let's arrange heart by Sex in descending order
# and Cholesterol in ascending order
arrange(heart, desc(Sex), Cholesterol)

## summarise()
# Let's get the average RestBP in the heart data

summarise(heart, avg_rest_bp = mean(RestBP))
# We can also create multiple summaries at once
# The quantile function returns percentile values
summarise(heart, min_bp =    min(RestBP),
          bp_25th =   quantile(RestBP, 0.25),
          median_bp = median(RestBP),
          bp_75th =   quantile(RestBP, 0.75),
          max_bp =    max(RestBP))

## mutate()
# Create a new variable rest_bp_scaled
# We will take the RestBP variable, subtract its average value
# and divide by its standard deviation
heart <-  mutate(heart, 
                 rest_bp_scaled = (RestBP - mean(RestBP))/sd(RestBP))

# Let's see the result
select(heart, RestBP, rest_bp_scaled)

## Combining Steps With The %>% Operator
# Pipe operator example
# Let's say we want to convert the number 23 to a character with as.character
# In base R this is done as follows
as.character(23)

# This same task is done the following way with %>%
# %>% passes the first result, a numeric vector of length 1, into the as.character()
# function where it is then converted
23 %>% as.character()

# Let's use %>% with dplyr functions
# I would like to obtain the average RestBP for patients older than 60

heart %>% filter(Age > 60) %>% 
  summarise(avg_rest_bp_60 = mean(RestBP))

# Same analysis as above, but without the %>% operator
# Note how the code above is much easier to follow in terms
# of what the user is trying to do

filtered_df <- filter(heart, Age >60) 

# Now we pass to the summarise function
summarise(filtered_df, avg_rest_bp_60 = mean(RestBP))

## group_by()
heart %>% filter(Age > 60) %>% 
  group_by(Sex) %>% 
  summarise(avg_rest_bp_60 = mean(RestBP))

# the n() function in dplyr returns the number of rows per group
heart %>% group_by(ChestPain) %>% 
  summarise(patients_n = n())

heart %>% group_by(ChestPain, HeartDisease) %>% 
  summarise(patients_n = n(),
            avg_chol = mean(Cholesterol),
            sd_chol = sd(Cholesterol))

# Joing data frames
purchases <- tibble(customer_id = c(45, 12, 100, 100, 54, 25),
                    product_id = c(1, 1, 3, 5, 6, 1))

products <- tibble(product_id = c(1, 2, 3, 4, 5, 6),
                   product_type = c("Tennis ball", "Soccer ball", "Hockey puck",
                                    "Football", "Basketball", "Baseball"),
                   price = c(1.25, 22.25, 8.75, 15.25, 17.25, 7.25))

# View data
purchases

products

## Left Joins
# Bring in product information to purchases data
left_join(purchases, products, by = "product_id")

# Bring in purchase data to product table
left_join(products, purchases, by = "product_id")

# Bring in product prices only to purchases data
left_join(purchases, 
          products %>% select(product_id, price), 
          by = "product_id")

# Bring in product information to purchases data
right_join(products, purchases, by = "product_id")

# Bring in purchase data to product table
right_join(purchases, products, by = "product_id")

# All observations in products and purchases with all combinations
inner_join(products, purchases, by = "product_id")

# Same result, just ordered differently
inner_join(purchases, products, by = "product_id")

# Returning all observations in both tables
full_join(products, purchases, by = "product_id")

# All rows in products that also appeared in purchases (by product_id)
semi_join(products, purchases, by = "product_id")

# All rows in purchases that also appear in products (by product_id)
semi_join(purchases, products, by = "product_id")

# All rows in products that did not appear in purchases (by product_id)
anti_join(products, purchases, by = "product_id")

# All rows in purchases that did not appear in products (by product_id)
# We get an empty data frame in this case
anti_join(purchases, products, by = "product_id")
