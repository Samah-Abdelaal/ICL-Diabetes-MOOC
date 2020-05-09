# https://www.coursera.org/learn/logistic-regression-r-public-health/quiz/xXNHQ/cross-tabulation/attempt

g <- read.csv(file = "Data/final-diabetes-data-for-R-_csv_-_2_.csv",
              header = T,
              sep = ",")

colnames(g)

age <- g$age
gender <- g$gender

library(tidyverse)
age_categorized <- case_when(age < 45 ~ "under 45",
                             age >= 45 & age < 65 ~ "45-64",
                             age >= 65 & age < 75 ~ "65-74",
                             age >= 75 ~ "75 or over",
                             TRUE ~ NA)

# Error: must be a character vector, not a logical vector !!!

age_categorized <- ifelse(age < 45, "< 45",
                          ifelse(age >= 45 & age < 65, "45-64",
                                 ifelse(age >= 65 & age < 75, "65-74",
                                        ifelse(age >= 75, "75 or over", NA))))
table(age_categorized, exclude = NULL)
# no. of females aged under 45 is 126
# no missing values

age_by_gender <- table(age_categorized, gender, exclude = NULL)

round(100*prop.table(age_by_gender, margin = 2))
# pct of ALL PEOPLE who are male and aged 65-75 ia 5%

# CORRECT ANSWER
# https://www.coursera.org/learn/logistic-regression-r-public-health/supplement/Rwd4X/results-of-cross-tabulation

##### Here is the R code to do the cross-tabulations and the resulting output 

# creating "age" variable 
age <- g[,"age"] 

# creating a categorical variable "age_grouped" 
age_grouped <- ifelse(age < 45, "under 45", 
                      ifelse(age >= 45 & age < 65, "45 - 64",  
                             ifelse(age >= 65 & age < 75, "65 - 74",  
                                    ifelse(age >= 75, "75 or over", NA)))) 

# displaying new variable in a table 
table(age_grouped, exclude = NULL) 

## age_grouped 
##    45 - 64    65 - 74 75 or over   under 45  
##        139         41         23        200 

# cross tabulating with gender 
age_group_by_gender <- table(age_grouped, gender, exclude = NULL) 

# display the cross tabulation 
age_group_by_gender 

##             gender 
## age_grouped  female male 
##   45 - 64        75   64 
##   65 - 74        21   20 
##   75 or over     12   11 
##   under 45      126   74 

# display the cross tabulation as proportion of whole sample, converting to percentage and rounding to 1 decimal place 
round(100 * prop.table(age_group_by_gender), digits = 1) 

##             gender 
## age_grouped  female male 
##   45 - 64      18.6 15.9 
##   65 - 74       5.2  5.0 
##   75 or over    3.0  2.7 
##   under 45     31.3 18.4 

# displaying the age frequencies by gender 
round(100 * prop.table(age_group_by_gender, margin = 2), digits = 1) 

##             gender 
## age_grouped  female male 
##   45 - 64      32.1 37.9 
##   65 - 74       9.0 11.8 
##   75 or over    5.1  6.5 
##   under 45     53.8 43.8 

# initialising the age_grouped vector by copying the already existing age vector 
age_grouped <- age 


# below says: if age < 45, then label the value "under 45", if not, then keep it what it already was in age_grouped 
age_grouped <- ifelse(age < 45, "under 45", age_grouped) 

# repeat for the other categories 
age_grouped <- ifelse(age >= 45 & age < 65, "45 - 64", age_grouped) 
age_grouped <- ifelse(age >= 65 & age < 75, "65 - 74", age_grouped)  
age_grouped <- ifelse(age >= 75, "75 or over", age_grouped) 



# check that things make sense 
table(age_grouped, exclude = NULL) 

## age_grouped 
##    45 - 64    65 - 74 75 or over   under 45  
##        139         41         23        200 

# optional extra check for the extra cautious! 
head(cbind(age_grouped, age)) 

##      age_grouped age  
## [1,] "45 - 64"   "46" 
## [2,] "under 45"  "29" 
## [3,] "45 - 64"   "58" 
## [4,] "65 - 74"   "67" 
## [5,] "45 - 64"   "64" 
## [6,] "under 45"  "34" 