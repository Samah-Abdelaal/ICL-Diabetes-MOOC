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