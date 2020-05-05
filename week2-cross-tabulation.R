# https://www.coursera.org/learn/logistic-regression-r-public-health/quiz/xXNHQ/cross-tabulation/attempt

g <- read.csv(file = "Data/final-diabetes-data-for-R-_csv_-_2_.csv",
              header = T,
              sep = ",")

colnames(g)

age <- g$age

library(tidyverse)
age_categorized <- case_when(age < 45 ~ "under 45",
                             age >= 45 & age < 65 ~ "45-64",
                             age >= 65 & age < 75 ~ "65-74",
                             age >= 75 ~ "75 or over",
                             TRUE ~ NA)
