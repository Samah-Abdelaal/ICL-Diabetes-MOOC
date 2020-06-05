# Multiple Logistic Regression
# https://www.coursera.org/learn/logistic-regression-r-public-health/supplement/Y25Ub/feedback-multiple-regression-model

g <- read.csv(file = "Data/final-diabetes-data-for-R-_csv_-_2_.csv",
              header = T,
              sep = ",")

dm <- as.factor(g[, "dm"])
age <- g[, "age"]
gender <- as.factor(g[, "gender"])

h <- g[, "height"]
w <- g[, "weight"]

bmi_func <- function(h, w){
  bmi <- (w * 0.453592) / ((h * 0.0254) ^ 2)
  bmi_categorised <- ifelse(bmi < 18.5, "underweight",
                            ifelse(bmi >= 18.5 & bmi <= 25, "normal",
                                   ifelse(bmi > 25 & bmi <= 30, "overweight",
                                          ifelse(bmi > 30, "obese", NA))))
  print(table(bmi_categorised, exclude = NULL))
  }
bmi_func(h, w)

m <- glm(dm ~ age + gender + bmi,
         family = binomial(link = "logit"))
summary(m)

exp(confint(m))

# Week 3 Quiz
# https://www.coursera.org/learn/logistic-regression-r-public-health/supplement/IcsGM/feedback-on-the-assessment

dm <- as.factor(g[, "dm"])
age <- g[, "age"]
chol <- g[, "chol"]
insurance <- as.factor(g[, "insurance"])

m <- glm(dm ~ age + chol + insurance,
         family = binomial(link = "logit"))
summary(m)

exp(m$coefficients)

exp(confint(m))
