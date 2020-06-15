# Applying Backwards Elimination (with feedback)
# https://www.coursera.org/learn/logistic-regression-r-public-health/supplement/xTHNo/feedback-backwards-elimination

g <- read.csv(file = "Data/final-diabetes-data-for-R-_csv_-_2_.csv",
              header = T,
              sep = ",")

##### Make the variables and run the models #####

dm <- as.factor(g[, "dm"])
age <- g[, "age"]

h <- g[, "height"]
w <- g[, "weight"]

bmi <- (w * 0.453592) / ((h * 0.0254) ^ 2)

chol <- g[, "chol"]
hdl <- g[, "hdl"]
systolic <- g[, "bp.1s"]
diastolic <- g[, "bp.1d"]

model <- glm(dm ~ age + bmi + chol + hdl + systolic + diastolic,
         family = binomial(link = "logit"))
summary(model)

anova(model, test = "Chisq")

# neither of the BP variables is significantly associated
# with the odds of being diagnosed with diabetes

model <- glm(dm ~ age + bmi + chol + hdl,
             family = binomial(link = "logit"))
summary(model)

# Have any of the coefficients for the four
# remaining variables changed? Not much, which is good
###
# strange that systolic and diastolic are not significant... 
# find out is to see if it correlates with other variables

cor.test(systolic, hdl) # not significant
cor.test(systolic, bmi) # significant
cor.test(systolic, chol) # very significant
cor.test(systolic, age) # extremely significant

# So systolic BP correlates weakly (but statistically significantly) with cholesterol
# and moderately (and also statistically significantly) with age.
####

# As an exercise, you can try leaving age out of the model:
# is systolic BP significant now?

model <- glm(dm ~ bmi + chol + hdl + systolic + diastolic,
             family = binomial(link = "logit"))
summary(model)
