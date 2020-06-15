# New Model with different predictors (feedback)
# https://www.coursera.org/learn/logistic-regression-r-public-health/supplement/cv6D3/feedback-on-the-new-model

# adding gender + location + frame + insurance + smoking 
model <- glm(dm ~ age + bmi + chol + hdl + systolic + diastolic + gender + location + frame + insurance + smoking, family = binomial(link = logit)) 

summary(model)

anova(model, test = "Chisq")