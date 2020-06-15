# Model fit in logistic regression
# https://www.coursera.org/learn/logistic-regression-r-public-health/supplement/fxqKG/model-fit-in-logistic-regression

# McFadden’s r-squared:

# design your logistic regression 
full_model <- glm(dm ~ age + chol + insurance, family=binomial (link=logit)) 

# check your model 
summary(full_model) 

# run a null model 
null_model <- glm(dm ~ 1, family=binomial (link=logit)) 

# check 
summary(null_model)

# calculate McFadden's R-square 
R2 <- 1-logLik(full_model)/logLik(null_model) 

# print it 
R2
########

# c-statistic:

# install a package 
install.packages("DescTools")

# load package 
require(DescTools)

# design your logistic regression 
full_model <- glm(dm ~ age + chol + insurance, family=binomial (link=logit)) 

# check your model 
summary(full_model)

# generate the c-statistic 
Cstat(full_model)
###########

# Hosmer-Lemeshow statistic and test:

# H-L test 

# install package "ResourceSelection" 
install.packages("ResourceSelection") 
 
# load package 
require(ResourceSelection) 

# design your logistic regression 
full_model <- glm(dm ~ age + chol + insurance, family = binomial(link = logit)) 

full_model$y

# run Hosmer-Lemeshow test 
HL <- hoslem.test(x = full_model$y, y = fitted(full_model), g = 10) 
HL

# plot the observed vs expected number of cases for each of the 10 groups 
plot(HL$observed[,"y1"], HL$expected[,"yhat1"])

# plot the observed vs expected number of noncases for each of the 10 groups 
plot(HL$observed[,"y0"], HL$expected[,"yhat0"])

# plot observed vs. expected prevalence for each of the 10 groups 
plot(x = HL$observed[,"y1"]/(HL$observed[,"y1"]+HL$observed[,"y0"]), 
     y = HL$expected[,"yhat1"]/(HL$expected[,"yhat1"]+HL$expected[,"yhat0"]))

# OR

# install package("generalhoslem") 
install.packages("generalhoslem")

# load package 
require(generalhoslem)

# run Hosmer-Lemeshow test 
logitgof(obs = full_model$y, exp = fitted(full_model), g = 10)
