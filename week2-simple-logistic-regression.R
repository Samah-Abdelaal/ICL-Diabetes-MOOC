# https://www.coursera.org/learn/logistic-regression-r-public-health/supplement/YPw6w/practice-in-r-simple-logistic-regression
# https://www.coursera.org/learn/logistic-regression-r-public-health/supplement/TkoUt/feedback-output-and-interpretation-from-simple-logistic-regression

g <- read.csv(file = "Data/final-diabetes-data-for-R-_csv_-_2_.csv",
              header = T,
              sep = ",")

# assigning variables

dm <- as.factor(g[, "dm"]) # categorical
gender <- as.factor(g[, "gender"]) # categorical
age <- g[, "age"] # continuous

###################

# Null model (empty)

m <- glm(dm ~ 1,
         family = binomial(link = logit))
summary(m)

# Checking R's interpretation of the binary outcome

table(m$y)


# Interpreting the model

odds <- exp(-1.7047) # the odds of having dm are 0.182

p <- odds/(1 + odds) # each participant has a 0.15 (15%) probability of having dm

# confirming the numbers

table(dm)

60/330 #odds (ratio of success to failure) = 0.182
60/(330+60) # p = 0.15 (15%)

##################

# Simple logistic model (one predictor)

# categorical

m <- glm(dm ~ gender,
         family = binomial(link = logit))
summary(m)


# interpreting the model

(odds <- exp(0.08694)) # odds = 1.09 (usually reported to 2 decimal places)

(p <- odds / (1 + odds)) # probability = 0.51


# checking R's interpretation of gender

contrasts(gender)

# This confirms that the coefficient given in the output refers to male
# because males have a 1 next to them in the above output and females have a zero.
# The log odds for females are incorporated into the intercept.

# R will by default organise values (called levels) of categorical variables
# alphabetically. You can check the order like so:

levels(gender) # by default, female is the first level (reference group)

# to redefine the reference group

gender <- relevel(gender, ref = "male")

levels(gender) # male is the first level (reference group)


# rerun the model

m <- glm(dm ~ gender,
         family = binomial(link = logit))
summary(m)

# the coefficient is now negative because:
# log(A/B) = - log(B/A)


m$coefficients

exp(m$coefficients)

################################

# continuous

m <- glm(dm ~ age,
         family = binomial(link = logit))
summary(m)


# interpreting the model

# the intercept (the log odds of having dm at birth"age = 0")

(odds <- exp(-4.404530)) # odds = 0.01222

(p <- odds / (1 + odds)) # probability = 0.01207

# When odds are SMALL, they are very close to probability

# the age coefficient (the log odds of having dm for each one-year increase in age)

(odds <- exp(0.052465)) # odds = 1.05


# Mathematically, the difference between 2 log odds is the same as the ratio between them

# example: log odds of 25 and 24 years old

(log_24 <- -4.404530 + (24*0.052465))
(log_25 <- -4.404530 + (25*0.052465))

log_25 - log_24
log_25 / log_24 
# which (when exponentiated) produces the odds ratio:
# the amount by which your odds increases when you get a year older

# NOT TRUE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#############################

# checking the linearity assumption

# create a cross tabulation of age and diabetes status

dm_by_age <- table(age, dm)

# output the frequencies of diabetes status by age

freq_table <- prop.table(dm_by_age, margin = 1)

# calculate the odds of having diabetes

odds <- freq_table[, "yes"]/freq_table[, "no"]

# calculate the log odds

logodds <- log(odds)

# plot the ages found in the sample against the log odds of having diabetes
plot(row.names(freq_table), logodds)

# Note to self: you cannot plot age directly against logodds because they have different length

nrow(freq_table)
nrow(g)
