# https://www.coursera.org/learn/logistic-regression-r-public-health/supplement/SbowS/practice-in-r-describing-variables
# For this activity, I’d like you to consider not just age and gender
# but also BMI, HDL and cholesterol.
# Have a go at summarising each of them separately – numerically and with plots.

g <- read.csv(file = "Data/final-diabetes-data-for-R-_csv_-_2_.csv",
              header = T,
              sep = ",")

# assigning variables

dm <- g$dm
gender <- g$gender
bmi <- (g$weight * 0.453592) / ((g$height * 0.0254) ^ 2)
hdl <- g$hdl
chol <- g$chol

# summarization

table(dm, exclude = NULL)

table(gender)
summary(bmi)
summary(hdl)
summary(chol)

# visualization

hist(bmi)
plot(density(bmi, na.rm = T), main = "")

hist(hdl)
plot(density(hdl, na.rm = T), main = "")

hist(chol)
plot(density(chol, na.rm = T), main = "")

# relation with dm

# numerically

table(gender, dm)

bmi_categorised <- ifelse(bmi < 18.5, "underweight",
                          ifelse(bmi >= 18.5 & bmi <= 25, "normal",
                                 ifelse(bmi > 25 & bmi <= 30, "overweight",
                                        ifelse(bmi > 30, "obese", NA)))) 

table(bmi_categorised, dm)

# visually
# bmi
bmi_by_dm <- table(bmi, dm)

freq_table <- prop.table(bmi_by_dm, margin = 1)

odds <- freq_table[, "yes"] / freq_table[, "no"]
logodds <- log(odds)

plot(rownames(freq_table), logodds)

# hdl
hdl_by_dm <- table(hdl, dm)

freq_table <- prop.table(hdl_by_dm, margin = 1)

odds <- freq_table[, "yes"] / freq_table[, "no"]
logodds <- log(odds)

plot(rownames(freq_table), logodds)

# chol
chol_by_dm <- table(chol, dm)

freq_table <- prop.table(chol_by_dm, margin = 1)

odds <- freq_table[, "yes"] / freq_table[, "no"]
logodds <- log(odds)

plot(rownames(freq_table), logodds)

########################################

# FEEDBACK

# Shape of the distributions of the five variables

summary(chol)

> summary(chol)

Min. 1st Qu. Median Mean 3rd Qu. Max. NA's

78.0 179.0 204.0 207.8 230.0 443.0 1

chol.no.na <- chol[is.na(chol)==0]

d <- density(chol.no.na)

plot(d,main = "") 

This looks reasonably normal. Here’s the same plot for HDL:

Another that’s a bit skewed to the right. Here’s the one for BMI:


# Assessing crude relations between predictors and the outcome

# define the gender variable 
gender <- as.factor(g[,"gender"]) 
 
# cross tabulation 
dm_by_gender <- table(gender, dm) 
# not including NA values because there aren't that many 

# proportion of diabetes status by gender 
dm_by_gender_prop <- prop.table(dm_by_gender, margin = 1) 

# calculate the odds of having diabetes by gender 
odds_gender <- dm_by_gender_prop[, "yes"]/dm_by_gender_prop[, "no"] 

# calculate the log odds 
logodds_gender <- log(odds_gender) 

# plot the log odds of having diabetes by gender 
dotchart(logodds_gender)
# Here is the dot chart for gender. It’s not very useful.
# This next chart draws lines instead of dots and it’s also not very useful.
plot(as.factor(names(logodds_gender)), logodds_gender) 

# Now, plot the relation between age and the outcome.
# The first one plots age by the individual year and
# the second one puts it into four groups.

# define the age variable (continuous) 
age <- age <- g[,"age"] 


# create a cross tabulation of age and diabetes status  
dm_by_age <- table(age, dm) # not including NA values because there aren't that many 

# output the frequencies of diabetes status by age 
dm_by_age_prop <- prop.table(dm_by_age, margin = 1) 

# calculate the odds of having diabetes 
odds_age <- dm_by_age_prop[, "yes"]/dm_by_age_prop[, "no"] 

# calculate the log odds 
logodds_age <- log(odds_age) 

# plot the ages found in the sample against the log odds of having diabetes 
plot(rownames(dm_by_age_prop), logodds_age) 

# age grouping converting continuous variable to a categorical (ordinal) one  
age_grouped <- ifelse(age < 45, "under 45", 
                      ifelse(age >= 45 & age < 65, "45 - 64",  
                             ifelse(age >= 65 & age < 75, "65 - 74",  
                                    ifelse(age >= 75, "75 or over", NA)))) 

age_grouped <- factor(age_grouped, levels = c("under 45", "45 - 64", "65 - 74", "75 or over")) 



# create a cross tabulation of age and diabetes status  
dm_by_age_grouped <- table(age_grouped, dm) 

# output the frequencies of diabetes status by age 
age_grouped_prop <- prop.table(dm_by_age_grouped, margin = 1) 

# calculate the odds of having diabetes 
odds_age_grouped <- age_grouped_prop[, "yes"]/age_grouped_prop[, "no"] 

# calculate the log odds 
logodds_age_grouped <- log(odds_age_grouped) 

# plot the age groups found in the sample against the log odds of having diabetes 
dotchart(logodds_age_grouped) 

# Now let’s plot the relation between cholesterol and the outcome.
# The first one plots cholesterol by individual value and
# the second one puts it into three groups.

# define chol as a continuous variable 
chol <- g[,"chol"] 


# create a cross tabulation of cholesterol and diabetes status  
dm_by_chol <- table(chol, dm) # not including NA values because there aren't that many 

# output the frequencies of diabetes status by cholesterol 
dm_by_chol_prop <- prop.table(dm_by_chol, margin = 1) 

# calculate the odds of having diabetes 
odds_chol <- dm_by_chol_prop[, "yes"]/dm_by_chol_prop[, "no"] 

# calculate the log odds 
logodds_chol <- log(odds_chol) 

# plot the cholesterol found in the sample against the log odds of having diabetes 
plot(rownames(dm_by_chol_prop), logodds_chol, xlim=c(150, 300)) 

# categorising chol into an ordinal variable 

# https://www.medicalnewstoday.com/articles/315900.php 
chol_categorised <- ifelse(chol < 200, "healthy",  
                           ifelse(chol < 240, "borderline high", 
                                  ifelse(chol >= 240, "high", NA))) 

# make sure that it is treated as a factor/categorical variable and ordering the levels within the factor for the table 
chol_categorised <- factor(chol_categorised, levels = c("healthy", "borderline high", "high")) 




# create a cross tabulation of cholesterol and diabetes status  
dm_by_chol_categorised <- table(chol_categorised, dm) # not including NA values because there aren't that many 

# output the frequencies of diabetes status by cholesterol 
dm_by_chol_categorised_prop <- prop.table(dm_by_chol_categorised, margin = 1) 

# calculate the odds of having diabetes 
odds_chol_categorised <- dm_by_chol_categorised_prop[, "yes"]/dm_by_chol_categorised_prop[, "no"] 

# calculate the log odds 
logodds_chol_categorised <- log(odds_chol_categorised) 

# plot the cholesterol categories found in the sample against the log odds of having diabetes 
dotchart(logodds_chol_categorised)

# You can do the same thing for HDL as we have just done for cholesterol.


# Here is the code to show the relation between BMI and diabetes.
# This puts BMI into four categories.

#bmi 
height <- g[,"height"] 
weight <- g[,"weight"] 
height.si <- height*0.0254 
weight.si <- weight*0.453592 
bmi <- weight.si/height.si^2 


# categorising BMI 

bmi_categorised <- ifelse(bmi < 18.5, "underweight", 
                          ifelse(bmi >= 18.5 & bmi <= 25, "normal", 
                                 ifelse(bmi > 25 & bmi <= 30, "overweight", 
                                        ifelse(bmi > 30, "obese", NA)))) 

# make sure that it is treated as a factor/categorical variable and ordering the levels within the factor for the table 
bmi_categorised <- factor(bmi_categorised, levels = c("underweight", "normal", "overweight","obese")) 

# create a cross tabulation of BMI and diabetes status  
dm_by_bmi_categorised <- table(bmi_categorised, dm) # not including NA values because there aren't that many 

# output the frequencies of diabetes status by BMI 
dm_by_bmi_categorised_prop <- prop.table(dm_by_bmi_categorised, margin = 1) 

# calculate the odds of having diabetes 
odds_bmi_categorised <- dm_by_bmi_categorised_prop[, "yes"]/dm_by_bmi_categorised_prop[, "no"] 

# calculate the log odds 
logodds_bmi_categorised <- log(odds_bmi_categorised) 

# plot the BMI categories found in the sample against the log odds of having diabetes 
dotchart(logodds_bmi_categorised) 

# Note that in the above graph, there is no dot for underweight.


#  To calculate the Pearson correlation coefficient between two continuous,
# (roughly) normally distributed variables in R, we can type:

cor.test(x=chol,y=hdl,method= "pearson") 

Pearson's product-moment correlation 
data:  chol and hdl 

t = 3.7983, df = 400, p-value = 0.0001683 

alternative hypothesis: true correlation is not equal to 0 

95 percent confidence interval: 

 0.09042379 0.27929136 

sample estimates: 

      cor  

0.1865809

# This code excludes patients with missing data and tells us that
# cholesterol and HDL are indeed correlated (p=0.00017) but only weakly
# (r=0.19 to two decimal places). You can happily try both of those in the model.
# For the two blood pressure values, however, the Pearson correlation coefficient
# is 0.60, which is a bit too high for comfort. You’d probably be best
# to try only one of the two at a time rather than trying to include both.