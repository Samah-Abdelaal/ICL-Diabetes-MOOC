# https://www.coursera.org/learn/logistic-regression-r-public-health/supplement/g9Ujb/how-to-describe-data-in-r

g <- read.csv(file = "Data/final-diabetes-data-for-R-_csv_-_2_.csv",
              header = T,
              sep = ",")

dim(g)

# to check variable names
colnames(g)
dimnames(g)[[2]]

# assigning variables

chol <- g["chol"] # continuous

gender <- as.factor(g[, "gender"]) # categorical
dm <- as.factor(g[, "dm"]) # categorical


t <- table(gender)

addmargins(t) #adding total to the table

round(prop.table(t), digits = 3)

round(100*prop.table(t), digits = 1)

dm2 <- factor(dm, exclude = NULL) # to include NAs in categorical variables
table(dm2)

summary(chol) # NAs are included by default

height <- g[, "height"]
weight <- g[, "weight"]

summary(height)
summary(weight)

# height and weight are in inches and pounds

# to transform into SI units
height.si <- height*0.0254
weight.si <- weight*0.453592

bmi <- weight.si / height.si ^ 2
summary(bmi)

# categorizing bmi
bmi_categorised <- ifelse(bmi < 18.5, "underweight", 
                          ifelse(bmi >= 18.5 & bmi <= 25, "normal", 
                                 ifelse(bmi > 25 & bmi <= 30, "overweight", 
                                        ifelse(bmi > 30, "obese", NA)))) 

# check that the bmi_categorised variable has worked  
table(bmi_categorised, exclude = NULL) 

# frequencies of diabetes by BMI category 
dm_by_bmi_category <- table(bmi_categorised, dm2, exclude = NULL) 

# check 
dm_by_bmi_category 

# with the row percentages 
round(100 * prop.table(dm_by_bmi_category, margin = 1), digits = 1) 
