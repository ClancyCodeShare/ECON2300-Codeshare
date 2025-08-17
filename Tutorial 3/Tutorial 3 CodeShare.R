#Clear the working Environment
rm(list = ls())
#Set the working directory
setwd("[your working directory here]")
getwd()


#Access necessary packages
install.packages(c("readr", "dplyr", "estimatr", "psych"))
library(readr) # package for fast read rectangular data
library(dplyr) # package for data manipulation
library(estimatr) # package for commonly used estimators with robust SE
library(psych) # package containing many functions useful for data analysis

#Question 1:

#Access Earnings and Height data:

EH = read_csv("Earnings_and_Height.csv")

#a)

#Running a regression of earnings on height:

reg1 = lm_robust(earnings~height, data = EH, se_type = "stata")
summary(reg1)

#b), c) repeating for female and male workers:

reg2 = lm_robust(earnings~height, data = subset(EH, sex == "0:female"), 
                 se_type = "stata")
summary(reg2)

reg3 = lm_robust(earnings~height, data = subset(EH, sex != "0:female"), 
                 se_type = "stata")
summary(reg3)

#d) use a difference in means test:

#H0: difference = 0
#H1: difference != 0

#Estimated difference:

#as.numeric coerces the output to just be a number, otherwise it prints it with
#the text "height".
est = reg3$coefficients[2] - reg2$coefficients[2]

#Standard error for each group:

se1 = reg2$std.error[2]
se2 = reg3$std.error[2]

#Calculate the standard error of the difference between means:

se_difference = sqrt(se1^2+se2^2)

#Now calculate the confidence interval and/or T score

CI = c(est - 1.96*se_difference, est + 1.96*se_difference)
T_score = est/se_difference

#Both CI and T_score agree (as they should) that we have a significant difference
#The CI does not cross 0 and the T_score is greater than 1.96 in absolute value
#so we have confidence at least at the 95% significance level.

#Question 2

#remove our previous environment
rm(list = ls())

#Load the Growth data

Growth = read_csv("Growth.csv")

#Run a regression of growth on tradeshare

reg1 = lm_robust(growth~tradeshare, data = subset(Growth, country_name != "Malta"),
                 se_type = "stata")
#a), b)

#Check the summary of the regression
summary(reg1)

#Checking T value, p value and confidence interval (although you only need to
#check one) we have:
#T = 1.942 < 1.96
#p = 0.5670 > 0.05
#CI crosses over null hypothesis value, 0

#All testing methods lead to the conclusion: Our estimated slope is not
#statistically significant at 5% or 1% levels

#c) We need our estimate and its standard error:

est = reg1$coefficients[2]
se = reg1$std.error[2]

#Use the relevant formula to calculate the confidence interval, with T = 2.58

CI = c(est - 2.58*se, est+2.58*se)

#See that this confidence interval crosses 0, so we do not have a statistically
#significant result at the 1% significance level


#Question 3
#Clear the environment
rm(list=ls())

#Load the birthweight data:

BW = read_csv("birthweight_smoking.csv")
#Attach the data so we dont have to type BW$birthweight
attach(BW)
#a)

describe(birthweight)

#Apply the describe function to birthweight for each different value of smoker:
tapply(birthweight, smoker, describe)

#b)
#We can calculate by hand:

estimate = mean(birthweight[smoker == 0]) - mean(birthweight[smoker == 1])

se_smoker = sd(birthweight[smoker == 1])/sqrt(sum(smoker == 1))
#sum(smoker == 1) will add up the number of observations that are smokers
se_non_smoker =  sd(birthweight[smoker == 0])/sqrt(sum(smoker == 0))

se = sqrt(se_smoker^2 + se_non_smoker^2)

#Now calculate the CI

CI = c(estimate - 1.96*se, estimate + 1.96*se)
CI
#Confidence interval does not cross 0 -> statistical significance at 5% level

#Alternatively, use t.test:

t.test(birthweight[smoker == 1], birthweight[smoker == 0])


#c)

reg1 = lm_robust(birthweight~smoker, data = BW, se_type = "stata")
summary(reg1)

#The magnitude of the slope coefficient is exactly equal to the difference
#between the means of birth weights for smokers and non-smokers.
#The same is true for the standard error (with some rounding)

#We can calculate by hand:

CI = c(reg1$coefficients[2] - 1.96*reg1$std.error[2],
       reg1$coefficients[2] + 1.96*reg1$std.error[2])
#See that we get the same confidence interval as before (although with a - sign
#because the difference has been taken the other way around)