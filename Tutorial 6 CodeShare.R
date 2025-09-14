install.packages("multcomp")

library(readr) # package for fast read rectangular data
library(dplyr) # package for data manipulation
library(ggplot2) # package for elegant data visualisations
library(estimatr) # package for commonly used estimators with robust SE
library(texreg) # package converting R regression output to LaTeX/HTML tables
library(car) # package for functions used in "An R Companion to Applied Regression"
library(multcomp) # package for simultaneous tests and CIs for general linear hypotheses

rm(list = ls())
setwd("[Your working directory here]")
getwd()

LM = read_csv("lead_mortality.csv")

#Question 1

#a)
n_lead = sum(LM$lead == 1)
n_no_lead = sum(LM$lead == 0)

overall_mean= mean(LM$infrate)
lead_mean= mean(LM$infrate[LM$lead == 1])
no_lead_mean= mean(LM$infrate[LM$lead == 0])

se_lead = sd(LM$infrate[LM$lead ==1 ])/sqrt(n_lead)
se_no_lead = sd(LM$infrate[LM$lead == 0])/sqrt(n_no_lead)

difference = lead_mean-no_lead_mean
se_difference = sqrt(se_lead^2 + se_no_lead^2)
CI = c(difference - 1.96 * se_difference,
       difference + 1.96 * se_difference)

#Confidence interval crosses 0, therefore the difference
#is not statistically significant
#-> cannot reject null hypothesis

#b)

LM = read_csv("lead_mortality.csv") %>%
  mutate(lead_ph = lead*ph)

reg1 = lm_robust(infrate ~ lead + ph + lead_ph, data = LM,
                 se_type = "stata")

fig8.1 <- ggplot(LM, aes(x = ph, y = infrate, col = as.factor(lead))) +
  labs(title = "Figure 1: Infant Mortality Rate and pH Value",x = "PH Value",
       y = "Infant Mortality Rate") +
  geom_smooth(data = LM, method = "lm", se = FALSE, size = 1) +
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, size = 12, family = "serif",
                                  face = "bold"),
        legend.position = c(0.9, 0.8),
        legend.title = element_blank(),
        legend.text = element_text(family = "serif", face = "bold"),
        legend.key = element_rect(color = "transparent"),
        legend.background = element_rect(fill = "lightgrey",
                                         size = 0.8,
                                         linetype="solid")) +
  scale_color_discrete(name = "Lead", labels = c(" lead = 0", " lead = 1"))
print(fig8.1)

linearHypothesis(reg1, c("lead = 0", "lead_ph = 0"), test=c("F"))

#We have a statistically significant result for the joint hypothesis.

summary(reg1)

#The interaction term coefficient is statistically significant at
#the 5% significance level, so there is a statistically significant 
#interaction between the effect of lead and ph on infrate. That means
#we have evidence that the effect of lead depends on ph, and vice versa

B_lead = reg1$coefficients[2]
B_lead_ph = reg1$coefficients[4]

ph_mean = mean(LM$ph)
effect_1 = B_lead + B_lead_ph * ph_mean


ph_mean_plus = mean(LM$ph) + sd(LM$ph)
effect_2 = B_lead + B_lead_ph * ph_mean_plus

ph_mean_minus = mean(LM$ph) - sd(LM$ph)
effect_3 = B_lead + B_lead_ph * ph_mean_minus

#Two approaches:

#Approach 1:

#Restructure the regression model so that one of the coefficients
#is equal to the effect of lead when ph = 6.5

#To do this, add and subtract B_3 * 6.5 * lead from the regression model.
#You get:

#infrate = B_0 + (B_1 + 6.5*B_3)*lead + B_2*ph + B_3(lead*ph - 6.5*lead)
#infrate = B_0 + C*lead + B_2*ph + B_3*lead*(ph- 6.5)

#See that when ph = 6.5, the only effect of lead is from the coefficient
#C, so a confidence interval on C is a confidence interval on the effect
#of lead when ph = 6.5

#We can this confidence interval from the regression output for 
#the above regression formula:

LM <- read_csv("lead_mortality.csv") %>%
  mutate(lead_ph = lead*ph, lead_ph_65 = lead*(ph - 6.5))

reg2 = lm_robust(infrate ~ lead + ph + lead_ph_65, data = LM, se_type = "stata")
summary(reg2)

#Alternatively, use functions in rStudio to access this confidence
#interval. Check that the answers match. (They won't match to every
#decimal place due to rounding/different calculation methods)

confint(glht(reg1, linfct = c("lead + 6.5*lead_ph = 0")))

#Question 2

rm(list = ls())



#Attaching the data using the mutate command to add various
#non-linear terms that we will include in the regressions
CPS12 <- read_csv("cps12.csv") %>%
  mutate(ln_ahe = log(ahe),
         ln_age = log(age),
         age2 = age*age,
         fem_bac = female*bachelor,
         fem_age = female*age,
         fem_age2= female*age2,
         bac_age = bachelor*age,
         bac_age2= bachelor*age2)
attach(CPS12)

#Running each regression asked for in the question:
reg1 = lm_robust(ahe ~ age + female + bachelor, data = CPS12, se_type = "stata")
reg2 = lm_robust(ln_ahe ~ age + female + bachelor, data = CPS12, se_type = "stata")
reg3 = lm_robust(ln_ahe ~ ln_age + female + bachelor, data = CPS12, se_type = "stata")
reg4 = lm_robust(ln_ahe ~ age + age2 + female + bachelor,
                 data = CPS12, se_type = "stata")
reg5 = lm_robust(ln_ahe ~ age + age2 + female + bachelor + fem_bac,
                 data = CPS12, se_type = "stata")
reg6 = lm_robust(ln_ahe ~ age + age2 + fem_age + fem_age2 + female +
                   bachelor + fem_bac, data = CPS12, se_type = "stata")
reg7 = lm_robust(ln_ahe ~ age + age2 + bac_age + bac_age2 + female +
                   bachelor + fem_bac, data = CPS12, se_type = "stata")
reg8 = lm_robust(ln_ahe ~ age + age2 + fem_age + fem_age2
                 + bac_age + bac_age2 + female + bachelor + fem_bac,
                 data = CPS12, se_type = "stata")

#Command to display a table in RStudio, you may need to zoom out and/or drag
#your console pane to cover more of the screen for it to fit.

#Can use texreg instead to output latex code if you prefer.
screenreg(list(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8),
          include.ci = F, caption.above = T,
          digits = 3, caption = "Earnings and Age, 2012",
          custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)"))

#a)

#For reg1, if age increases from 25 to 26 or from 33 to 34, the effect is the same,
#as we have a purely linear model, where changes do not depend on the value of
#the data.

summary(reg1)
#For any age, the predicted effect of an extra year is +0.5103

#b)

#This regression is a log - linear model. The coefficient on age
#is the (approximate) predicted percentage change in earnings for a unit
#change in age.

#Again, the age term is linear, so the effect of increasing age does not
#depend on the size of age. For either age increase, the effect is an increase
#by (1 + 0.026) times, or by 2.6%

#c)

#For the log- log model, the effect of an increase in age will depend on the
#absolute size of age. This model predicts the change in log(earnings) for a
#change in log(age), which is approximately the percentage change in earnings,
#for a percentage change in age.

#We can work out the percentage change in age using the log approximation:

log(26)-log(25)

log(34)-log(33)

#And we should get a similar result by just calculating percentage change:

(26-25)/25

(34-33)/33

#Then multiply these by 0.753 for the predicted proportional increase in earnings
#for the corresponding proportional increase in age:


0.753 * (26-25)/25

0.753 * (34-33)/33

#(multiply these by 100% if you want it as a percentage)

#d) We have a nonlinear term of age so the effect of increasing
#age will depend on the absolute level of age. Age no longer
#has log terms, but we still have log earnings as the dependent variable.

#In this case we will have to calculate out the effect rather than observing
#directly off the regression.

delta_log_y_1 = reg4$coefficients[2]*(26) - reg4$coefficients[2]*(25) +
  reg4$coefficients[3]*26^2 - reg4$coefficients[3]*25^2

delta_log_y_2 = reg4$coefficients[2]*(35) - reg4$coefficients[2]*(34) +
  reg4$coefficients[3]*35^2 - reg4$coefficients[3]*34^2

#(note we could simplify the expression by factoring the linear terms)

#No code for next sections, check blackboard answers for detailed explanations