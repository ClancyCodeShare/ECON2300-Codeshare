rm(list = ls())
#setwd("")
getwd()

install.packages(c("plm","fastDummies"))

library(readr) # package for fast read rectangular data
library(dplyr) # package for data manipulation
library(estimatr) # package for commonly used estimators with robust SE
library(texreg) # package converting R regression output to LaTeX/HTML tables
library(plm) # package for estimating linear panel data models
library(fastDummies) # package for creating dummy/indicator variables

Guns <- read_csv("Guns.csv") %>%
  mutate(lvio = log(vio), lrob = log(rob), lmur = log(mur))
attach(Guns)
View(Guns)
#(a)

#Pooled OLS, with cluster and heteroskedasticity robust standard errors:
#(cluster robust standard errors correct for the fact that errors
#might correlate with entities at different times, eg. high inflation
#in a particular location correlates with high inflation in that location
#next month.)

#In the most basic form of linear regression - we would assume that errors
#are identical and independent (in terms of their distribution).
#This makes calculating standard errors very simple.
#Heteroskedasticity is a concern because it means that our errors aren't identical.

#Clustered errors violate independence, and so just like we need to use robust
#standard errors to get unbiased standard when heteroskedasticity is present,
#we do the same in the presence of clustered errors.
pols1 = lm_robust(lvio ~ shall, data = Guns, se_type = "stata")
pols2 = lm_robust(lvio ~ shall + incarc_rate + density + avginc +
                    pop + pb1064 + pw1064 + pm1029,
                  data = Guns, se_type = "stata")

screenreg(list(pols1, pols2), include.ci = FALSE,
          custom.model.names = c("Just Shall", "Control Vars"),
          custom.gof.rows = list(SER = c("SER1", "SER2")),
          custom.gof.names = c("R^2", "Adj R^2", "Number obs", "SER"),
          )


#(b) Adding fixed state effects
#DON'T TRUST THE STANDARD ERRORS REPORTED YET! PLM DOES NOT CALCULATE
#CLUSTER ROBUST STANDARD ERRORS, WE HAVE TO GET THEM MANUALLY!
fe1 = plm(lvio ~ shall + incarc_rate + density + avginc +
            pop + pb1064 + pw1064 + pm1029,
          data = Guns, model = "within", index = c("stateid", "year"),
          effect = "individual")
#state based effects, fe regesssion ^

#Calculating the cluster robust standard errors:

SE.fe1 <- sqrt(diag(vcovHC(fe1, type = "sss", cluster = "group")))

#(sss applies the small sample correction as when we usually use "stata")
#vcovHC calculates an HAC variance covariance matrix for our regression,
#then diag extracts the diagonal elements (the variances) and sqrt takes
#their square roots, to give the standard errors.

#We can override the standard error values using override.se in screenreg,
#we will need to give standard errors for the pols1 and pols2 models even
#though they are already correct since we are overriding all of the SEs.
#The same will apply for the p values

# extract SE of pooled OLS estimator
SE.pols1 <- pols1$std.error
SE.pols2 <- pols2$std.error
# compute p-values
p.pols1 <- 2*(1 - pnorm(abs(pols1$coefficients/SE.pols1)))
p.pols2 <- 2*(1 - pnorm(abs(pols2$coefficients/SE.pols2)))
p.fe1 <- 2*(1 - pnorm(abs(fe1$coefficients/SE.fe1)))

screenreg(list(pols1, pols2, fe1),
          override.se = list(SE.pols1, SE.pols2, SE.fe1),
          override.pvalues = list(p.pols1, p.pols2, p.fe1),
          include.ci = FALSE,
          stars = c(0.01, 0.05, 0.1),
          custom.header = list("Pooled" = 1:2,
                               "Fixed Effects" = 3),
          custom.model.names = c("(1) Pooled OLS (1)", "(2) Pooled OLS",
                                 "(3) Fixed Effects"),
          digits = 4
          )

#SER vs RMSE:

#SER = sqrt(SSE/(n-k-1))
#RMSE = sqrt(SSE/(n))

errors.pols1 = pols1$fitted.values - lvio
squared.errors = errors.pols1^2
SSE = sum(squared.errors)

pols1$nobs
pols1$df.residual
n = 1173
n_minus_k_minus_1 = pols1$df.residual
RMSE = sqrt(SSE/n)
#0.6168263
SER = sqrt(SSE/n_minus_k_minus_1)
#0.6173528

#c) Estimating the model with fixed state and time effects:
fe2 = plm(lvio ~ shall + incarc_rate + density + avginc +
            pop + pb1064 + pw1064 + pm1029,
          data = Guns, model = "within", index = c("stateid", "year"),
          effect = "twoway")
SE.fe2 <- sqrt(diag(vcovHC(fe2, type="sss", cluster="group")))
p.fe2 <- 2*(1 - pnorm(abs(fe2$coefficients/SE.fe2)))
#entity and time based effects included by setting effect = "twoway"

screenreg(list(pols1, pols2, fe1, fe2),
          override.se = list(SE.pols1, SE.pols2, SE.fe1, SE.fe2),
          override.pvalues = list(p.pols1, p.pols2, p.fe1, p.fe2),
          include.ci = FALSE,
          custom.model.names = c("(1) Pooled OLS (1)", "(2) Pooled OLS",
                                 "(3) Fixed Effects", "(4) Fixed Effects & Time Effects"),
          digits = 5)

#You might also include time effects by simply adding dummy variables for each
#year to the model fe1:

fe3 = plm(lvio ~ shall + incarc_rate + density + avginc +
            pop + pb1064 + pw1064 + pm1029 + factor(year),
          data = Guns, model = "within", index = c("stateid", "year"),
          effect = "individual")

#Can also use fast dummies to replicate plm (but less
#efficiently). We can drop the year and stateid columns since they
#are redundent given we now have dummy variables for them, and 
#we do not want to include them as well as the dummy variables.

Guns.v2 = dummy_cols(Guns, select_columns = c("stateid", "year")) %>%
  select(-year, -stateid)
View(Guns.v2)

#Ideally we don't want to have to type out each of the many dummy variables
#into a regression. Instead we can use a . to run a regression on all
#of the variables in the data and either index our data to only the
#desired columns, or use a - to remove the unwanted variables from the regression:
#We can put a -1 after the . to remove the intercept term from our
#regression, and doing so means we no longer have to worry about
#dummy variable traps (these only cause perfect multicolinearity
#if we have a constant coefficient)
fe1.1 = lm_robust(lvio ~ .-1, data = Guns.v2[c(4:11,15:65)], se_type = "stata")

screenreg(list(fe1,fe1.1), include.ci = F)

fe2.1 = lm_robust(lvio ~.-1, data =Guns.v2[c(4:12, 15:65,67:88)], se_type = "stata")

fe2.2 = lm_robust(lvio ~.-1- mur - rob - lmur - lrob - lvio, data =Guns.v2[c(4:12, 15:65,67:88)], se_type = "stata")

screenreg(list(fe2,fe2.1,fe2.2), include.ci = F)


