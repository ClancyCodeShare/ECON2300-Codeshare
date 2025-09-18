rm(list = ls())
#setwd()
getwd()


library(readr) # package for fast read rectangular data
library(dplyr) # package for data manipulation
library(ggplot2) # package for elegant data visualisations
library(estimatr) # package for commonly used estimators with robust SE
library(texreg) # package converting R regression output to LaTeX/HTML tables
library(car) # package for functions used in "An R Companion to Applied Regression"
library(multcomp) # package for simultaneous tests and CIs for general linear hypotheses

CPS12 <- read_csv("cps12.csv") %>%
  mutate(ln_ahe = log(ahe),
         ln_age = log(age),
         age2 = age*age,
         fem_bac = female*bachelor,
         fem_age = female*age,
         fem_age2= female*age2,
         bac_age = bachelor*age,
         bac_age2 = bachelor * age2)

#We have mutated some additional terms to our dataset to complete
#the regressions in the next section of these tutorial questions.

#We will run the same four regressions as last time, adding some additional
#ones for the additional questions:

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

#If you like, you can generate a texreg table of these regressions:

texreg(list(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8),
       include.ci = F, caption.above = T,
       digits = 3, caption = "Earnings and Age, 2012",
       custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)"))

#Alternatively, you can print off a table in your rstudio console using
#screenreg:

screenreg(list(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8), include.ci = F,
          custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)"))

#You can edit many features of the table output in screenreg, for instance,
#in the above code we set custom model names using a vector of character terms,
#one term for each model, to set their names in their columns in the table.

#for instance, setting custom.header can allow us to put extra descriptions
#on top of our models. Setting:

#custom.header = list("lin-lin" = 1, "log-lin" = 2, "log-log" = 3, "log - age^2" = 4:8)

#Describes the form of each of our models, as "lin-lin", "log-lin" and so on.
#The final argument of the list gives the same header to columns 4 to 8.

#We can also edit the "Goodness of fit" section of the table, via the
#various gof arguments. A relatively easy method we can use will add extra rows
#to this section of the table, for instance if we wanted to include SER in
#our goodness of fit section. Using the formula from the week 2 lecture slides
#for SER: sqrt(SSE/(n-k)), for a model with n observations and k coefficients, 
#we can calculate the SER for each regression and input these values into our
#table. 

#With your SER values calculated, format them in a vector:

SER_example = c("SER 1", "SER 2", "SER 3", "SER 4", "SER 5", "SER 6", "SER 7", "SER 8")

#We can then add this list as a custom row titled "SER", using the argument;

#custom.gof.rows = list(SER = SER_example)

#Rather than working out each of your answers for SER and manually putting them
#into a vector, it will likely be easier to use a for loop to iterate over
#each of your regressions and calculate the SER for each. For example:

#(Note that this working with for loops is not necessary for your assessment,
#though it may be interesting/ helpful)

#First make a data frame to store our regressions.
regressions = list()

#Now to actually input the regressions into the list:
for (i in 1:8){
  regressions[[i]] = get(paste0("reg", i))
}
#The paste0() function allows us to write multiple different characters in our
#loop, for each iteration it will return a character "regi", where "i" is
#replaced with a number from 1 to 8 for whatever iteration of the loop we
#are in. get() will then output our character string "regi" as an object name,
#ie without the quotation marks. So if i is equal to 1, then this iteration
#will just give us our already calculated reg1.

#Note the double square brackets used to index the list, this ensures that
#the value we are putting inside the list does not get coerced into a list
#type itself. The code will appear to work with single bracket indexing, but
#you will lose the special features of the "regression" type that reg1 to reg8
#have and this will mess with your calculations.

#With your regressions now stored in a list that you can iterate over, you can
#calculate the SER for each of the regression in a for loop. You can store these
#in a vector (or another data structure if you like):

#This vector will be filled with 8 NA values, to be replaced later.
SER_example2 = c(rep(NA, 8))

#This for loop is only an example and does not actually calculate the SER,
#to calculate SER you will only need to implement the SER formula into this
#loop, rather than my example code with only prints off a character string
#based on the iteration (so you will replace the paste0 code with this):

for (i in 1:8) {
  SER_example2[i] = paste0("reg", i, " SER")
}

#We can put our headers and different SER rows into the screenreg command as
#follows:

screenreg(list(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8), include.ci = F,
          custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)"),
          custom.header = list("lin-lin" = 1, "log-lin" = 2, "log-log" = 3, "log - age^2" = 4:8),
          custom.gof.rows = list("SER1" = SER_example, "SER2" = SER_example2),
          digits = 3)

#See that custom.gof.rows allows us to input both of our SER calculations
#in the list, although you will only have one SER calculation of course.
#This just shows you can add multiple extra rows to the goodness of fit section
#if desired.

#Part h)

#Defining the ages we want to predict/ plot over,
#from 25 to 34 in increments of 1
age <- seq(25, 34, by = 1)

#Calculate the regressions at each of our chosen values of age:
ln_aheb <- 1.941 + 0.0255*age
ln_ahec <- 0.150 + 0.753*log(age)
ln_ahed <- 0.792 + 0.104*age - 0.00133*age^2

#Make tables (data.frames) of our calculations to make things easier
#to plot, as things are labeled and stored together neatly
datab <- data.frame(ln_ahe = ln_aheb, age = age, data = "b")
datac <- data.frame(ln_ahe = ln_ahec, age = age, data = "c")
datad <- data.frame(ln_ahe = ln_ahed, age = age, data = "d")

#Combine our tables together. rbind connects data.frames by rows,
#as opposed to by their columns, which would use cbind. In this case, you
#can picture each table being stuck underneath the last.
data.bcd <- rbind.data.frame(datab, datac, datad)

#Code to output the graph with various aesthetic settings
fig8.2 <- ggplot(data.bcd, aes(x = age, y = ln_ahe)) +
  #v this is the line that plots our curves
  geom_line(aes(col = data), size = 0.8) +
  labs(title = "Figure 2: Regression Lines (2) - (4)",
       x = "Age", y = "log of AHE") +
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, size = 12, family = "serif",
                                  face = "bold"),
        legend.position = c(0.15, 0.85),
        legend.title = element_blank(),
        legend.text = element_text(family = "serif", face = "bold"),
        legend.key = element_rect(color = "transparent"),
        legend.background = element_rect(fill = "lightgrey",
                                         size = 0.8,
                                         linetype="solid")) +
  scale_color_discrete(name = "Model", labels = c(" Regression (2)",
                                                  " Regression (3)",
                                                  " Regression (4)"))
print(fig8.2)

#i)

#SER = sqrt(SSE/(n-k))

reg1$res_var #RMSE^2
#RMSE = sqrt(SSE/n)
#RMSE^2 = MSE/n

reg1$res_var * reg1$nobs
#RMSE^2 * n = SSE

sqrt(reg1$res_var * reg1$nobs/(7440 - 4))

#To make these calculations, we can use a for loop to iterate over the
#given scenarios. We are asked to predict for cases where an individual
#is and isn't female, and where they do and do not have a bachelor's degree.

Predictions = data.frame(matrix(0L, nrow = 2, ncol = 2))
#This code makes a data.frame to hold our data, which starts off initally
#as an empty 2x2 matrix. We coerce the matrix into a data.frame so that
#we can give it column and row names:

rownames(Predictions) = c("male","female")
colnames(Predictions) = c("bachelor = 0", "bachelor = 1")

#Using i to iterate of male and female
for (i in 0:1){
  #using j to iterate over bachelor
  for (j in 0:1){
    Predictions[i+1,j+1] = predict(reg5, newdata = data.frame(age = 30, female = i,
                                                          bachelor = j,
                                                          age2 = 30^2,
                                                          fem_bac = i*j))
  }
}

#Taking the difference between any of these estimates of log(ahe) gives
#and estimated percentage difference, so long as the values are not too
#far apart.


#j)

#reg6 checks for this by adding an interaction term for female and both
#age and age2.

#We can run a hypothesis test on reg6 to get an answer:

linearHypothesis(reg6, c("fem_age = 0", "fem_age2 = 0"), test = "F")

#k) reg7 includes interaction terms for bachelor with age and age2, 
#we can also run a hypothesis test on these:

linearHypothesis(reg7, c("bac_age = 0", "bac_age2 = 0"), test = "F")

#l)

#reg8 has (just barely) the best adjusted R^2, so we will use this to summarise
#the effects. We can do this by showing the effect of age for different ages, 
#sexes and education levels. We will use ages 25, 32 and 34, and check for male,
#and female predictions, and bachelor and no bachelor predictions:

Table = data.frame(matrix(0L, nrow = 4, ncol = 5))
rownames(Table) = c("Bachelor = 0, Female = 1", "Bachelor = 1, Female = 1",
                    "Bachelor = 1, Female = 0", "Bachelor = 0, Female = 0")
colnames(Table) = c("25", "32", "34", "25->32", "32->34")
#The table has 4 rows and 5 columns. The rows are for each combination
#of female and bachelor, of which there are 4. The first 3 columns are
#for each age value, and the fourth and 5th column and for the difference between
#the 1st and 2nd, and the 2nd and 3rd columns (giving an approximation
#percentage difference due to the log of the dependent variable).

#Defining the ages we want to iterate over:
Ages = c(25,32,34)

#The loop will fill in the first row of our table, then add 1 to the 
#object i, so that on the next iteration, the loop fills in the 2nd row of 
#the table, and so on.
i=1

for (BACHELOR in 0:1){
  for (FEMALE in c(1,0)){
    for (j in 1:3){
      AGE = Ages[j]
      Table[i,j] = predict(reg8, newdata = data.frame(
        age = AGE, age2 = AGE^2, fem_age = FEMALE * AGE,
        fem_age2 = FEMALE * AGE^2, bac_age = BACHELOR * AGE, bac_age2 = BACHELOR * AGE^2,
        female = FEMALE, bachelor = BACHELOR, fem_bac = FEMALE*BACHELOR
      ))
      if (j>=2){Table[i,j+2] = (Table[i,j] - Table[i,j-1])/(Ages[j] - Ages[j-1])*100}
      if (j==3){i = i+1}
    }
  }
}
print(Table)


#Tutorial 7 part a)

#See solutions for detailed written discussion on internal validity

#part b)

#We can examine temporal validity by constraining our regressions to some
#subset of the times in the data we used. For instance, if we filter our data
#to only include observations from 1992:

rm(list = ls())
CPS <- read_csv("cps92_12.csv") %>%
  mutate(cpi = 140.3*(year == 1992) + 229.6*(year == 2012),
         ahe12 = (ahe/cpi)*229.6) %>%
  mutate(ln_ahe12 = log(ahe12),
         ln_age = log(age),
         age2 = age*age,
         fem_bac = female*bachelor,
         fem_age = female*age,
         fem_age2= female*age2,
         bac_age = bachelor*age,
         bac_age2= bachelor*age2) %>%
  filter(year == 1992)
attach(CPS)

reg1 = lm_robust(ahe12 ~ age + female + bachelor, data = CPS, se_type = "stata")
reg2 = lm_robust(ln_ahe12 ~ age + female + bachelor, data = CPS, se_type = "stata")
reg3 = lm_robust(ln_ahe12 ~ ln_age + female + bachelor, data = CPS, se_type = "stata")
reg4 = lm_robust(ln_ahe12 ~ age + age2 + female + bachelor,
                 data = CPS, se_type = "stata")
reg5 = lm_robust(ln_ahe12 ~ age + age2 + female + bachelor + fem_bac,
                 data = CPS, se_type = "stata")
reg6 = lm_robust(ln_ahe12 ~ age + age2 + fem_age + fem_age2 + female +
                   bachelor + fem_bac, data = CPS, se_type = "stata")
reg7 = lm_robust(ln_ahe12 ~ age + age2 + bac_age + bac_age2 + female +
                   bachelor + fem_bac, data = CPS, se_type = "stata")
reg8 = lm_robust(ln_ahe12 ~ age + age2 + fem_age + fem_age2
                 + bac_age + bac_age2 + female + bachelor + fem_bac,
                 data = CPS, se_type = "stata")

screenreg(list(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8), include.ci = F,
          custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)"),
          digits = 3)

#We can also summarise the effects from reg8 just as before:

Table = data.frame(matrix(0L, nrow = 4, ncol = 5))
rownames(Table) = c("Bachelor = 0, Female = 1", "Bachelor = 1, Female = 1",
                    "Bachelor = 1, Female = 0", "Bachelor = 0, Female = 0")
colnames(Table) = c("25", "32", "34", "25->32", "32->34")

Ages = c(25,32,34)

i=1

for (BACHELOR in 0:1){
  for (FEMALE in c(1,0)){
    for (j in 1:3){
      AGE = Ages[j]
      Table[i,j] = predict(reg8, newdata = data.frame(
        age = AGE, age2 = AGE^2, fem_age = FEMALE * AGE,
        fem_age2 = FEMALE * AGE^2, bac_age = BACHELOR * AGE, bac_age2 = BACHELOR * AGE^2,
        female = FEMALE, bachelor = BACHELOR, fem_bac = FEMALE*BACHELOR
      ))
      if (j>=2){Table[i,j+2] = (Table[i,j] - Table[i,j-1])/(Ages[j] - Ages[j-1])*100}
      if (j==3){i = i+1}
    }
  }
}
print(Table)

#The regressions do not change much, and the summarisation in the table gives
#similar answers direction ally, only with different precise values and some
#minor anomalies, such as earning decreasing for Bachelor = 0, Female = 1, and
#age in creasing from 32 to 34, where before we saw an increase.

#Part b)
#To inform discussion about validity problems we run the following regressions:


rm(list = ls())

BW <- read_csv("birthweight_smoking.csv") %>%
  mutate(young = as.numeric(age <= 20),
         m_ed1 = as.numeric(educ < 12),
         m_ed2 = as.numeric(educ == 12),
         m_ed3 = (educ > 12)*(educ < 16),
         m_ed4 = as.numeric(educ == 16),
         m_ed5 = as.numeric(educ > 16),
         age2 = age*age,
         smoker_age = smoker*age,
         smoker_young = smoker*young)
attach(BW)
reg1 = lm_robust(birthweight ~ smoker + alcohol + nprevist + unmarried,
                 data = BW, se_type = "stata")
reg2 = lm_robust(birthweight ~ smoker + alcohol + nprevist + unmarried + age + educ,
                 data = BW, se_type = "stata")
reg3 = lm_robust(birthweight ~ smoker + alcohol + nprevist + unmarried + age +
                   m_ed2 + m_ed3 + m_ed4 + m_ed5,
                 data = BW, se_type = "stata")
reg4 = lm_robust(birthweight ~ smoker + alcohol + nprevist + unmarried + m_ed2 +
                   m_ed3 + m_ed4 + m_ed5 + young,
                 data = BW, se_type = "stata")
reg5 = lm_robust(birthweight ~ smoker + alcohol + nprevist + unmarried + age +
                   age2, data = BW, se_type = "stata")
reg6 = lm_robust(birthweight ~ smoker + alcohol + nprevist + unmarried + age +
                   smoker_age, data = BW, se_type = "stata")
reg7 = lm_robust(birthweight ~ smoker + alcohol + nprevist + unmarried + young +
                   smoker_young, data = BW, se_type = "stata")

screenreg(list(reg1,reg2,reg3,reg4,reg5,reg6,reg7),
          include.ci = F, digits = 3)

#We test a variety of possible ommited factors, eg alcohol and nprevist.\

#We also test for a non-linear functional form in terms of age, finding that
#the non-linear terms is probably important, hence we would likely
#suffer from misspecification by only included age linearly

#Errors in variables bias may be present if, for example, smoking rate were
#underreported.

#Sample selection bias is unlikely to be a major problem, the data is taken
#from a random sample of babies born in pensylvania.

#Simultaneous causality could be a problem - mothers who are at risk of 
#a low weight pregnancy may choose not to smoke to mitigate this risk factor
#at a higher rate than average, which would "blunt" the effect of smoking
#that we detect in the data, as we would on average be looking at mothers
#who are less likely in general to have a low birthweight baby

#Heteroskedasticity and correlation in errors are unlikely to be a problem,
#as we use robust standard errors and the sample is taken randomly from a large
#population.