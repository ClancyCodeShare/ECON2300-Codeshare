rm(list = ls())
setwd("[Paste your working directory here]")
getwd()

#Install and access necessary libraries:

install.packages(c("readr", "dplyr", "ggplot2", "estimatr", "Hmisc"))

library(readr) # package for fast read rectangular data
library(dplyr) # package for data manipulation
library(ggplot2) # package for elegant data visualisations
library(estimatr) # package for commonly used estimators with robust SE
library(Hmisc) # package containing many functions useful for data analysis


#Question 1:

#Load the appropriate data into the environment:

Growth = read_csv("Growth.csv")

#a)

#Plotting the data with axis labels and a title:

#Basic Plot
plot(Growth$tradeshare, Growth$growth,
     xlab = "Average TradeShare",
     ylab = "Average Growth Rate",
     main = "Growth and Tradeshare Data, Average over 1960-1995")

#Prettier Plot (Using ggplot)
fig1 = ggplot(Growth, #Tells ggplot to access Growth for any data
              aes(tradeshare, growth)) +#assigns the mappings of the X and Y variables
                                        #here we are telling ggplot to put tradeshare
                                        #on the X axis and growth on the Y axis.
                                        #aes() is used to manipulate
                                        #the data into a form that ggplot
                                        #will accept
  geom_point(alpha = 0.75, size = 1.5, color = "cyan4")+#Sets the style
                                                        #of the points on the plot
                                                        #Can also set the mapping of points,
                                                        #but this was already done above with the aes() argument
  labs(title = "Figure 1: Growth Rate and Trade Share",
       x = "Trade Share", y = "Growth Rate") + #Sets various labels' text
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, family = "serif",
                                  face = "bold", size = 10))
print(fig1)

#Perhaps there is a weak positive relationship?

#b) 
#We can single out the Malta point using annotate()

fig2 = ggplot(Growth, aes(tradeshare, growth)) +
  geom_point(alpha = 0.75, size = 1.5, color = "cyan4")+
  labs(title = "Figure 1: Growth Rate and Trade Share",
       x = "Trade Share", y = "Growth Rate") + 
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, family = "serif",
                                  face = "bold", size = 10)) +
  annotate("text", family = "serif", size = 3,
           x = Growth$tradeshare[Growth$country_name == "Malta"],
           y = Growth$growth[Growth$country_name == "Malta"] - 0.5,
           label = "Malta")

print(fig2)

#We could also add annotate to fig1

fig1 = fig1 +
  annotate("text", family = "serif", size = 3,
           x = Growth$tradeshare[Growth$country_name == "Malta"],
           y = Growth$growth[Growth$country_name == "Malta"] - 0.5,
           label = "Malta")
print(fig1)

#c)

#Regression with robust standard errors:

reg1 = lm_robust(growth ~ tradeshare, data = Growth, se_type = "stata")
#Present output table:

summary(reg1)

#We have estimated the coefficient on tradeshare to be 2.31, and the
#intercept to be 0.6403. We can use these to estimate the growth rate
#when trade share is 1 and when it is 0.5

y1 = reg1$coefficients[1] + reg1$coefficients[2] * 1
y0.5 = reg1$coefficients[1] + reg1$coefficients[2] * 0.5
y1
y0.5

#We could also use the predict function:

predict(reg1, newdata = data.frame(tradeshare=c(0.5,1)))

#d)

#To  exclude Malta from the data, we can use subset:

reg2 = lm_robust(growth ~ tradeshare, 
                 data = subset(Growth,country_name != "Malta"),
                 se_type = "stata")
summary(reg2)

#We could do that same thing using indexing:
reg2 = lm_robust(growth ~ tradeshare, 
                 data = Growth[Growth$country_name!= "Malta",],
                 se_type = "stata")
summary(reg2)

#e)
#Plotting the regressions against the data:

fig3 = ggplot(Growth, aes(tradeshare, growth)) +
  geom_point(alpha = 0.5, size = 1.5, color = "cyan4") +
  labs(title = "Figure 3: Growth Rate and Trade Share",
       x = "Trade Share", y = "Growth Rate") +
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, size = 10,
                                  family = "serif",
                                  face = "bold"))+
  #Fitted lines, first for reg1, then for reg2
  geom_smooth(method = "lm", se = FALSE, size = 0.75, color = "violetred3") +
  geom_smooth(data = subset(Growth, country_name != "Malta"),
    method = "lm", se = FALSE, size = 0.75, color = "dodgerblue3")

print(fig3)

#f)

#Malta is an island nation that transfers a lot of freight. That
#means a lot of goods are imported by Malta, and they are subsequently
#exported, not used or transformed in Malta. This gives it a high
#tradeshare, but this is an unusual case and is not the kind of thing
#we are investigating here, if we wanted to see whether nations that
#do more trade have higher/lower growth.

#Question 2

#Load the appropriate data
rm(list = ls())
EH = read_csv("Earnings_and_Height.csv")

#a)

#We can use describe:

describe(EH$height, descript = "height")

#or median:

median(EH$height)

#b)

describe(EH$earnings[EH$heigh<= 67], 
         descript = "earnings for height <=67")
describe(EH$earnings[EH$heigh> 67], 
         descript = "earnings for height >67")
#2 sample t test
t.test(EH$earnings[EH$height <= 67], EH$earnings[EH$height>67])

#c)

fig4 = ggplot(EH, aes(height, earnings)) +
  geom_point(alpha = .75, size = 1.5, color = "dodgerblue3") +
  labs(title = "Figure 4: Earnings and Height",
       x = "Height", y = "Earnings") +
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, family = "serif",
                                  face = "bold", size = 10)) +
  geom_smooth(method = "lm", se = FALSE, size = 0.75, color = "violetred3")
print(fig4)

#Earnings are reported within brackets, rather than the individuals
#exact earnings, hence why the values appear on horizontal lines in
#the figure

#d)

reg3 = lm_robust(earnings ~ height, data = EH, se_type = "stata")
summary(reg3)

predict(reg3, newdata = data.frame(height = c(67, 70, 65)))

#e)
summary(reg3)
SER3 = sqrt(reg3$res_var)

#f)

reg4 = lm_robust(earnings~height, 
                 data = subset(EH, sex == "0:female"),
                 se_type = "stata")
summary(reg4)
SER4 = sqrt(reg4$res_var)

#g)

reg5 = lm_robust(earnings~height, 
                 data = subset(EH, sex != "0:female"),
                 se_type = "stata")
summary(reg5)
SER5 = sqrt(reg5$res_var)
