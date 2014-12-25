library(ggplot2)
data(diamonds)

# Color: D is best J is worst
summary(diamonds)
?diamonds

str(diamonds)

# Create a histogram of the price of
# all the diamonds in the diamond data set.
qplot(x = price, data = diamonds, binwidth = 100) +
  facet_wrap(~cut, scales="free_y")

summary(diamonds$price)

five <- subset(diamonds, price < 500)
str(five)

twofifty <- subset(diamonds, price < 250)
str(twofifty)

expensive <- subset(diamonds, price >= 15000)
str(expensive)

by(diamonds$price, diamonds$cut, max)
by(diamonds$price, diamonds$cut, min)
by(diamonds$price, diamonds$cut, median)

diamonds$price_carat <- diamonds$price/diamonds$carat
diamond

qplot(x = price, data = diamonds, binwidth = .1) +
  facet_wrap(~cut, scales="free_y") +
  scale_x_log10()

# Investigate the price of diamonds using box plots,
# numerical summaries, and one of the following categorical
# variables: cut, clarity, or color.

# box plot of price by color
qplot(x = color, y = price,
      data = diamonds,
      geom = 'boxplot', color = color) +
  coord_cartesian(ylim = c(0,10000))

by(diamonds$price, diamonds$color, summary)

4214-911

7695-1860

qplot(x = carat,
      data = diamonds,
      geom = 'freqpoly',
      binwidth = .1) +
  scale_x_continuous()

qplot( x = carat,
       data = diamonds,
       geom = 'freqpoly',
       color = I('black'), 
       fill = I('#099DD9'),
       binwidth = 0.3
) + 
  scale_x_continuous(breaks = seq(0, 5, 0.2)) + 
  scale_y_continuous(breaks = seq(0, 10000, 1000))

table(diamonds$carat)



# BIRTHDAYS


getwd()
setwd('C:/Users/charlier/Downloads')
birthdays <- read.csv('birthdaysExample.csv')
str(birthdays)

dat.in <- read.table("birthdaysExample.csv", header=T, as.is=T, dec=",", sep=";")

#----- Libraries
library(ggplot2)
library(lubridate)
library(gridExtra)

#----- Dates in good format - Get a data.frame
dat.gd <- mdy(dat.in$dates)

dat.df <- data.frame( Value=dat.gd, Year=year(dat.gd), Month=month(dat.gd), Day=day(dat.gd), WeeDa=wday(dat.gd,label=T, abbr=T))
dat.df$WeeDa <- factor(dat.df$WeeDa, levels=c('Mon', 'Tues','Wed','Thurs','Fri','Sat','Sun'), ordered=T)

ggplot(dat.df, aes(x=Month)) + 
  geom_histogram(binwidth = 1, fill = '#5760AB') +
  scale_x_discrete(breaks = seq(1, 12, 1)) +
  labs(y="Frequency") +
  labs(x="Month") + 
  theme(axis.title.x=element_text(size=rel(1.2), color='blue'))



ggplot(dat.df, aes(x=Day)) + 
  geom_histogram(binwidth = 1, fill = '#5760AB') +
  scale_x_discrete(breaks = seq(1, 31, 1)) +
  labs(y="Frequency") +
  labs(x="Day") + 
  theme(axis.title.x=element_text(size=rel(1.2), color='blue'))


ggplot(dat.df, aes(x=WeeDa)) + 
  geom_histogram(binwidth = 1, fill = '#5760AB') +
  labs(y="Frequency") +
  labs(x="Day of the Week") + 
  theme(axis.title.x=element_text(size=rel(1.2), color='blue'))
