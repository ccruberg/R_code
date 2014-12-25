library (ggplot2)

getwd()
setwd('C:/Users/charlier/Downloads')

pf <- read.csv('pseudo_facebook.tsv', sep = '\t') 
summary(pf)

# scatterplot of age and friend count
qplot(x=age, y=friend_count, data=pf)

# same scatterplot using ggplot syntax
ggplot(aes(x=age, y=friend_count), data=pf) + 
  geom_jitter (alpha = 1/20) +
  xlim(13,90)

# summarize age variable
summary(pf$age)

ggplot(aes(x=age, y=friend_count), data=pf) + 
  geom_point (alpha = 1/20) +
  xlim(13,90) + 
  coord_trans(y = "sqrt")


# Examine the relationship between
# friendships_initiated (y) and age (x)
# using the ggplot syntax.

ggplot(aes(x=age, y=friendships_initiated), data=pf) + 
  geom_jitter (alpha = 1/10) +
  xlim(13,90) +
  ylim(0,5000) + 
  coord_trans(y = "sqrt")
  
# CONDITIONAL MEANS
install.packages('dplyr')
library(dplyr)

age_groups <- group_by(pf, age)
pf.fc_by_age <- summarise(age_groups,
          friend_count_mean = mean(friend_count),
          friend_count_median = median(friend_count),          
          n = n())

pf.fc_by_age <- arrange(pf.fc_by_age, age)

head(pf.fc_by_age)

pf.fc_by_age <- pf %>%
  group_by(age) %>%
  summarise(friend_count_mean = mean(friend_count),
            friend_count_median = median(friend_count),
            n = n()) %>%
  arrange(age)

head(pf.fc_by_age)

ggplot(aes(x=age, y=friend_count), data=pf) + 
  xlim(13,90) + 
  geom_point(aplha = 0.05,
             position  = position_jitter(h = 0),
             color = 'orange') + 
  coord_trans(y = 'sqrt') +
  geom_line(stat = 'summary', fun.y = mean) + 
  geom_line(stat = 'summary', fun.y = quantile, probs = .1, linetype = 2, color = 'blue') + 
  geom_line(stat ='summary', fun.y = quantile, probs = .5,  color = 'blue') + 
  geom_line(stat = 'summary', fun.y = quantile, probs = .9, linetype = 2, color = 'blue')

cor(pf$age, pf$friend_count)
  
with(subset(pf,age<70), cor.test(age,friend_count))

# Create a scatterplot of likes_received (y)
# vs. www_likes_received (x). Use any of the
# techniques that you've learned so far to
# modify the plot.

ggplot(aes(x=www_likes_received, y=likes_received), data=pf) +
  geom_point() + 
  xlim(0, quantile(pf$www_likes_received, 0.95)) + 
  ylim(0, quantile(pf$likes_received, 0.95)) + 
  geom_smooth(method = 'lm', color = 'red')

cor(pf$www_likes_received, pf$likes_received)


# MORE CAUTION WITH CORRELATION
install.packages('alr3')
library(alr3)
data(Mitchell)
?Mitchell

cor.test(Mitchell$Month, Mitchell$Temp)
names(Mitchell)

# Create a scatterplot of temperature (Temp)
# vs. months (Month).

ggplot(aes(x = Month, y = Temp), data=Mitchell) +
  geom_point() +
  scale_x_discrete(breaks = seq(0, 2013, 12))

# Create a new variable, 'age_with_months', in the ‘pf’ data frame.
# Be sure to save the variable in the data frame rather than creating
# a separate, stand-alone variable. You will need to use the variables
# 'age' and 'dob_month' to create the variable ‘age_with_months’.

pf <- read.delim('/datasets/ud651/pseudo_facebook.tsv')

pf$age_in_months <- pf$age + (12 - pf$dob_month)/12

# Create a new data frame called
# pf.fc_by_age_months that contains
# the mean friend count, the median friend
# count, and the number of users in each
# group of age_with_months. The rows of the
# data framed should be arranged in increasing
# order by the age_with_months variable.
install.packages('dplyr')
library(dplyr)
age_groups <- group_by(pf, age_in_months)
pf.fc_by_age_in_months <- summarise(age_groups,
                          friend_count_mean = mean(friend_count),
                          friend_count_median = median(friend_count),          
                          n = n())

pf.fc_by_age_in_months <- arrange(pf.fc_by_age_in_months, age_in_months)

head(pf.fc_by_age_in_months)

# Create a new scatterplot showing friend_count_mean
# versus the new variable, age_with_months. Be sure to use
# the correct data frame (the one you create in the last
# exercise) AND subset the data to investigate
# users with ages less than 71.



plotme <- subset(pf.fc_by_age_in_months, age_in_months < 71.00)

ggplot (aes(x = age_in_months, y = friend_count_mean), data=plotme) + 
  geom_line()


