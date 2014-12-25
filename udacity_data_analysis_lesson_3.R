getwd()
setwd('C:/Users/charlier/Downloads')

install.packages('ggplot2', dependencies = T) 
library(ggplot2)

pf <- read.csv('pseudo_facebook.tsv', sep = '\t') 
summary(pf)

# what variables are in the table
str(pf)

# plot friend count by gender
qplot(x = friend_count, data = subset(pf, !is.na(gender)), binwidth = 10) +
  scale_x_continuous(limits = c(0,1000), breaks = seq(0,1000,50)) +
  facet_wrap(~gender)

table(pf$gender)
by(pf$friend_count, pf$gender, summary)

# plot age and change color to orange
qplot(x=age, data=pf, 
      xlab = 'User Age',
      ylab = 'Number of Users in Sample',
      color = I('black'), fill = I('#F79420'),
      binwidth=1)  +
  scale_x_discrete(breaks = seq(13, 113, 5))

install.packages('gridExtra') 
library(gridExtra)

p1 = qplot(x=friend_count, data=pf, 
           xlab = 'User Age',
           ylab = 'Number of Users in Sample',
           color = I('black'), fill = I('#F79420'),
           binwidth=50)  +
  scale_x_discrete(breaks = seq(13, 113, 5))

p2 = qplot(x=sqrt(friend_count), data=pf, 
           xlab = 'User Age',
           ylab = 'Number of Users in Sample',
           color = I('black'), fill = I('#F79420'),
           binwidth=5)  +
  scale_x_discrete(breaks = seq(13, 113, 5))

p3 = qplot(x=log10(friend_count +1), data=pf, 
           xlab = 'User Age',
           ylab = 'Number of Users in Sample',
           color = I('black'), fill = I('#F79420'),
           binwidth=0.1)  +
  scale_x_discrete(breaks = seq(13, 113, 5))

grid.arrange(p1,p2,p3, ncol=1)

# who has more likes? Men or Women?
qplot(x = www_likes,
      data = subset(pf, !is.na(gender)),
      xlab = 'Likes',
      ylab = 'Proportion of Users with that Likes',
      geom = 'freqpoly', color = gender) +
  scale_x_continuous() +
  scale_x_log10()

by(pf$www_likes, pf$gender, sum)

# boxplot
qplot(x = gender, y = friend_count,
      data = subset(pf, !is.na(gender)),
      geom = 'boxplot', color = gender) +
  coord_cartesian(ylim = c(0,250))

by(pf$friendships_initiated, pf$gender, summary)
      
names(pf)
mobile_check_ins <- NA
mobile_check_ins <- ifelse(pf$mobile_likes>0,1,0)
yes_checks <- length(which(mobile_check_ins == 1)) 
denominator <-length(mobile_check_ins)
percent <- yes_checks/denominator*100
percent
