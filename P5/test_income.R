library(ggplot2)
library(dplyr)
library(gridExtra)


setwd('C:\\Users\\secretary-1\\Udacity\\Data Analysis\\P5')
pf <- read.csv('income_per_person_gdppercapita_ppp_inflation_adjusted.csv')

every_44_mean <- data.frame(country=pf[,1], I44_1st = rowMeans(pf[,2:44], na.rm=TRUE),
           I44_2nd = rowMeans(pf[,45:88], na.rm=TRUE),
           I44_3rd = rowMeans(pf[,89:132], na.rm=TRUE),
           I44_4th = rowMeans(pf[,133:176], na.rm=TRUE),
           I44_5th = rowMeans(pf[,177:220], na.rm=TRUE))

data1 <- subset(every_44_mean, 
                country == 'Saudi Arabia'| country == 'United Arab Emirates'| 
                country == 'Bahrain'| country == 'Kuwait'|
                country == 'Oman' | country == 'Qatar')

data1$country
data1$country <- c('Bahrain','Kuwait','Oman','Qatar','KSA','UAE')

p1 <- ggplot(data= data1, aes(country,I44_1st))+
  geom_col()
p2 <- ggplot(data= data1, aes(country,I44_2nd))+
  geom_col()
p3 <- ggplot(data= data1, aes(country,I44_3rd))+
  geom_col()
p4 <- ggplot(data= data1, aes(country,I44_4th))+
  geom_col()
p5 <- ggplot(data= data1, aes(country,I44_5th))+
  geom_col()

grid.arrange(p1, p2, p3, p4, p5)
names(data1)
da

library(reshape2)

convr_data1 <- melt(data1, id.vars="country", variable="duration")

ggplot(convr_data1, aes(x=country, y=value, colour=duration)) + 
  geom_point(size=2)+
  ylab('income per person')


ggsave('gcc_income.jpeg')


ggplot(aes(x = country, y= value, group = 1), data = convr_data1)+
  geom_col(aes(color = duration, fill = factor(duration)))


ggplot(aes(x = duration, y= value, group = 1), data = convr_data1)+
  geom_line(aes(color = country))
