library(ggplot2)
library(dplyr)
library(gridExtra)


setwd('C:\\Users\\secretary-1\\Udacity\\Data Analysis\\P5')
pf <- read.csv('income_per_person_gdppercapita_ppp_inflation_adjusted.csv', check.names = FALSE)
# check.names = FALSE used to disable changing years to Xyear

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

ggplot(aes(x = country, y= value, group = 1), data = convr_data1)+
  geom_col(aes(color = duration, fill = factor(duration)))


ggplot(aes(x = duration, y= value, group = 1), data = convr_data1)+
  geom_line(aes(color = country))


pf2 <- read.csv('Book1.csv')

pf$country <- as.character(pf$country)
class(pf$country)

pf2$country <- as.character(pf2$country)
class(pf2$country)

new_pf2 <- left_join(pf, pf2, by=c('country'='country'))


new_pf2$country <- factor(new_pf2$country)
class(new_pf2$country)

convr_data2 <- melt(new_pf2, id.vars=c("country",'Location', 'Income') , variable.name="Years", value.name ='GDP')

convr_data2$Years <- as.integer(as.character(convr_data2$Years))
class(convr_data2$Years)

ggplot(data = convr_data2, aes(Years, GDP))+
  geom_line(aes(color = Income), stat = 'summary', fun.y = mean)+
  scale_x_continuous(breaks = seq(1800, 2018,20))+
  facet_wrap(~Location)

ggsave('countries_income_during_200.jpeg')
