nyc = read.csv('new-york-city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)
head(wash)
summary(chi)
library(ggplot2)
library(lubridate)
library(dplyr)

ggplot(aes(Day), data = nyc_plus_chi_plus_wash)+ 
  geom_bar() + 
  facet_grid(~City)+
  ggtitle('Daily Ridership patterns')+
  ylab('# of trips started')+
  xlab('Day of the week')

nyc_edited$Day <- factor(weekdays((as.Date(nyc_edited$Start.Time))), levels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
chi_edited$Day <- factor(weekdays((as.Date(chi_edited$Start.Time))), levels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
wash_edited$Day <- factor(weekdays((as.Date(wash_edited$Start.Time))), levels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))

nyc_plus_chi_plus_wash <- rbind(nyc_edited, chi_edited, wash_edited)

wash_edited <- mutate(wash, Gender = NA, Birth.Year = NA, City = 'Washington')
nyc_edited <- mutate(nyc, City = 'New York')
chi_edited <- mutate(chi, City = 'Chicago')

table(nyc_plus_chi_plus_wash$Day, nyc_plus_chi_plus_wash$City)

##Popular Month##

nyc_edited$Month <- factor(months((as.Date(nyc_edited$Start.Time))), levels = c('January','February','March','April','May','June','July','August','September','October','November','December'))
chi_edited$Month <- factor(months((as.Date(chi_edited$Start.Time))), levels = c('January','February','March','April','May','June','July','August','September','October','November','December'))
wash_edited$Month <- factor(months((as.Date(wash_edited$Start.Time))), levels = c('January','February','March','April','May','June','July','August','September','October','November','December'))

ggplot(aes(Month), data = nyc_plus_chi_plus_wash)+ 
  geom_bar() + 
  facet_grid(~City)+
  ggtitle('Monthly Ridership patterns')+
  ylab('# of trips started')+
  xlab('Month of the year')

table(droplevels(nyc_plus_chi_plus_wash$Month), nyc_plus_chi_plus_wash$City)

##Common Hour of Day##
nyc_plus_chi_plus_wash$Hour <- format(ymd_hms(nyc_plus_chi_plus_wash$Start.Time), '%H')

ggplot(aes(Hour), data = nyc_plus_chi_plus_wash)+ 
  geom_bar() + 
  facet_grid(~City)+
  ggtitle('Hourly Ridership patterns')+
  ylab('# of trips started')+
  xlab('Hour of the day')

table(nyc_plus_chi_plus_wash$Hour, nyc_plus_chi_plus_wash$City)

#Q2

summary(nyc_plus_chi_plus_wash)

ggplot(aes(Trips), data = nyc_plus_chi_plus_wash)+ 
  geom_bar() + 
  facet_grid(~City)

nyc_plus_chi_plus_wash %>% group_by(City) %>% count(Start.Station) %>% filter(n == max(n))

nyc_plus_chi_plus_wash %>% group_by(City) %>% count(End.Station) %>% filter(n == max(n))

nyc_plus_chi_plus_wash$Trips <- paste(nyc_plus_chi_plus_wash$Start.Station, nyc_plus_chi_plus_wash$End.Station, sep = "|")
head(nyc_plus_chi_plus_wash)

nyc_plus_chi_plus_wash %>% group_by(City) %>% count(Trips) %>% filter(n == max(n))


##Q3

ggplot(aes(x = nyc_plus_chi_plus_wash$City, y = nyc_plus_chi_plus_wash$Trip.Duration, fill = nyc_plus_chi_plus_wash$City), data = nyc_plus_chi_plus_wash)+
  geom_boxplot()+
  coord_cartesian(ylim = c(250,1400))+
  stat_summary(fun.y=mean, colour="darkred", geom='point', 
               shape=18, size=3, show.legend = FALSE)+
  ggtitle('Mean & Median trip duration by city')+
  xlab('City')+
  ylab('Trip Duration (seconds)')+
  scale_y_continuous(breaks = seq(250,1400,100))+
  labs(fill = 'City')

by(nyc_plus_chi_plus_wash$Trip.Duration, nyc_plus_chi_plus_wash$City, summary)
by(nyc_plus_chi_plus_wash$Trip.Duration.Hours, nyc_plus_chi_plus_wash$City, sum)

nyc_plus_chi_plus_wash$Trip.Duration.Hours <- nyc_plus_chi_plus_wash$Trip.Duration / 3600

ggplot(aes(x = nyc_plus_chi_plus_wash$City, y = nyc_plus_chi_plus_wash$Trip.Duration.Hours, fill = nyc_plus_chi_plus_wash$Day), data = nyc_plus_chi_plus_wash)+
  geom_col()+
  ggtitle('Total trip duration by city')+
  xlab('City')+
  ylab('Total travel time (hours)')+
  labs(fill = 'Day of week')

summary(nyc_plus_chi_plus_wash)
