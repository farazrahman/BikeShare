#First of all we will start with loading our Libraries. I have used three important libraries here for my Exploratory Data Analysis
#readr - will be used for reading the files
#dplyr - will be used for data manipulation
#ggplot2 - will be used for plotting the graphs and analysis

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

options(scipen = 9999)
options(warn = -1)

#Next I am reading the data into three files chicago, new york and washington

chi <- read_csv("chicago.csv")
ny <- read_csv("new-york-city.csv")
wash <- read_csv("washington.csv")

#We will check the summaries of each data set

sapply(list("Chicago" = chi,"New York" = ny,"Washington"  =  wash), glimpse)

sapply(list("Chicago" = chi,"New York" = ny,"Washington"  =  wash), summary)


#Checking NA Values in each files

chi %>% summarise_all(funs(sum(is.na(.))))
ny %>% summarise_all(funs(sum(is.na(.))))
wash %>% summarise_all(funs(sum(is.na(.))))


# We see that there are a lot of timestamp columns which will be useful for determining the popular days and popular hours
# Hence we will clean the data in the next step

#Extract date time and day month. 
#The timestamp is in dttm format. We will use the library lubridate to extract month day and hour of the day.


ny$`Start Time` <- ymd_hms(ny$`Start Time`)
ny$`End Time` <- ymd_hms(ny$`End Time`)


wash$`Start Time` <- ymd_hms(wash$`Start Time`)
wash$`End Time` <- ymd_hms(wash$`End Time`)


chi$`Start Time` <- ymd_hms(chi$`Start Time`)
chi$`End Time` <- ymd_hms(chi$`End Time`)

#Created a function to extract hour, month and day from start_time of all three csv files

extract <- function(data){
  data$hour <- hour(data$`Start Time`)
  data$month <- month(data$`Start Time`)
  data$day <- weekdays(data$`Start Time`)
  return(data)
}

chicago <- extract(chi)
newyork <- extract(ny)
washington <- extract(wash)
#Now we have our three datasets chicago, newyork and washington on which we will perform the exploratory analysis and answer some questions



####Exploratory Data Analysis########

#Q1 Popular times of travel (i.e., occurs most often in the start time)

#What is the most common month? 

ggplot(chicago, aes(month)) + geom_histogram(bins = 6, color = I('white'))+  
  scale_x_continuous(breaks = seq(1,6,1))+ scale_y_continuous(breaks = seq(0,100000, 10000))+
  xlab("month") +  ylab("Count")+ ggtitle("Popular Months in Chicago")

#The most popular month in chicago is month 6 with number of trips reaching upto 100000

ggplot(newyork, aes(month)) + geom_histogram(bins = 6, color = I('white'))+  
  scale_x_continuous(breaks = seq(1,6,1))+ scale_y_continuous(breaks = seq(0,100000, 10000))+
  xlab("month") +  ylab("Count")+ ggtitle("Popular Months in New York City")

#The most popular month in NewYork City is month 6 with number of trips reaching upto 75000 trips which is lesser than chicago



ggplot(washington, aes(month)) + geom_histogram(bins = 6, color = I('white'))+  
  scale_x_continuous(breaks = seq(1,6,1))+ scale_y_continuous(breaks = seq(0,100000, 10000))+
  xlab("month") +  ylab("Count")+ ggtitle("Popular Months in Washington")

#The most popular month in Washington is month 6 with number of trips little below 70000 trips, 
#however, the number of tips in the month 4 ismore w.r.t other cities


#What is the most common day of week?

ggplot(chicago, aes(day)) + geom_bar()+  
  xlab("Weekdays") +  ylab("Count")+ ggtitle("Popular Weekdays in Chicago")

#Monday and Tuesday are the most common days where the number of trips reach around 40000

ggplot(newyork, aes(day)) + geom_bar()+  
  xlab("Weekdays") +  ylab("Count")+ ggtitle("Popular Weekdays in New York City")

#Wednesday followed by Thursday are the most popular trips in Newy York City with number of trips reaching upto 50000.

ggplot(washington, aes(day)) + geom_bar()+  
  xlab("Weekdays") +  ylab("Count")+ ggtitle("Popular Weekdays in Washington")

#Wednesday sees most trips in washington.

#What is the most common hour of day?

ggplot(chicago, aes(hour)) + geom_histogram(bins = 24, color = I('white'))+  
  scale_x_continuous(breaks = seq(0,23,1))+
  xlab("hour") +  ylab("count")+ ggtitle("Popular hours in Chicago")

#The morning peak hour in chicago id 8 am around 25000 and evening peak hour is 5 pm around 35000 trips.


ggplot(newyork, aes(hour)) + geom_histogram(bins = 24, color = I('white'))+  
  scale_x_continuous(breaks = seq(0,23,1))+
  xlab("hour") +  ylab("count")+ ggtitle("Popular hours in New York City")

#The morning peak hour in newyork is 8 am around 25000 and evening peak hour is 5-6 pm around 30000 trips.

ggplot(washington, aes(hour)) + geom_histogram(bins = 24, color = I('white'))+  
  scale_x_continuous(breaks = seq(0,23,1))+
  xlab("hour") +  ylab("count")+ ggtitle("Popular hours in Washington")


#The trend in washington is slightly different, where the peak hours start from 1 am and reaches maximum at 8 am with 30000 trips.
#Although evening peak hour is around 5 pm but the number of trips is around 10000 which is much lesser than other cities.
#Note: Limits were set in x-axis for better view of the ranges in x-axis



##Q2 Trip duration

#What is the gender wise median travel time for users in different cities ?

summary(chicago$`Trip Duration`)

#Median trip duration in chicago is 670.0 seconds and Mean trip duration is 936.2 seconds

ggplot(data = subset(chicago, !is.na(Gender)), aes("Gender",`Trip Duration`, fill = Gender)) + geom_boxplot()+  
  scale_y_continuous(limits = c(0,2000))+
  xlab("Gender") +  ylab("Time in Seconds ")+ ggtitle("Trip duration in Chicago")


summary(newyork$`Trip Duration`)

#Median trip duration in newyork is 609.0 seconds and Mean trip duration is 899.7 seconds

ggplot(data = subset(newyork, !is.na(Gender)), aes("Gender",`Trip Duration`, fill = Gender)) + geom_boxplot()+  
  scale_y_continuous(limits = c(0,2000))+
  xlab("Gender") +  ylab("Time in Seconds ")+ ggtitle("Trip duration in newyork")

#The median trip duration of females is more than their male counterparts.
#Note: Limits were set in y-axis for better view of the ranges in y-axis
#Gender Legend is used to view the median trip duration of both gender types in boxplots. 

#Q3 User info

#What are the counts of each gender (only available for NYC and Chicago)?

chicago %>% group_by(Gender) %>% filter(!is.na(Gender))%>%
  summarise(total = length(Gender))%>%
  ggplot(aes(Gender, total)) + geom_bar(stat = 'identity')+
  xlab("Gender") +  ylab("Count ")+ ggtitle("Counts of each gender in chicago")


newyork %>% group_by(Gender) %>% filter(!is.na(Gender))%>%
  summarise(total = length(Gender))%>%
  ggplot(aes(Gender, total)) + geom_bar(stat = 'identity')+
  xlab("Gender") +  ylab("Count ")+ ggtitle("Counts of each gender in new york")


#Females are lesser in number as compared to Males in chicago and newyork
#Note: library dplyr was used to summarise the data
#observations where gender were NA were filtered out


#What are the earliest, most recent, most common year of birth (only available for NYC and Chicago)?

chicago %>% group_by(Gender,`Birth Year`) %>% filter(!is.na(Gender)) %>% 
  summarise(total = length(`Birth Year`))%>%
  ggplot(aes(`Birth Year`, total)) + geom_bar(stat = 'identity', fill = 'orange')+
  coord_trans(y = 'sqrt')+
  facet_grid(Gender~.)+
  scale_x_continuous(limits = c(1930,2017), breaks = seq(1930,2007,1))+
  theme(axis.text.x = element_text(angle = 60))+
  xlab("Birth Year") +  ylab("Count")+ ggtitle("Trip duration in chicago")


newyork %>% group_by(Gender,`Birth Year`) %>% filter(!is.na(Gender)) %>% 
  summarise(total = length(`Birth Year`))%>%
  ggplot(aes(`Birth Year`, total)) + geom_bar(stat = 'identity', fill = 'orange')+
  coord_trans(y = 'sqrt')+
  facet_grid(Gender~.)+
  scale_x_continuous(limits = c(1930,2017), breaks = seq(1930,2007,1))+
  theme(axis.text.x = element_text(angle = 60))+
  xlab("Birth Year") +  ylab("Count")+ ggtitle("Trip duration in newyork")


#The user count increases with increase in Birth Year, with highest number of users belong to 1988-89
#Note: Limits were set in x-axis for removing the outlier years and male and female facet grids were used.

