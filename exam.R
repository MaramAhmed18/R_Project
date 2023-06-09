setwd("C:/Users/almnara/Desktop/labs")
data <- read.csv("G3_sydney_hobart_times.csv", na.strings = c(''))
data

#defind NA values locations
is.na(data)

#covert Year column from int to char(not used in analysis)
data$Year <-as.character(data$Year)
str(data)

#remove text in num values in Time column (#Data Cleaning)
data$Time <-gsub("day",'', data$Time)
data$Time <-as.numeric(data$Time)
str(data)

#for the last col
#way 1 drop the whole column (all values are NA)
data$Code.Time.less.than.3 <- NULL
data

#Way 2 fill values of the column based on another variable
data[is.na(data$Code.Time.less.than.3) & data$Time< 3 , 'Code.Time.less.than.3'] = 'yes'
data[is.na(data$Code.Time.less.than.3) & data$Time>= 3 , 'Code.Time.less.than.3'] = 'no'
data[is.na(data$Code.Time.less.than.3) & is.na(data$Time) , 'Code.Time.less.than.3'] = 'NA'
data$Code.Time.less.than.3= as.factor(data$Code.Time.less.than.3)
data

#Way 3 fill values of the column based on mean of another variable by if else
x <- mean(data$Time)
data$Code.Time.less.than.3 <- as.factor(ifelse(data$Time > x, "no", "yes"))
data

#way1: replace NA values by multiple imputation technique(#completing Missing values)
install.packages("mice")
library(mice)
t <- mice(data, 
          m=5 ,
          meth= c('',"pmm",'','',''),
          maxit= 10)
data<- complete(t,5)
data

#way2: drop data with missing values(#drop Missing values)
install.packages("tidyr")
library(tidyr)
cleandata <- drop_na(data)
cleandata

#check missing values are completed or not
any(is.na(data))

#########################################
#Data visualization
install.packages("tidyverse")
library(ggplot2)

# Histogram plot on time column
draw1 <- ggplot(data, aes(Time))
draw1+geom_histogram(fill='black',alpha = 0.5) + ggtitle("Time Plot")

# Scatter plot between fleet_start and fleet_finish columns
draw2 <- ggplot(data, aes(fleet_start , fleet_finish))
draw2+ geom_point()

#Bar plot to summarize the Time to Code.Time.less.than.3 and Year
draw3<-ggplot(data , aes(x=Time,fill= Code.Time.less.than.3))
draw3 +geom_bar() +theme_light()+facet_wrap(~Year)
