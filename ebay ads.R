library(lubridate)
library(ggplot2)
library(gridExtra)
getwd()
setwd("C:/Users/df84/Desktop/lets do it/New Projects")

# read in data, change file name or location for different cases
mydata <- read.csv("C:/Users/dayif/Desktop/lets do it/New Projects/Used Auto Ads Data - Ebay .csv", header=TRUE) 

# use dim to check how large is our data
dim(mydata)
# use sty to get further information 
str(mydata)
# throw out unnecessary variables, mostly determined by general knowledge and real-world experience
myvars <- names(mydata) %in% c("name", "seller", "offerType", "abtest", "nrOfPictures") 
mydata = mydata[!myvars]
# clean the data from the strictest standard, such as the month of registration can not be any value except 1-12...
mydata$monthOfRegistration=as.numeric(mydata$monthOfRegistration)
mydata = subset(mydata,monthOfRegistration>=1 & monthOfRegistration<= 12)
summary(mydata$monthOfRegistration)
# year of registration, use boxplot to analyze variable distribution, it is reasonable to eliminate any value greater than 1960 and less than 2017
summary(mydata$yearOfRegistration)
mydata$yearOfRegistration = as.numeric(as.character(mydata$yearOfRegistration))

ggplot(aes(x=vehicleType, y=yearOfRegistration), data = mydata) + 
  geom_boxplot() +
  ylim(1975, 2017)

mydata = subset(mydata, yearOfRegistration >= 1960 & yearOfRegistration <= 2017)

# standardlize three date-related variables:
mydata$dateCrawled = mdy_hm(mydata$dateCrawled)
mydata$dateCreated = mdy_hm(mydata$dateCreated)
mydata$lastSeen = mdy_hm(mydata$lastSeen)
str(mydata)
# take a look at our main response variable price and remove na's
summary(mydata$price) 
options(scipen = 5,digits=4) # no scientific notation
mydata = mydata[complete.cases(mydata$price),]
summary(mydata$price) # there are some outliers, however we keep them and deal with them later

# convert powerPS from factor to numeric data, and we will deal with outliers later
mydata$powerPS = as.numeric(as.character(mydata$powerPS))
summary(mydata$powerPS)
mydata = mydata[complete.cases(mydata$powerPS),]

# select 0.05-0.95 for throwing out outliers
# mydata <- subset(mydata, quantile(mydata$powerPS, 0.05) < powerPS & powerPS < quantile(mydata$powerPS, 0.95))

# take a look at main categorical variables and rename blank cell to "None" type
summary(mydata$vehicleType)
mydata$vehicleType = sub("^$","None",mydata$vehicleType)
mydata$vehicleType = as.factor(mydata$vehicleType)
plot(mydata$vehicleType)
#mydata = mydata[!(is.na(mydata$vehicleType) | mydata$vehicleType==""), ]

# gearbox
summary(mydata$gearbox)
mydata$gearbox = sub("^$","Unknown",mydata$gearbox)
mydata$gearbox = as.factor(mydata$gearbox)
plot(mydata$gearbox)

# fuelType
summary(mydata$fuelType)
mydata$fuelType = sub("^$","None",mydata$fuelType)
mydata$fuelType = as.factor(mydata$fuelType)
plot(mydata$fuelType)

# model
summary(mydata$model)
plot(mydata$model)

# brand
summary(mydata$brand)

# adding two date-related variables: used car's age and selling time(not sure)
# look at several simple plots to check if there is any relationship between variables 
ggplot(mydata, aes(x=vehicleType)) + 
  geom_bar(fill= 'blue', color='black') +
  labs(x= 'Vehicle Type', y= 'number of cars') +
  ggtitle('Vehicle Type Frequency Diagram')

ggplot(mydata, aes(x=gearbox)) + 
  geom_bar(fill= 'darkgreen', color='black') +
  labs(x= 'gearbox type', y= 'number of cars') +
  ggtitle('Gearbox Type Frequency Diagram')

ggplot(mydata, aes(x=powerPS)) + 
  geom_histogram(fill= 'orange', color='black', binwidth=20) +
  labs(x= 'engine power', y= 'number of cars') +
  ggtitle('engine power Frequency Diagram')

ggplot(mydata, aes(x=price)) + 
  geom_bar(fill= 'orange', color='black') +
  labs(x= 'price', y= 'number of cars') +
  ggtitle('price Frequency Diagram')

ggplot(mydata, aes(x=kilometer)) + 
  geom_bar(fill= 'purple', color='black') +
  labs(x= 'kilometer', y= 'number of cars') +
  ggtitle('kilometer Frequency Diagram')

# from the graphs of engine power and price, we can find that extreme outliers are affecting our analysis of the variables. Therefore, we have to remove them.
mydata <- subset(mydata, price < quantile(mydata$price, 0.95))
mydata <- subset(mydata, powerPS < quantile(mydata$powerPS, 0.95))
# it is also reasonable to believe that neither price nor engine power should have 0 values.
mydata <- subset(mydata, price >0)
mydata <- subset(mydata, powerPS >0)

# reproduce the graphs

# engine power vs price
ggplot(data = subset(mydata, !is.na(powerPS)), aes(x = powerPS, y = price)) +
  geom_point(alpha = 0.02, color = I("red"), position = 'jitter') +
  geom_smooth() +
  facet_wrap(~vehicleType) +
  xlab('Engine Power') +
  ylab('Price') +
  ggtitle('Engine Power vs. Price')

# kilometer vs price 
ggplot(data = subset(mydata, !is.na(kilometer)), aes(x = kilometer, y = price)) +
  geom_point(alpha = 0.02, color = I("red"), position = 'jitter') +
  geom_smooth() +
  facet_wrap(~vehicleType) +
  xlab('Kilometer') +
  ylab('Price') +
  ggtitle('Kilometer vs. Price')


