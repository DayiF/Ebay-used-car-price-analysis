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

# take a look at engine power, convert variable type from factor to numetic 

# Therefore, we can use 0-0.9 range for the clean data.
# mydata <- subset(mydata, price < quantile(mydata$price, 0.90))

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

summary(mydata$gearbox)
mydata = mydata[!(is.na(mydata$gearbox) | mydata$gearbox==""), ]

summary(mydata$fuelType)
mydata = mydata[!(is.na(mydata$fuelType) | mydata$fuelType==""), ]

summary(mydata$offerType)
mydata = mydata[!(is.na(mydata$offerType) | mydata$offerType==""), ]

summary(mydata$model)
mydata = mydata[!(is.na(mydata$model) | mydata$model==""), ]

summary(mydata$brand)
table(mydata$vehicleType,mydata$brand)

# need to clean the dataset and convert the date format 
summary(mydata$monthOfRegistration)
mydata$monthOfRegistration=as.numeric(mydata$monthOfRegistration)
mydata = subset(mydata,monthOfRegistration>=1 & monthOfRegistration<= 12)


# mydata$dateCrawled <- ymd(mydata$dateCrawled) 
# mydata$dateCreated <- ymd_hms(mydata$dateCreated)
# mydata$lastSeen <- ymd_hms(mydata$lastSeen)

# look at several simple plots to check if there is any relationship between variables 
plot(mydata$vehicleType)
hist(mydata$powerPS)

plot(mydata$price~mydata$kilometer)
plot(mydata$price~mydata$powerPS)

# engine power vs price
ggplot(data = subset(mydata, !is.na(powerPS)), aes(x = powerPS, y = price)) +
  geom_point(alpha = 1/50, color = I("#990000"), position = 'jitter') +
  geom_smooth() +
  facet_wrap(~vehicleType) +
  xlab('Engine Power') +
  ylab('Price') +
  ggtitle('Engine Power vs. Price')

# kilometer vs price 
ggplot(data = subset(mydata, !is.na(kilometer)), aes(x = kilometer, y = price)) +
  geom_point(alpha = 1/50, color = I("#990000"), position = 'jitter') +
  geom_smooth() +
  facet_wrap(~vehicleType) +
  xlab('Engine Power') +
  ylab('Price') +
  ggtitle('Kilometer vs. Price')


