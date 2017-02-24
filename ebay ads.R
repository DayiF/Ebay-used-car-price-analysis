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
# take a look at our main response variable price
summary(mydata$price) # there is one NA, which means missing data point, we can simply remove it due to the large dataset volume 
mydata = mydata[complete.cases(mydata$price),]
summary(mydata$price)
options(scipen = 5,digits=4) # no scientific notation
quantile(mydata$price, 0.05)
quantile(mydata$price, 0.1)
quantile(mydata$price, 0.90)

ggplot(aes(x=vehicleType, y=price), data = mydata) + 
  geom_boxplot() +w
  ylim(quantile(mydata$price, 0.05), quantile(mydata$price, 0.95))


p1 <- ggplot(aes(x="price", y=price), data = mydata) + 
  geom_boxplot()

p2 <- ggplot(aes(x="price", y=price), data = mydata) + 
  geom_boxplot() +
  ylim(0, quantile(mydata$price, 0.99))

p3 <- ggplot(aes(x="price", y=price), data = mydata) + 
  geom_boxplot() +
  ylim(0, quantile(mydata$price, 0.95))

p4 <- ggplot(aes(x="price", y=price), data = mydata) + 
  geom_boxplot() +
  ylim(0, quantile(mydata$price, 0.90))

grid.arrange(p1, p2, p3, p4, ncol = 2)
# From the graph, it is reasonable for us to conclude that price greater than 15000 can be considered as outliers. 
# Therefore, we can use 0-0.9 range for the clean data.
mydata <- subset(mydata, price < quantile(mydata$price, 0.90))

# powerPS also has outliers, and we need to convert it from factor to numeric data
mydata$powerPS = as.numeric(levels(mydata$powerPS)[mydata$powerPS])
summary(mydata$powerPS)
quantile(mydata$powerPS, 0.1)
quantile(mydata$powerPS, 0.90)
quantile(mydata$powerPS, 0.95)

ggplot(aes(x=vehicleType, y=powerPS), data = mydata) + 
  geom_boxplot() +
  ylim(quantile(mydata$powerPS, 0.05), quantile(mydata$powerPS, 0.95))
# select 0.05-0.95 for throwing out outliers
mydata <- subset(mydata, quantile(mydata$powerPS, 0.05) < powerPS & powerPS < quantile(mydata$powerPS, 0.95))

# take a look at main categorical variables and remove blank/na values
summary(mydata$vehicleType)
mydata = mydata[!(is.na(mydata$vehicleType) | mydata$vehicleType==""), ]


summary(mydata$gearbox)
mydata = mydata[!(is.na(mydata$gearbox) | mydata$gearbox==""), ]

summary(mydata$fuelType)
mydata = mydata[!(is.na(mydata$fuelType) | mydata$fuelType==""), ]

summary(mydata$offerType)

summary(mydata$model)

# need to clean the dataset and convert the date format 

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


