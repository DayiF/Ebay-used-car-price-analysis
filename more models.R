library(lubridate)
library(ggplot2)
library(gridExtra)
library(MASS)
library(leaps)
library(car)
library(caret)
library(randomForest)
library(miscTools)
library(gbm)
library(coefplot)

install.packages(c("lubridate","ggplot2","gridExtra","MASS","leaps","car","caret","randomForest","miscTools","gbm","coefplot"))

mydata <- read.csv("C:/Users/dayif/Desktop/lets do it/New Projects/Used Auto Ads Data - Ebay .csv", header=TRUE) 

myvars <- names(mydata) %in% c("name", "seller", "offerType", "abtest", "nrOfPictures") 
mydata = mydata[!myvars]

mydata$monthOfRegistration=as.numeric(as.character(mydata$monthOfRegistration))
mydata = subset(mydata,monthOfRegistration>=1 & monthOfRegistration<= 12)

mydata$yearOfRegistration = as.numeric(as.character(mydata$yearOfRegistration))
mydata = subset(mydata, yearOfRegistration >= 1960 & yearOfRegistration <= 2017)

mydata$vehicleType = sub("^$","None",mydata$vehicleType)
mydata$vehicleType = as.factor(mydata$vehicleType)

mydata$gearbox = sub("^$","Unknown",mydata$gearbox)
mydata$gearbox = as.factor(mydata$gearbox)

mydata$fuelType = sub("^$","Unknown",mydata$fuelType)
mydata$fuelType = as.factor(mydata$fuelType)

mydata$notRepairedDamage = sub("^$","Unknown",mydata$notRepairedDamage)
mydata$notRepairedDamage = sub("Unknown","No",mydata$notRepairedDamage)
mydata$notRepairedDamage = sub("No","other",mydata$notRepairedDamage)
mydata$notRepairedDamage = sub("ja","yes",mydata$notRepairedDamage)
mydata$notRepairedDamage = sub("nein","Maybe",mydata$notRepairedDamage)
mydata$notRepairedDamage = as.factor(mydata$notRepairedDamage)

mydata$dateCrawled = mdy_hm(mydata$dateCrawled)
mydata$dateCreated = mdy_hm(mydata$dateCreated)
mydata$lastSeen = mdy_hm(mydata$lastSeen)

mydata$age = (year(today())-mydata$yearOfRegistration)
mydata$age2 = (mydata$age ^ 2)
mydata$sellingTime <- as.integer(as.Date(mydata$lastSeen) - as.Date(mydata$dateCreated))

options(scipen = 5,digits=4) # no scientific notation
mydata = mydata[complete.cases(mydata$price),]
mydata <- subset(mydata, price < quantile(mydata$price, 0.95))
mydata <- subset(mydata, price >0)

mydata$powerPS = as.numeric(as.character(mydata$powerPS))
mydata = mydata[complete.cases(mydata$powerPS),]
mydata <- subset(mydata, powerPS < quantile(mydata$powerPS, 0.95))
mydata <- subset(mydata, powerPS >0)

## linear model
## creating dummies
mydata$bus <- ifelse(mydata$vehicleType=="bus",1,0)
mydata$cabrio <- ifelse(mydata$vehicleType=="cabrio",1,0)
mydata$coupe <- ifelse(mydata$vehicleType=="coupe",1,0)
mydata$kleinwagen <- ifelse(mydata$vehicleType=="kleinwagen",1,0)
mydata$kombi <- ifelse(mydata$vehicleType=="kombi",1,0)
mydata$limousine <- ifelse(mydata$vehicleType=="limousine",1,0)
mydata$suv <- ifelse(mydata$vehicleType=="suv",1,0)
mydata$automatik <- ifelse(mydata$gearbox=="automatik",1,0)
mydata$manuell <- ifelse(mydata$gearbox=="manuell",1,0)
mydata$benzin <- ifelse(mydata$fuelType=="benzin",1,0)
mydata$diesel <- ifelse(mydata$fuelType=="diesel",1,0)

## generating trainning and validation sets
set.seed(123) # randomly set 70% training data set and 30% testing data set
subdata <- sample(nrow(mydata), floor(nrow(mydata)*0.7))
training <- mydata[subdata,]
validation <- mydata[-subdata,]

## rf
rfdata = subset(mydata, gearbox != "Unknown")
rfdata = subset(mydata, vehicleType != "None")
rfdata = subset(mydata, fuelType != "Unknown")


rfvars <- names(rfdata) %in% c("price", "kilometer", "age", "age2", "powerPS", "bus", "cabrio", "coupe", "kleinwagen",
                               "kombi", "limousine", "automatik", "diesel")  # remove sub, manu, and benzin cause we dont have unknown values now
rfdata = rfdata[rfvars]

set.seed(123) 
subrfdata <- sample(nrow(rfdata), floor(nrow(rfdata)*0.7))
rftraining <- rfdata[subrfdata,]
rfvalidation <- rfdata[-subrfdata,]


rf <- randomForest(price ~., data=rftraining, ntree=5)
rf2 <- randomForest(price ~., data=rftraining, ntree=10)
rf3 <- randomForest(price ~., data=rftraining, ntree=20)
rf4 <- randomForest(price ~., data=rftraining, ntree=30)

# random forest algorithm improves the R^2 to 80%, however increasing number of trees from 5 to 30 does no improve R^2 mush, but reduce MSE a bit.

varImpPlot(rf4,n.var=15)
plot(rf4)

rfr2 = sum((predict(rf,rfvalidation)-mean(rfvalidation$price))^2)/sum((rfvalidation$price - mean(rfvalidation$price))^2)
rf2r2 = sum((predict(rf2,rfvalidation)-mean(rfvalidation$price))^2)/sum((rfvalidation$price - mean(rfvalidation$price))^2)
rf3r2 = sum((predict(rf3,rfvalidation)-mean(rfvalidation$price))^2)/sum((rfvalidation$price - mean(rfvalidation$price))^2)
rf4r2 = sum((predict(rf4,rfvalidation)-mean(rfvalidation$price))^2)/sum((rfvalidation$price - mean(rfvalidation$price))^2)
rfmse = mean((predict(rf,rfvalidation)-rfvalidation$price)^2)
rf2mse = mean((predict(rf2,rfvalidation)-rfvalidation$price)^2)
rf3mse = mean((predict(rf3,rfvalidation)-rfvalidation$price)^2)
rf4mse = mean((predict(rf4,rfvalidation)-rfvalidation$price)^2)

rfvalidation$predict <- predict(rf4, rfvalidation)
rfvalidation$error <- rfvalidation$predict-rfvalidation$price

par(mfrow=c(1,2))

ggplot(data =rfvalidation, aes(x=price, y=predict) )+
  geom_point(alpha=0.2)+
  geom_smooth()+
  xlab('price') +
  ylab('predicted') +
  ggtitle('price vs predicted price')


ggplot(data =rfvalidation, aes(x=price, y=error) )+
  geom_point(alpha=0.2)+
  geom_smooth()+
  xlab('price') +
  ylab('error') +
  ggtitle('price vs predicted price error')

rfR2s = c(rfr2,rf2r2,rf3r2,rf4r2) # 0.812, 0.818, 0.818, 0.805
rfMSEs = c(rfmse,rf2mse,rf3mse,rf4mse) # 3343722, 3209989, 3169890, 3162521

fit.lmrf = lm(price~.,data = rftraining)
summary(fit.lmrf)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

rfnormtraining = rftraining
rfnormtraining$price = normalize(rfnormtraining$price)
rfnormtraining$powerPS = normalize(rfnormtraining$powerPS)
rfnormtraining$kilometer = normalize(rfnormtraining$kilometer)

rfnormvalidation = rfvalidation
rfnormvalidation$price = normalize(rfnormvalidation$price)
rfnormvalidation$powerPS = normalize(rfnormvalidation$powerPS)
rfnormvalidation$kilometer = normalize(rfnormvalidation$kilometer)

summary(rfnormtraining$kilometer)

fit.normlmrf = lm(price~.,data = rfnormtraining)
summary(fit.normlmrf) ## returns same R^2 as the unnormlized data, this because we have removed outliers and our data is ordered well
coefplot(fit.normlmrf,type="l")

mean(predict(fit.normlmrf, newdata = rfnormvalidation)-rfnormvalidation$price)^2

## best subset method with 8 variables 
regfit.full = regsubsets(price ~ kilometer + age + age2 + powerPS+bus+cabrio+coupe+
                           kleinwagen+kombi+limousine+suv+
                           automatik+manuell+benzin+diesel, training)
reg.summary=(summary(regfit.full))

## with 15 variables
regfit.full = regsubsets(price ~ kilometer + age + age2 + powerPS+bus+cabrio+coupe+
                           kleinwagen+kombi+limousine+suv+
                           automatik+manuell+benzin+diesel, data=training, nvmax=15)
## shows the model with 15 variables is the best one 
which.max(reg.summary$adjr2)
which.min(reg.summary$bic)

?plot.regsubsets
par(mfrow = c(2,2))
plot(reg.summary$rss, xlab="Number of variables", ylab="RSS", type="l")
plot(reg.summary$adjr2,xlab="Number of variables", ylab="Adjusted R square", type="l")
plot(regfit.full, scale="bic")
plot(regfit.full, scale="adjr2")


## forward method
regfit.fwd = regsubsets(price ~ kilometer + age + age2 + powerPS+bus+cabrio+coupe+
                          kleinwagen+kombi+limousine+suv+
                          automatik+manuell+benzin+diesel, data=training, nvmax=15, method="forward")
summary(regfit.fwd)
which.min(summary(regfit.fwd)$bic)
min(summary(regfit.fwd)$bic)

## calculating validation error, select the model with smallest error
test.mat = model.matrix(price ~ kilometer + age + age2 + powerPS+bus+cabrio+coupe+
                          kleinwagen+kombi+limousine+suv+
                          automatik+manuell+benzin+diesel, data = validation)


val.errors = rep(NA,15)
for(i in 1:15){
  coefi = coef(regfit.full, id=i)
  pred = test.mat[,names(coefi)]%*%coefi
  val.errors[i] = mean((validation$price-pred)^2)
}

which.min(val.errors) # minium validation error is 15

# use cross validation to select the best model, then use full dataset(mydata) to predict coeficients

regfit.all = lm(price ~ kilometer + age + age2 + powerPS+bus+cabrio+coupe+
                          kleinwagen+kombi+limousine+suv+
                          automatik+manuell+benzin+diesel, data=mydata)

summary(regfit.all)
coefplot((regfit.all))

## gbm 

gbmModel = gbm(formula = price ~ .,
               data = rftraining,
               n.trees = 3000,
               shrinkage = .01,
               n.minobsinnode = 5000)

gbmModel2 = gbm(formula = price ~ .,
               data = rftraining,
               n.trees = 3000,
               shrinkage = .05,
               n.minobsinnode = 10000)

summary(gbmModel)


gbmValPred = predict(object = gbmModel,
                              newdata = rfvalidation,
                              n.trees = 1500,
                              type = "response")
gbmValPred2 = predict(object = gbmModel2,
                     newdata = rfvalidation,
                     n.trees = 1500,
                     type = "response")

gbmr2 = sum((gbmValPred-mean(rfvalidation$price))^2)/sum((rfvalidation$price - mean(rfvalidation$price))^2)
gbm2r2 = sum((gbmValPred2-mean(rfvalidation$price))^2)/sum((rfvalidation$price - mean(rfvalidation$price))^2)
gbmmse = mean((gbmValPred-rfvalidation$price)^2)
gbmmse2 = mean((gbmValPred2-rfvalidation$price)^2)

gbm2r2

gbm.perf(gbmModel2)
