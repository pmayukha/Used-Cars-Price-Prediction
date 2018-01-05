setwd("C:/Users/Mayukha/Desktop/INSOFE/Phase 1/MITH")

rm(list = ls(all=TRUE))

train <- read.csv("train.csv")
test <- read.csv("test.csv")

summary(train)
summary(test)
apply(train, 2, function(x){length(unique(x))})
apply(test, 2, function(x){length(unique(x))})
colnames(train)
table(train$NumberOfPictures)
train <- train[,-c(1,3,4,5,18)]


sort(colSums(is.na(test)),decreasing = TRUE)
sort(colSums(is.na(train)),decreasing = TRUE)

#No rows have nas more than 5
train[which(rowSums(is.na(train)) >5),]

library(corrplot)
corrplot(cor(train[,c(2,4,6,8,9)]))
attach(train)
library(ggplot2)
ggplot(train, aes(PowerOfTheEngine, Price, color = IsDamageRepaired)) + 
  geom_point() +
  geom_jitter(position = position_jitter(height = .1)) + theme_bw() 
prop.table(table(train$VehicleType))


#For imputing Gearboxtype
ggplot(test, aes(TypeOfTheFuelUsed, fill=GearBoxType)) + geom_bar()
ggplot(test, aes(VehicleType, fill=GearBoxType)) + geom_bar()
#In all vehicle types manual gearbox is the mode
prop.table(table(train[train$VehicleType,"GearBoxType"]))
#Imputing with manual
train$GearBoxType[is.na(train$GearBoxType)] <- "manual"  
test$GearBoxType[is.na(test$GearBoxType)] <- "manual"  


#Imputing Type of Fuel used
ggplot(train, aes(VehicleType, fill=TypeOfTheFuelUsed)) + geom_bar()
ggplot(train, aes(VehicleType, Price, color = TypeOfTheFuelUsed)) + 
  geom_jitter(position = position_jitter(height = .1)) + theme_bw() 

train$TypeOfTheFuelUsed[which(is.na(train$TypeOfTheFuelUsed) & train$VehicleType == "bus" )] <- "diesel"
train$TypeOfTheFuelUsed[which(is.na(train$TypeOfTheFuelUsed) & train$VehicleType == "Combi")] <- "diesel"
train$TypeOfTheFuelUsed[which(is.na(train$TypeOfTheFuelUsed) & train$VehicleType == "suv")] <- "diesel"
test$TypeOfTheFuelUsed[which(is.na(test$TypeOfTheFuelUsed) & test$VehicleType == "bus" )] <- "diesel"
test$TypeOfTheFuelUsed[which(is.na(test$TypeOfTheFuelUsed) & test$VehicleType == "Combi")] <- "diesel"
test$TypeOfTheFuelUsed[which(is.na(test$TypeOfTheFuelUsed) & test$VehicleType == "suv")] <- "diesel"

train$TypeOfTheFuelUsed[which(is.na(train$TypeOfTheFuelUsed))] <- "petrol"
test$TypeOfTheFuelUsed[which(is.na(test$TypeOfTheFuelUsed))] <- "petrol"

#Imputing Vehicle Type
ggplot(train, aes(TypeOfTheFuelUsed, fill=VehicleType)) + geom_bar()
prop.table(table(train$VehicleType))
train$VehicleType[which(is.na(train$VehicleType) & train$TypeOfTheFuelUsed == "petrol" )] <- "limousine"
train$VehicleType[which(is.na(train$VehicleType) & train$TypeOfTheFuelUsed == "diesel" )] <- "Combi"
train$VehicleType[which(is.na(train$VehicleType))] <- "limousine"
test$VehicleType[which(is.na(test$VehicleType) & test$TypeOfTheFuelUsed == "petrol" )] <- "limousine"
test$VehicleType[which(is.na(test$VehicleType) & test$TypeOfTheFuelUsed == "diesel" )] <- "Combi"
test$VehicleType[which(is.na(test$VehicleType))] <- "limousine"

#Imputing Is damage repaired
library(randomForest)
library(rpart)
damage_model <- rpart(IsDamageRepaired~ Price+VehicleType+YearOfVehicleRegistration+
                        GearBoxType+PowerOfTheEngine+DistranceTravelled+MonthOfVehicleRegistration+
                        TypeOfTheFuelUsed+BrandOfTheVehicle,
                      data = train[!is.na(train$IsDamageRepaired),],
                      method = "class")
library(rpart.plot)
rpart.plot(damage_model)
prop.table(table(train$IsDamageRepaired))
train$IsDamageRepaired[is.na(train$IsDamageRepaired)] <- "No"
test$IsDamageRepaired[is.na(test$IsDamageRepaired)] <- "No"

#Imputing month of registration 0

prop.table(table(train$MonthOfVehicleRegistration))

summary(train)

train$MonthOfVehicleRegistration[train$MonthOfVehicleRegistration == 0] <- NA
test$MonthOfVehicleRegistration[test$MonthOfVehicleRegistration == 0] <- NA
median(train$MonthOfVehicleRegistration, na.rm = T)
train$MonthOfVehicleRegistration[is.na(train$MonthOfVehicleRegistration)] <- median(train$MonthOfVehicleRegistration, na.rm = T)
test$MonthOfVehicleRegistration[is.na(test$MonthOfVehicleRegistration)] <- median(train$MonthOfVehicleRegistration, na.rm = T)
colnames(train)
library(lubridate)
as.Date(dmy(train$DataCollectedDate))
format(as.Date(train$DataCollectedDate, forma = "%d-%m-%Y"), format = "%Y-%m-%d")
str(train)
train1 <- train

#Converting to date format
for (i in c(1,13,15)){
  train[,i] <- format(as.Date(train[,i], forma = "%d-%m-%Y"), format = "%Y-%m-%d")
}
colnames(test)
str(test)
for (i in c(2,16,19)){
  test[,i] <- format(as.Date(test[,i], forma = "%d-%m-%Y"), format = "%Y-%m-%d")
}

#Creating column duration of selling after posting the ad
train$sellduration <- difftime(train$DateOfAdLastSeen,train$DateOfAdCreation, units = "days")
test$sellduration <- difftime(test$DateOfAdLastSeen,test$DateOfAdCreation, units = "days")
str(train)
str(test)
library(lubridate)
train$Carusage <- year(train$DateOfAdCreation) - train$YearOfVehicleRegistration
test$Carusage <- year(test$DateOfAdCreation) - test$YearOfVehicleRegistration
colnames(train)
train1 <- train
train1$sellduration <- as.numeric(train1$sellduration)

#Correlation plot
corrplot(cor(train1[,c(2,6,8,9,16,17)]), method = "number")
str(train[,c(2,4,6,8,9,16)])
str(train)
colnames(train)

set.seed(1234)
train_rows <- sample(1:nrow(train),(70*nrow(train))/100)
train_data <- train[train_rows,]
test_data <- train[-train_rows,]

train <- train[,-c(1,15,13)]
library(MASS)
train$sellduration <- as.numeric(train$sellduration)
linear_model <- lm(log(Price)~., data = train)
step <- stepAIC(linear_model, direction = "both")
summary(linear_model)
par(mfrow=c(2,2))
plot(linear_model)
summary(step)
library(DMwR)

pred = predict(linear_model,train)
regr.eval(train$Price,exp(pred))
pred_test = predict(linear_model,test)
pred_test = exp(pred_test)
str(train)
prop.table(table(train$BrandOfTheVehicle))

colnames(train)
library(caret)
library(rpart.plot)
library(randomForest)
library(DMwR)
colnames(train)
random_model <- rpart(Price~., data = train[,-c(6)], method = "anova")
rpart.plot(random_model)
pred = predict(random_model,train[,-c(6)])
regr.eval(train$Price,pred)
pred_test = predict(random_model,test)
colnames(train)


xgb.ctrl <- trainControl(method = "repeatedcv", repeats = 10, number = 3,
                         search = "random", allowParallel = T)
xgb.tune <- train(Price~.,data =train ,method="xgbLinear",
                  trControl=xgb.ctrl,
                  metric="RMSE")


pred = predict(xgb.tune,train)
regr.eval(train$Price,pred)
pred_test = predict(xgb.tune,test)







table(test$TypeOfTheFuelUsed)
ggplot(train, aes(x = TypeOfTheFuelUsed, Price, color = GearBoxType)) +
  geom_point(aes(group = VehicleType)) +
  geom_line(aes(group = VehicleType))
summary(test)


pricepred <- data.frame(VehicleID = test$VehicleID, Price = pred_test, row.names = NULL)

write.csv(pricepred,"prediction.csv", quote=F, row.names = F)
