#Data Collection
#Switch based on the Training or Testing
data = read.csv("train.csv")
#data = read.csv("test.csv")
(data)
str(data)
summary(data)

#----------------Data Visualization-----------------# 
library(tidyverse)
ggplot(data,aes(MSZoning))+
  geom_bar()
#Alley can be removed from the dataset as most of it is NA
ggplot(data,aes())+
  geom_bar()
colnames(data)
unique(data$MSZoning)
sum(is.na(data$MSSubClass))
#Alley is removed from the dataset
data <- subset(data, select = -Alley)


data$MSZoning = as.numeric(factor(data$MSZoning,levels=unique(data$MSZoning)))
data$Street = as.numeric(factor(data$Street,levels=unique(data$Street)))
 (data)
colSums(is.na(data))
# Columns with NAs are : LotFrontage, MasVnrType, MasVnrArea, BsmtQual, BsmtCond,
# BsmtExposure, BsmtFinType1, BsmtFinType2, FireplaceQu, GarageType, GarageYrBlt, 
# GarageFinish, GarageQual, GarageCond, -(PoolQC, Fence, MiscFeature) 

#Columns which can be removed: PoolQC, MiscFeature, Fence
data <- subset(data, select = -PoolQC)
data <- subset(data, select = -c(MiscFeature, Fence))
View(data)
table((data$MSZoning))
summary(data$LotFrontage)
data$MSZoning[is.na(data$MSZoning)]=2

table((data$Utilities))
data$Utilities[is.na(data$Utilities)]="AllPub"
sum(is.na(data$Utilities))



#Replacing the NA values for 
table(data$LotFrontage)
sum(is.na(data$LotFrontage))
mean(data$LotFrontage[!is.na(data$LotFrontage)])
data$LotFrontage[is.na(data$LotFrontage)]=
  mean(data$LotFrontage[!is.na(data$LotFrontage)])

#Remove MasVnrType from the dataset. 
sum(is.na(data$MasVnrType))
table(data$MasVnrType=="None")
unique(data$MasVnrType)
data <- subset(data, select = -c(MasVnrType))

#Replacing the NAs with Mean in MasVnrArea
table(is.na(data$MasVnrArea))
summary(data$MasVnrArea)
data$MasVnrArea[is.na(data$MasVnrArea)]=
  mean(data$MasVnrArea[!is.na(data$MasVnrArea)])

#Removed the NA values and factorred the levels for BsmtQual param
str(data$BsmtQual)
table(data$BsmtQual)
ggplot(data,aes(BsmtQual))+
  geom_bar()
sum(is.na(data$BsmtQual))
table(data$BsmtQual)
data$BsmtQual[is.na(data$BsmtQual)]='TA'
data$BsmtQual = as.numeric(factor(data$BsmtQual,levels=unique(data$BsmtQual)))

#Removed the NA in BsmtExposure
str(data$BsmtCond)
table(data$BsmtExposure)
sum(is.na(data$BsmtExposure))
data$BsmtExposure[is.na(data$BsmtExposure)]='No'
#data <- data[!is.na(data$BsmtExposure), ]
data$BsmtCond = 
  as.numeric(factor(data$BsmtCond,levels=unique(data$BsmtCond)))

#Removing Column BsmtFinType1 and Type2
sum(is.na(data$BsmtFinType1))
unique(data$BsmtFinType1)
library(tidyverse)
ggplot(data,aes(BsmtFinType1))+
  geom_bar()
data <- subset(data, select = -c(BsmtFinType1, BsmtFinType2))

#Remove FireQuality as there are too many NAs
str(data$FireplaceQu)
sum(is.na(data$FireplaceQu))
ggplot(data,aes(FireplaceQu))+
  geom_bar()
data <- subset(data, select = -c(FireplaceQu))

#Replace NA in GarageType with Attchd
(unique(data$GarageType))
sum(is.na(data$GarageType))
ggplot(data,aes(GarageType))+
  geom_bar()
data$GarageType[is.na(data$GarageType)]="Attchd"

#Replace the Garage Year Built NA with mean, change the years to numberic with 2010 as base. 
sum(is.na(data$GarageYrBlt))
max(data$GarageYrBlt[!is.na(data$GarageYrBlt)])
data$GarageYrBlt = 2010 - data$GarageYrBlt
mean(data$GarageYrBlt[!is.na(data$GarageYrBlt)])
data$GarageYrBlt[is.na(data$GarageYrBlt)]=mean(data$GarageYrBlt[!is.na(data$GarageYrBlt)])

#Analysis on GarageFinish
sum(is.na(data$GarageFinish))
table(data$GarageFinish)
data$GarageFinish = 
  as.numeric(factor(data$GarageFinish,levels=unique(data$GarageFinish)))
data$GarageFinish[is.na(data$GarageFinish)]=2

#On GarageQual
sum(is.na(data$GarageQual))
table(data$GarageQual)
data$GarageQual[is.na(data$GarageQual)]="TA"
data$GarageQual = 
  as.numeric(factor(data$GarageQual,levels=unique(data$GarageQual)))

#On GarageCond
sum(is.na((data$GarageCond)))
data$GarageCond[is.na(data$GarageCond)]="TA"
sum(is.na(data))

colSums(is.na(data))
unique(data$Electrical)
str(data)

#Factoring and Cleaning all character params
data$LotConfig = 
  as.numeric(factor(data$LotConfig,levels=unique(data$LotConfig)))
data$LandContour = 
  as.numeric(factor(data$LandContour,levels=unique(data$LandContour)))
data$Utilities = 
  as.numeric(factor(data$Utilities,levels=unique(data$Utilities)))
data$KitchenQual = 
  as.numeric(factor(data$KitchenQual,levels=unique(data$KitchenQual)))
data$GarageArea[is.na(data$GarageArea)]=mean(data$GarageArea[!is.na(data$GarageArea)])

data <- subset(data, select = -LandSlope)
data <- subset(data, select = -Neighborhood)
data <- subset(data, select = -c(Condition1,Condition2))
data <- subset(data, select = -RoofMatl)
data <- subset(data, select = -Exterior1st)
data <- subset(data, select = -Exterior2nd)
data <- subset(data, select = -Functional)
data <- subset(data, select = -GarageCars)
data <- subset(data, select = -SaleType)
data <- subset(data, select = -HouseStyle)
table(data$KitchenQual)
data$KitchenQual[is.na(data$KitchenQual)]=1
colnames(data)
colnames(dataTest)
View(data)
#Data Splitting 
library(caTools)
split = sample.split(data$SalePrice,SplitRatio = 0.75) 
dataTrain = subset(data,split==TRUE)
dataTest = subset(data,split==FALSE)
unique(dataTrain$RoofStyle)
colSums(is.na(data))
#Best Modelling
Linear_Reg_Model = lm(SalePrice~MSSubClass+LotFrontage+LotArea+OverallQual+
                        MasVnrArea+TotalBsmtSF+GrLivArea+BedroomAbvGr+
                        KitchenQual+GarageArea+PoolArea,data=dataTrain)

#Predictions of the Model
Predictions <- predict(Linear_Reg_Model,newdata = dataTest)
class(Predictions)
rmse <- sqrt(mean((Predictions-data$SalePrice)^2))
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
class(dataTest$SalePrice)
Predictions = as.integer(Predictions)
View(data)
plot(dataTest$LotArea, dataTest$SalePrice, col = "blue", pch = 16,  
     main = "Actual vs. Predicted Values")
points(dataTest$LotArea, Predictions, col = "red", pch = 17)
legend("topleft", legend = c("Actual", "Predicted"), 
       col = c("blue", "red"), pch = c(16, 17))
#Plotting the Box Plot and dont find much outliers in the required params
for (col in colnames(data)) {
  boxplot(data[, col], main = paste("Box Plot for", col))
}
nrow(data$MSSubClass)
result_df <- data.frame(Id= data$Id, SalePrice = Predictions)
write.csv(result_df, file = "sample_submission.csv", row.names = FALSE)
sum(is.na(result_df))
result_df$SalePrice[is.na(result_df$SalePrice)]=
  mean(result_df$SalePrice[!is.na(result_df$SalePrice)])
