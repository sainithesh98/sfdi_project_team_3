#Data Collection
data = read.csv("train.csv")
View(data)
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
View(data)
colSums(is.na(data))
# Columns with NAs are : LotFrontage, MasVnrType, MasVnrArea, BsmtQual, BsmtCond,
# BsmtExposure, BsmtFinType1, BsmtFinType2, FireplaceQu, GarageType, GarageYrBlt, 
# GarageFinish, GarageQual, GarageCond, -(PoolQC, Fence, MiscFeature) 

#Columns which can be removed: PoolQC, MiscFeature, Fence
data <- subset(data, select = -PoolQC)
data <- subset(data, select = -c(MiscFeature, Fence))
View(data)

summary(data$LotFrontage)

#Replacing the NA values for 
data$LotFrontage
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
data$BsmtQual
data <- data[!is.na(data$BsmtQual), ]
data$BsmtQual = as.numeric(factor(data$BsmtQual,levels=unique(data$BsmtQual)))

#Removed the NA in BsmtExposure
str(data$BsmtCond)
unique(data$BsmtExposure)
sum(is.na(data$BsmtExposure))
data <- data[!is.na(data$BsmtExposure), ]
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
data <- subset(data, select = -LandSlope)
data <- subset(data, select = -Neighborhood)
data <- subset(data, select = -c(Condition1,Condition2))


#Data Splitting 
library(caTools)
split = sample.split(data$SalePrice,SplitRatio = 0.75) 
dataTrain = subset(data,split==TRUE)
dataTest = subset(data,split==FALSE)
Linear_Reg_Model = lm(SalePrice~.,data=dataTrain)

Predictions <- predict(Linear_Reg_Model,newdata = dataTest)
rmse <- sqrt(mean((Predictions-dataTest$SalePrice)^2))
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
