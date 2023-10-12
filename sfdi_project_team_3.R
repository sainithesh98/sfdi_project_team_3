#Data Collection
data = read.csv("train.csv")
View(data)
str(data)
summary(data)

#----------------Data Visualization----------------- 
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
