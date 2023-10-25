#Importing all the neccessary Libraries
library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
install.packages("gridExtra")
library(gridExtra)
library(scales)
install.packages("Rmisc")
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)
install.packages("relaimpo")
library(relaimpo)
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
library(randomForest)

#Training and testing data
train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)
test_final <- read.csv("test.csv", stringsAsFactors = F)

#Data Size and Structure 
dim(train)
str(train[,c(1:10, 81)]) #display first 10 variables and the response variable

#Dropping the IDs but keeping the test IDs in a vector. These are needed to compose the 
#submission file
test_labels <- test$Id
test$Id <- NULL
train$Id <- NULL
test$SalePrice <- NA
all <- rbind(train,test)
dim(all)

#Exploring the SalesPrice Variable
ggplot(data=all[!is.na(all$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000))
summary(all$SalePrice)

#Correlations with the Sales Price
numericVars <- which(sapply(all, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')
## There are 37 numeric variables
all_numVar <- all[, numericVars]
#correlations of all numeric variables
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") 

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")


#Exploring the Overall Quality Variable
ggplot(data=all[!is.na(all$SalePrice),], aes(x=factor(OverallQual), y=SalePrice))+
  geom_boxplot(col='blue') + labs(x='Overall Quality') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000))

#Above Grade Living Area (square feet) Variable
ggplot(data=all[!is.na(all$SalePrice),], aes(x=GrLivArea, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_text_repel(aes(label = ifelse(all$GrLivArea[!is.na(all$SalePrice)]>4500, rownames(all), 
                                     '')))


#Missing data, Label Encoding, Factorize variables with both Training and testing dataset. 
NAcol <- which(colSums(is.na(all)) > 0)
sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE)
cat('There are', length(NAcol), 'columns with missing values')

#----------------> Imputation of Missing Data <-----------------------

#PoolQC 
unique(all$PoolQC)
all$PoolQC[is.na(all$PoolQC)] <- 'None'
Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
all$PoolQC<-as.integer(revalue(all$PoolQC, Qualities))
table(all$PoolQC)

#Miscellaneous Feature
all$MiscFeature[is.na(all$MiscFeature)] <- 'None'
all$MiscFeature <- as.numeric(factor(all$MiscFeature))
unique(all$MiscFeature)
ggplot(all[!is.na(all$SalePrice),], aes(x=MiscFeature, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))
table(all$MiscFeature)
sum(is.na(all$MiscFeature))

#Alley
all$Alley[is.na(all$Alley)] <- 'None'
all$Alley <- as.factor(all$Alley)
ggplot(all[!is.na(all$SalePrice),], aes(x=Alley, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 200000, by=50000), labels = comma)
table(all$Alley)

#Fence
all[!is.na(all$SalePrice),] %>% group_by(Fence) %>% summarise(median = median(SalePrice), 
                                                              counts=n())
all$Fence[is.na(all$Fence)] <- 'None'
table(all$Fence)
#Convert the Fence to Factor
all$Fence <- as.numeric(factor(all$Fence))

#Fireplace Variable
all$FireplaceQu[is.na(all$FireplaceQu)] <- 'None'
all$FireplaceQu<-as.integer(revalue(all$FireplaceQu, Qualities))
table(all$FireplaceQu)
table(all$Fireplaces)
sum(table(all$Fireplaces))


#Lot Variables
ggplot(all[!is.na(all$LotFrontage),], aes(x=as.factor(Neighborhood), y=LotFrontage)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
for (i in 1:nrow(all)){
  if(is.na(all$LotFrontage[i])){
    all$LotFrontage[i] <- as.integer(median(all$LotFrontage[all$Neighborhood==all$Neighborhood[i]], 
                                            na.rm=TRUE)) 
  }
}
all$LotShape<-as.integer(revalue(all$LotShape, c('IR3'=0, 'IR2'=1, 'IR1'=2, 'Reg'=3)))
table(all$LotShape)
sum(table(all$LotShape))

ggplot(all[!is.na(all$SalePrice),], aes(x=as.factor(LotConfig), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))
all$LotConfig <- as.numeric(factor(all$LotConfig))
table(all$LotConfig)


#Garage Variable
all$GarageYrBlt[is.na(all$GarageYrBlt)] <- all$YearBuilt[is.na(all$GarageYrBlt)]
length(which(is.na(all$GarageType) & is.na(all$GarageFinish) & is.na(all$GarageCond) & 
               is.na(all$GarageQual)))
kable(all[!is.na(all$GarageType) & is.na(all$GarageFinish), 
          c('GarageCars', 'GarageArea', 'GarageType', 'GarageCond', 'GarageQual', 
            'GarageFinish')])
all$GarageCond[2127] <- names(sort(-table(all$GarageCond)))[1]
all$GarageQual[2127] <- names(sort(-table(all$GarageQual)))[1]
all$GarageFinish[2127] <- names(sort(-table(all$GarageFinish)))[1]

#display "fixed" house
kable(all[2127, c('GarageYrBlt', 'GarageCars', 'GarageArea', 'GarageType', 
                  'GarageCond', 'GarageQual', 'GarageFinish')])

all$GarageCars[2577] <- 0
all$GarageArea[2577] <- 0
all$GarageType[2577] <- NA
length(which(is.na(all$GarageType) & is.na(all$GarageFinish) & 
               is.na(all$GarageCond) & is.na(all$GarageQual)))

all$GarageType[is.na(all$GarageType)] <- 'No Garage'
all$GarageType <- as.numeric(factor(all$GarageType))
table(all$GarageType)

all$GarageFinish[is.na(all$GarageFinish)] <- 'None'
Finish <- c('None'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3)

all$GarageFinish<-as.numeric(factor(all$GarageFinish, Finish))
table(all$GarageFinish)

all$GarageQual[is.na(all$GarageQual)] <- 'None'
all$GarageQual<-as.integer(revalue(all$GarageQual, Qualities))
table(all$GarageQual)

all$GarageCond[is.na(all$GarageCond)] <- 'None'
all$GarageCond<-as.integer(revalue(all$GarageCond, Qualities))
table(all$GarageCond)


#Basement Variables
length(which(is.na(all$BsmtQual) & is.na(all$BsmtCond) & is.na(all$BsmtExposure) & 
               is.na(all$BsmtFinType1) & is.na(all$BsmtFinType2)))
all[!is.na(all$BsmtFinType1) & (is.na(all$BsmtCond)|is.na(all$BsmtQual)|is.na(all$BsmtExposure)|is.na(all$BsmtFinType2)),
    c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2')]

all$BsmtFinType2[333] <- names(sort(-table(all$BsmtFinType2)))[1]
all$BsmtExposure[c(949, 1488, 2349)] <- names(sort(-table(all$BsmtExposure)))[1]
all$BsmtCond[c(2041, 2186, 2525)] <- names(sort(-table(all$BsmtCond)))[1]
all$BsmtQual[c(2218, 2219)] <- names(sort(-table(all$BsmtQual)))[1]

all$BsmtQual[is.na(all$BsmtQual)] <- 'None'
all$BsmtQual<-as.integer(revalue(all$BsmtQual, Qualities))
table(all$BsmtQual)


all$BsmtCond[is.na(all$BsmtCond)] <- 'None'
all$BsmtCond<-as.integer(revalue(all$BsmtCond, Qualities))
table(all$BsmtCond)

all$BsmtExposure[is.na(all$BsmtExposure)] <- 'None'
Exposure <- c('None'=0, 'No'=1, 'Mn'=2, 'Av'=3, 'Gd'=4)

all$BsmtExposure<-as.integer(revalue(all$BsmtExposure, Exposure))
table(all$BsmtExposure)

all$BsmtFinType1[is.na(all$BsmtFinType1)] <- 'None'
FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
all$BsmtFinType1<-as.integer(revalue(all$BsmtFinType1, FinType))
table(all$BsmtFinType1)

all$BsmtFinType2[is.na(all$BsmtFinType2)] <- 'None'
FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)

all$BsmtFinType2<-as.integer(revalue(all$BsmtFinType2, FinType))
table(all$BsmtFinType2)

all[(is.na(all$BsmtFullBath)|is.na(all$BsmtHalfBath)|is.na(all$BsmtFinSF1)|is.na(all$BsmtFinSF2)|
       is.na(all$BsmtUnfSF)|is.na(all$TotalBsmtSF)), c('BsmtQual', 'BsmtFullBath', 'BsmtHalfBath', 
                                                       'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 
                                                       'TotalBsmtSF')]

all$BsmtFullBath[is.na(all$BsmtFullBath)] <-0
table(all$BsmtFullBath)

all$BsmtHalfBath[is.na(all$BsmtHalfBath)] <-0
table(all$BsmtHalfBath)

all$BsmtFinSF1[is.na(all$BsmtFinSF1)] <-0

all$BsmtFinSF2[is.na(all$BsmtFinSF2)] <-0

all$BsmtUnfSF[is.na(all$BsmtUnfSF)] <-0

all$TotalBsmtSF[is.na(all$TotalBsmtSF)] <-0


#Masonry Variable
length(which(is.na(all$MasVnrType) & is.na(all$MasVnrArea)))
all[is.na(all$MasVnrType) & !is.na(all$MasVnrArea), c('MasVnrType', 'MasVnrArea')]
all$MasVnrType[2611] <- names(sort(-table(all$MasVnrType)))[2] 
all[2611, c('MasVnrType', 'MasVnrArea')]


all$MasVnrType[is.na(all$MasVnrType)] <- 'None'
all[!is.na(all$SalePrice),] %>% group_by(MasVnrType) %>% summarise(median = median(SalePrice), 
                                                                   counts=n()) %>% arrange(median)
Masonry <- c('None'=0, 'BrkCmn'=0, 'BrkFace'=1, 'Stone'=2)
all$MasVnrType<-as.integer(revalue(all$MasVnrType, Masonry))
table(all$MasVnrType)
all$MasVnrArea[is.na(all$MasVnrArea)] <-0


#MSZoning
all$MSZoning[is.na(all$MSZoning)] <- names(sort(-table(all$MSZoning)))[1]
all$MSZoning <- as.numeric(factor(all$MSZoning))
table(all$MSZoning)
sum(table(all$MSZoning))

#Kitchen Variables
all$KitchenQual[is.na(all$KitchenQual)] <- 'TA' #replace with most common value
all$KitchenQual<-as.numeric(factor(all$KitchenQual, Qualities))
table(all$KitchenQual)
sum(table(all$KitchenQual))

table(all$KitchenAbvGr)
sum(table(all$KitchenAbvGr))

#Utilities
table(all$Utilities)
kable(all[is.na(all$Utilities) | all$Utilities=='NoSeWa', 1:9])
all$Utilities <- NULL

dim(train)
#Home Functionality
all$Functional[is.na(all$Functional)] <- names(sort(-table(all$Functional)))[1]
all$Functional <- as.integer(revalue(all$Functional, c('Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)))
table(all$Functional)
sum(table(all$Functional))

#Exterior Variables
all$Exterior1st[is.na(all$Exterior1st)] <- names(sort(-table(all$Exterior1st)))[1]
all$Exterior1st <- as.numeric(factor(all$Exterior1st))
table(all$Exterior1st)

all$Exterior2nd[is.na(all$Exterior2nd)] <- names(sort(-table(all$Exterior2nd)))[1]
all$Exterior2nd <- as.numeric(factor(all$Exterior2nd))
table(all$Exterior2nd)

all$ExterQual<-as.integer(factor(all$ExterQual, Qualities))
table(all$ExterQual)
sum(table(all$ExterQual))

#ExtCond No NAs
all$ExterCond[is.na(all$ExterCond)] <- 'None'
all$ExterCond<-as.integer(factor(all$ExterCond, Qualities))
table(all$ExterCond)
sum(is.na(all$ExterCond))

#Electrical System 
#imputing mode
all$Electrical[is.na(all$Electrical)] <- names(sort(-table(all$Electrical)))[1]

all$Electrical <- as.numeric(factor(all$Electrical))
table(all$Electrical)

#Sale Type and Condition 
#imputing mode
all$SaleType[is.na(all$SaleType)] <- names(sort(-table(all$SaleType)))[1]

all$SaleType <- as.numeric(factor(all$SaleType))
table(all$SaleType)

sum(table(all$SaleType))

all$SaleCondition <- as.factor(all$SaleCondition)
table(all$SaleCondition)

#Cross check if there are any NAs in the dataset.
NAcol <- which(colSums(is.na(all)) > 0)
sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE)


#------------->Label encoding and Factoring the character variables.<----------------

Charcol <- names(all[,sapply(all, is.character)])
Charcol
cat('There are', length(Charcol), 'remaining columns with character values')

#No ordinality, so converting into factors
all$Foundation <- as.numeric(factor(all$Foundation))
table(all$Foundation)
sum(table(all$Foundation))


#Heating and Airco
all$Heating <- as.numeric(factor(all$Heating))
table(all$Heating)

all$HeatingQC<-as.integer(factor(all$HeatingQC, Qualities))
table(all$HeatingQC)
sum(table(all$HeatingQC))

all$CentralAir<-as.integer(factor(all$CentralAir, c('N'=0, 'Y'=1)))
table(all$CentralAir)


#RoofStyle
all$RoofStyle <- as.numeric(factor(all$RoofStyle))
table(all$RoofStyle)

#No ordinality, so converting into factors
all$RoofMatl <- as.numeric(factor(all$RoofMatl))
table(all$RoofMatl)

#LandContour
#No ordinality, so converting into factors
all$LandContour <- as.numeric(factor(all$LandContour))
table(all$LandContour)

all$LandSlope<-as.integer(factor(all$LandSlope, c('Sev'=0, 'Mod'=1, 'Gtl'=2)))
table(all$LandSlope)

#Dwelling
ggplot(all[!is.na(all$SalePrice),], aes(x=as.factor(BldgType), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

#No ordinality, so converting into factors
all$BldgType <- as.numeric(factor(all$BldgType))
table(all$BldgType)

#No ordinality, so converting into factors
all$HouseStyle <- as.numeric(factor(all$HouseStyle))
table(all$HouseStyle)

#Neighborhood and Condition
all$Neighborhood <- as.numeric(factor(all$Neighborhood))
table(all$Neighborhood)

all$Condition1 <- as.numeric(factor(all$Condition1))
table(all$Condition1)

all$Condition2 <- as.numeric(factor(all$Condition2))
table(all$Condition2)

#Pavement of Streets and Driveway
all$Street<-as.integer(factor(all$Street, c('Grvl'=0, 'Pave'=1)))
table(all$Street)

all$PavedDrive<-as.integer(factor(all$PavedDrive, c('N'=0, 'P'=1, 'Y'=2)))
table(all$PavedDrive)


#Factoring some numberic to factors
all$MoSold <- as.numeric(factor(all$MoSold))
ys <- ggplot(all[!is.na(all$SalePrice),], aes(x=as.factor(YrSold), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 800000, by=25000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
  coord_cartesian(ylim = c(0, 200000)) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice

ms <- ggplot(all[!is.na(all$SalePrice),], aes(x=MoSold, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 800000, by=25000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
  coord_cartesian(ylim = c(0, 200000)) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice
grid.arrange(ys, ms, widths=c(1,2))


#Revalue the MSSubClass
all$MSSubClass <- as.numeric(factor(all$MSSubClass))

#revalue for better readability
all$MSSubClass<-factor(all$MSSubClass, c('20'='1 story 1946+', '30'='1 story 1945-', 
                                          '40'='1 story unf attic', '45'='1,5 story unf', 
                                          '50'='1,5 story fin', '60'='2 story 1946+', 
                                          '70'='2 story 1945-', '75'='2,5 story all ages', 
                                          '80'='split/multi level', '85'='split foyer', 
                                          '90'='duplex all style/age', 
                                          '120'='1 story PUD 1946+', '150'='1,5 story PUD all', 
                                          '160'='2 story PUD 1946+', '180'='PUD multilevel', 
                                          '190'='2 family conversion'))
str(all$MSSubClass)


#--------------> Visualization of Important Variables <--------------------------------
numericVars <- which(sapply(all, is.numeric)) #index vector numeric variables
factorVars <- which(sapply(all, is.factor)) #index vector factor variables
cat('There are', length(numericVars), 'numeric variables, and', length(factorVars), 'categoric variables')


#Correlation of the varibles
all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)
dim(all)
set.seed(2018)
quick_RF <- randomForest(x=all, y=all$SalePrice, ntree=100,importance=TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + 
  geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + 
  coord_flip() + theme(legend.position="none")
colnames(all)
#Remove the variables which is less than 5% for imputions using RF
all <- subset(all, select = -c(LotArea,BsmtQual,YearBuilt,Fireplaces,
                               BsmtFullBath,ExterQual,GarageType))
#------------------------> Feature Engineering ---------------------------------->
all$TotBathrooms <- all$FullBath + (all$HalfBath*0.5) + all$BsmtFullBath + (all$BsmtHalfBath*0.5)
all$Remod <- ifelse(all$YearBuilt==all$YearRemodAdd, 0, 1) #0=No Remodeling, 1=Remodeling
all$Age <- as.numeric(all$YrSold)-all$YearRemodAdd
all$IsNew <- ifelse(all$YrSold==all$YearBuilt, 1, 0)
table(all$IsNew)

all$NeighRich[all$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 2
all$NeighRich[!all$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale', 'StoneBr', 'NridgHt', 'NoRidge')] <- 1
all$NeighRich[all$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale')] <- 0

table(all$NeighRich)
all$TotalPorchSF <- all$OpenPorchSF + all$EnclosedPorch + all$X3SsnPorch + all$ScreenPorch


#Dropping Highly Correlated ones
dropVars <- c('YearRemodAdd', 'GarageYrBlt', 'GarageArea', 'GarageCond', 'TotalBsmtSF', 'TotalRmsAbvGrd', 
              'BsmtFinSF1')

all <- all[,!(names(all) %in% dropVars)]

numericVarNames <- numericVarNames[!(numericVarNames %in% c('MSSubClass', 'MoSold', 'YrSold', 'SalePrice', 
                                                            'OverallQual', 'OverallCond'))] 
#numericVarNames was created before having done anything
numericVarNames <- append(numericVarNames, c('Age', 'TotalPorchSF', 'TotBathrooms', 'TotalSqFeet'))

DFnumeric <- all[, names(all) %in% numericVarNames]

DFfactors <- all[, !(names(all) %in% numericVarNames)]
DFfactors <- DFfactors[, names(DFfactors) != 'SalePrice']

cat('There are', length(DFnumeric), 'numeric variables, and', length(DFfactors), 'factor variables')


#Skewness and Normalization of Data
for(i in 1:ncol(DFnumeric)){
  if (abs(skew(DFnumeric[,i]))>0.8){
    DFnumeric[,i] <- log(DFnumeric[,i] +1)
  }
}
PreNum <- preProcess(DFnumeric, method=c("center", "scale"))
print(PreNum)
#Normalize the data
DFnorm <- predict(PreNum, DFnumeric)
dim(DFnorm)

#ONE-HOT ENCODE : Predictors are converted to numeric
DFdummies <- as.data.frame(model.matrix(~.-1, DFfactors))
dim(DFdummies)

ZerocolTest <- which(colSums(DFdummies[(nrow(all[!is.na(all$SalePrice),])+1):nrow(all),])==0)

colnames(DFdummies[ZerocolTest])
DFdummies <- DFdummies[,-ZerocolTest]

ZerocolTrain <- which(colSums(DFdummies[1:nrow(all[!is.na(all$SalePrice),]),])==0)
colnames(DFdummies[ZerocolTrain])

fewOnes <- which(colSums(DFdummies[1:nrow(all[!is.na(all$SalePrice),]),])<10)
colnames(DFdummies[fewOnes])

DFdummies <- DFdummies[,-fewOnes] #removing predictors
dim(DFdummies)

skew(all$SalePrice)
colnames(all)
qqnorm(all$SalePrice)
qqline(all$SalePrice)
all$SalePrice <- log(all$SalePrice) 
#default is the natural logarithm, "+1" is not necessary as there are no 0's
skew(all$SalePrice)
qqnorm(all$SalePrice)
qqline(all$SalePrice)

combined <- cbind(DFnorm, DFdummies) #combining all (now numeric) predictors into one dataframe 
train1 <- combined[!is.na(all$SalePrice),]
test1 <- combined[is.na(all$SalePrice),]
colnames(all)
str(all)
ncol(all)
max(all$YrSold)
all$YrSold = 2010 - all$YrSold
all <- subset(all, select = -Alley)
all <- subset(all, select = -c(MSSubClass,HeatingQC))
all <- subset(all, select = -c(GarageFinish, KitchenQual))
all <- subset(all, select = -c(ExterCond))
all <- subset(all, select = -c(TotRmsAbvGrd))
all <- subset(all, select = -Street)
all <- subset(all, select = -LandSlope)
all <- subset(all, select = -CentralAir)
all <- subset(all, select = -PavedDrive)
sum(is.na(all))
#--------------------> Modelling <--------------------------
library(caTools)
colSums(is.na(all))
train <- all[!is.na(all$SalePrice),]
test <- all[is.na(all$SalePrice),]
test$Id <- test_final$Id
split = sample.split(train$SalePrice,SplitRatio = 0.75) 
dataTrain = subset(train,split==TRUE)
dataTest = subset(train,split==FALSE)
colnames(train)
set.seed(27042018)
colSums(is.na(all))
str(train)

#-----------------Running the Linear Regression Model-----------------------
Linear_Reg_Model = lm(SalePrice~.,data=dataTrain)
Linear_Reg_Model_1 = lm(SalePrice~MSSubClass+LotFrontage+LotArea+OverallQual+
                          MasVnrArea+GrLivArea+BedroomAbvGr+
                          KitchenQual+GarageCars+PoolArea,data=dataTrain)

summary(Linear_Reg_Model)
Predictions <- predict(Linear_Reg_Model,newdata = test)
dim(Predictions)
Predictions = as.integer(Predictions)
class(Predictions)
rmse <- (mean((Predictions-dataTest$SalePrice)^2))
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
class(dataTest$SalePrice)
result_df <- data.frame(Id= test$Id, SalePrice = Predictions*10^5)
write.csv(result_df, file = "sample_submission.csv", row.names = FALSE)
sum(is.na(result_df))


#------------------------ Decision Tree Model --------------------------------
DecisionTree=rpart(SalePrice~.,method="class",data=dataTrain)
prp(StevensTree)


ScreenPorch+WoodDeckSF+GarageCars+Functional+BsmtFullBath+X2ndFlrSF+X1stFlrSF+BsmtExposure+
  RoofMatlWdShngl+RoofMatlWdShake+RoofMatlTar&Grv+RoofMatlRoll+RoofMatlMetal+RoofMatlMembran+
  RoofMatlCompShg+YearBuilt+OverallCond+OverallQual+Condition2PosN+Condition1Norm+NeighborhoodStoneBr+
  NeighborhoodNridgHt+LotArea+MSZoningRM+MSZoningRL+MSZoningRH+MSZoningFV


#-----------------------------Random Forest Model-----------------------------
rf_model <- randomForest(SalePrice ~ NeighRich+PoolQC+PoolArea+X3SsnPorch+MiscFeature+Condition2+
                           LowQualFinSF+BsmtHalfBath+MiscVal+BsmtFinSF2+Heating+RoofMatl+
                           ScreenPorch+SaleType+LotConfig+Electrical+BsmtFinType2+
                           RoofStyle+Fence+HalfBath+Condition1+LandContour+MasVnrType+LotShape+
                           HouseStyle+EnclosedPorch+BldgType+YrSold+Functional+BsmtCond+
                           Exterior1st+KitchenAbvGr+SaleCondition+Exterior2nd+MoSold+BedroomAbvGr+
                           BsmtExposure+WoodDeckSF+GarageQual+MasVnrArea+Neighborhood+TotalPorchSF+
                           Foundation+OpenPorchSF
                           , data = dataTrain,importance = TRUE)
imp_RF <- importance(rf_model)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
dim(imp_DF)
dim(dataTrain)
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + 
  geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + 
  coord_flip() + theme(legend.position="none")
colnames(all)
summary(rf_model)

predictions <- predict(rf_model, newdata = dataTest)
rmse <- sqrt(mean(((predictions)*(10^5)-dataTest$SalePrice)^2))
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
result_rf <- data.frame(Id= test$Id, SalePrice = predictions*10^5)
write.csv(result_rf, file = "sample_submission.csv", row.names = FALSE)
sum(is.na(result_rf))
