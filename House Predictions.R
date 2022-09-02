library(data.table)
library(Matrix)
library(xgboost)
require(randomForest)
require(dplyr)
library(plyr)
library(ggplot2)
library(stringr)
library(caret)
library(ggplot2)
library(scales) #Used to remove exponential in ggplot2
library(randomForest)
library(psych)
library(corrplot)

train <- read.csv("C:/Users/francisco.alamo/OneDrive - Essex County Council/Desktop/Kaggle Projects/Predict House Prices/train.csv")

test <- read.csv("C:/Users/francisco.alamo/OneDrive - Essex County Council/Desktop/Kaggle Projects/Predict House Prices/test.csv")

test_labels <- test$Id
test$Id <- NULL
train$Id <- NULL

test$SalePrice <- NA
train2 <- rbind(train, test)
train <- rbind(train,test)
dim(train2)

ggplot(data=train[!is.na(train$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000),labels = comma)

summary(train$SalePrice)

#train2 <- train

names(train2)

#Check for Na values
cbind(
  lapply(
    lapply(train, is.na)
    , sum)
)
#or
NAcol <- which(colSums(is.na(train)) > 0)
sort(colSums(sapply(train[NAcol], is.na)), decreasing = TRUE)

#Val that are character
Charcol <- names(train[,sapply(train, is.character)])
Charcol

NumCol <- names(train[,sapply(train, is.numeric)])
# Moving variables to binary

#Street and Paved variables
#Type of road access to property is paved or not
table(train$Street)
sum(is.na(train$Street))

train2$Street[train$Street == "Pave"] <- 1
train2$Street[train$Street != "Pave"] <- 0

train2$Street <-as.integer(train2$Street)

table(train2$Street)

ggplot(train[!is.na(train$SalePrice),], aes(x=Alley, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

#Assuming there are not missing values. 
# Alley: Type of alley access to property
#Could be also consider as a factor
table(train$Alley)
sum(is.na(train$Alley))
train2$Alley[is.na(train$Alley)] <- "Na" 

train2$Alley <- as.factor(train2$Alley)

table(train2$Alley)

#PavedDrive 
table(train$PavedDrive)
sum(is.na(train$PavedDrive))
train$PavedDrive[is.na(train$PavedDrive)] <- "Na"

train2$PavedDrive[train$PavedDrive == "Y"] <- 2
train2$PavedDrive[train$PavedDrive == "P"] <- 1
train2$PavedDrive[train$PavedDrive == "N"] <- 0

train2$PavedDrive <- as.integer(train2$PavedDrive)

table(train2$PavedDrive)

ggplot(train2[!is.na(train2$SalePrice),], aes(x=PavedDrive, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

#Lot variables
#General shape of property regular vs irregular
table(train$LotShape)
sum(is.na(train$LotShape))

train2$LotShape <- as.factor(train2$LotShape)

table(train2$LotShape)

ggplot(train2[!is.na(train2$SalePrice),], aes(x=LotFrontage, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

#Linear feet of street connected to property
table(train$LotFrontage)
sum(is.na(train$LotFrontage))

train2$LotFrontage[is.na(train$LotFrontage)] <- 0 #Needs further exploration

train2$LotFrontage <- as.integer(train2$LotFrontage)

table(train2$LotFrontage)

#Lot Configuration
table(train$LotConfig)
sum(is.na(train$LotConfig))
train2$LotConfig[is.na(train$LotConfig)] <- "Na"

train2$LotConfig <- as.factor(train2$LotConfig)

table(train2$LotConfig)

ggplot(train3[!is.na(train3$SalePrice),], aes(x=HouseStyle
                                              , y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))
#Lot Area
table(train$LotArea)
sum(is.na(train$LotArea))
train2$LotArea[is.na(train$LotArea)] <- 0

train2$LotArea <- as.integer(train2$LotArea)

table(train2$LotArea)

ggplot(train2[!is.na(train2$SalePrice),], aes(x=LotArea, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))
#Utilitie / full services vs not full services
table(train$Utilities)
sum(is.na(train2$Utilities))
train2$Utilities[is.na(train$Utilities)] <- 0

train2$Utilities[train$Utilities == "AllPub"] <- 1
train2$Utilities[train$Utilities != "AllPub"] <- 0

train2$Utilities <- as.integer(train2$Utilities)

table(train2$Utilities)

#Heating and AC
#Central Air conditioning
table(train$CentralAir)
sum(is.na(train$CentralAir))
train2$CentralAir[is.na(train$CentralAir)] <- "Na"

train2$CentralAir[train$CentralAir == "Y"] <- 1
train2$CentralAir[train$CentralAir != "Y"] <- 0

train2$CentralAir <- as.integer(train2$CentralAir)

table(train2$CentralAir)

#Type of heating
table(train$Heating)
sum(is.na(train$Heating))
train2$Heating[is.na(train$Heating)] <- "Na"

train2$Heating <- as.factor(train2$Heating)

ggplot(train2[!is.na(train2$SalePrice),], aes(x=Heating, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))


table(train2$Heating)

# Heating quality and condition
table(train$HeatingQC)
sum(is.na(train$HeatingQC))
train2$HeatingQC[is.na(train$HeatingQC)] <- 0

train2$HeatingQC[train$HeatingQC == "Ex"] <- 5
train2$HeatingQC[train$HeatingQC == "Gd"] <- 4
train2$HeatingQC[train$HeatingQC == "TA"] <- 3
train2$HeatingQC[train$HeatingQC == "Fa"] <- 2
train2$HeatingQC[train$HeatingQC == "Po"] <- 1

train2$HeatingQC <- as.integer(train2$HeatingQC)

table(train2$HeatingQC)

#Land of property
# Slope of property (we assume that low slope would bring more value, as you can build more easily)
table(train$LandSlope)
sum(is.na(train$LandSlope))
train2$LandSlope[is.na(train$LandSlope)] <- "Na"

train2$LandSlope <- as.factor(train2$LandSlope)

table(train2$LandSlope)

#I decided to keep it as a factor as there is not an ordinal level
ggplot(train[!is.na(train$SalePrice),], aes(x=LandSlope, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))


#Land flatness of the property
table(train$LandContour)
sum(is.na(train$LandContour))
train2$LandContour[is.na(train$LandContour)] <- "Na"

train2$LandContour <- as.factor(train2$LandContour)

table(train2$LandContour)

ggplot(train[!is.na(train$SalePrice),], aes(x=LandContour, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

##############################
#Basement evaluation height
table(train$BsmtQual)
sum(is.na(train2$BsmtQual))
#First finding NA means not basement, can be replaced with 0
train2$BsmtQual[is.na(train$BsmtQual)] <- 0

train2$BsmtQual[train$BsmtQual == "Ex"] <- 5
train2$BsmtQual[train$BsmtQual == "Gd"] <- 4
train2$BsmtQual[train$BsmtQual == "TA"] <- 3
train2$BsmtQual[train$BsmtQual == "Fa"] <- 2
train2$BsmtQual[train$BsmtQual == "Po"] <- 1

train2$BsmtQual <- as.integer(train2$BsmtQual)

table(train2$BsmtQual)

ggplot(train[!is.na(train$SalePrice),], aes(x=BsmtQual, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))
#Basement general conditions of the basement
table(train$BsmtCond)
sum(is.na(train$BsmtCond))
train2$BsmtCond[is.na(train$BsmtCond)] <- 0

train2$BsmtCond[train$BsmtCond == "Ex"] <- 5
train2$BsmtCond[train$BsmtCond == "Gd"] <- 4
train2$BsmtCond[train$BsmtCond == "TA"] <- 3
train2$BsmtCond[train$BsmtCond == "Fa"] <- 2
train2$BsmtCond[train$BsmtCond == "Po"] <- 1

train2$BsmtCond <- as.integer(train2$BsmtCond)

table(train2$BsmtCond)
#Basement walkout or garden level walls
table(train$BsmtExposure)
sum(is.na(train$BsmtExposure))
train2$BsmtExposure[is.na(train$BsmtExposure)] <- 0

train2$BsmtExposure[train$BsmtExposure == "Gd"] <- 4
train2$BsmtExposure[train$BsmtExposure == "Av"] <- 3
train2$BsmtExposure[train$BsmtExposure == "Mn"] <- 2
train2$BsmtExposure[train$BsmtExposure == "No"] <- 1

train2$BsmtExposure <- as.integer(train2$BsmtExposure)

table(train2$BsmtExposure)

ggplot(train[!is.na(train$SalePrice),], aes(x=BsmtExposure, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))
#Basement rating of finished area
table(train$BsmtFinType1)
sum(is.na(train$BsmtFinType1))
train2$BsmtFinType1[is.na(train$BsmtFinType1)] <- 0

train2$BsmtFinType1[train$BsmtFinType1 == "GLQ"] <- 6
train2$BsmtFinType1[train$BsmtFinType1 == "ALQ"] <- 5
train2$BsmtFinType1[train$BsmtFinType1 == "BLQ"] <- 4
train2$BsmtFinType1[train$BsmtFinType1 == "Rec"] <- 3
train2$BsmtFinType1[train$BsmtFinType1 == "LwQ"] <- 2
train2$BsmtFinType1[train$BsmtFinType1 == "Unf"] <- 1

train2$BsmtFinType1 <- as.integer(train2$BsmtFinType1)

table(train2$BsmtFinType1)

# Basement type 1 finished square feet
table(train$BsmtFinSF1)
sum(is.na(train$BsmtFinSF1))
train2$BsmtFinSF1[is.na(train$BsmtFinSF1)] <- 0

train2$BsmtFinSF1 <- as.integer(train2$BsmtFinSF1)
table(train2$BsmtFinSF1)

# Basement2 rating of basement finished area (if multiple types)
table(train$BsmtFinType2)
sum(is.na(train$BsmtFinType2))
train2$BsmtFinType2[is.na(train$BsmtFinType2)] <- 0

train2$BsmtFinType2[train$BsmtFinType2 == "GLQ"] <- 6
train2$BsmtFinType2[train$BsmtFinType2 == "ALQ"] <- 5
train2$BsmtFinType2[train$BsmtFinType2 == "BLQ"] <- 4
train2$BsmtFinType2[train$BsmtFinType2 == "Rec"] <- 3
train2$BsmtFinType2[train$BsmtFinType2 == "LwQ"] <- 2
train2$BsmtFinType2[train$BsmtFinType2 == "Unf"] <- 1

train2$BsmtFinType2 <- as.integer(train2$BsmtFinType2)

table(train2$BsmtFinType2)

#Basement Type 2 finished square feet
table(train$BsmtFinSF2)
sum(is.na(train$BsmtFinSF2))
train2$BsmtFinSF2[is.na(train$BsmtFinSF2)] <- 0

train2$BsmtFinSF2 <- as.integer(train2$BsmtFinSF2)

table(train2$BsmtFinSF2)

# Total square feet of basement area
table(train$TotalBsmtSF)
sum(is.na(train$TotalBsmtSF))
train2$TotalBsmtSF[is.na(train$TotalBsmtSF)] <- 0

train2$TotalBsmtSF <- as.integer(train2$TotalBsmtSF)

table(train2$TotalBsmtSF)

# Unfinished square feet of basement area
table(train$BsmtUnfSF)
sum(is.na(train$BsmtUnfSF))
train2$BsmtUnfSF[is.na(train$BsmtUnfSF)] <- 0

train2$BsmtUnfSF <- as.integer(train2$BsmtUnfSF)

table(train2$BsmtUnfSF)

#Basement full bathroom
table(train$BsmtFullBath)
sum(is.na(train$BsmtFullBath))
train2$BsmtFullBath[is.na(train$BsmtFullBath)] <- 0

train2$BsmtFullBath <- as.integer(train2$BsmtFullBath)
table(train2$BsmtFullBath)

#Basement half bathroom (I assume not shower ?)
table(train$BsmtHalfBath)
sum(is.na(train$BsmtHalfBath))
train2$BsmtHalfBath[is.na(train$BsmtHalfBath)] <- 0

train2$BsmtHalfBath <- as.integer(train2$BsmtHalfBath)
table(train2$BsmtHalfBath)

# Electrical System
table(train$Electrical)
sum(is.na(train$Electrical))
train2$Electrical[is.na(train$Electrical)] <- names(sort(-table(train$Electrical)))[1]

train2$Electrical <- as.integer(plyr::revalue(train2$Electrical,
                                              c(Mix=1,FuseP=2,FuseF=3,
                                                FuseA=4,SBrkr=5)))

table(train2$Electrical)


ggplot(train2[!is.na(train2$SalePrice),], aes(x=Electrical, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))
#First Floor square feet
table(train$X1stFlrSF)
sum(is.na(train$X1stFlrSF))
train2$X1stFlrSF[is.na(train$X1stFlrSF)] <- 0

train2$X1stFlrSF <- as.integer(train$X1stFlrSF)

#Second Floor square feet
table(train$X2ndFlrSF)
sum(is.na(train$X2ndFlrSF))
train2$X2ndFlrSF[is.na(train$X2ndFlrSF)] <- 0

train2$X2ndFlrSF <- as.integer(train2$X2ndFlrSF)

table(train2$X2ndFlrSF)

#Low quality finished square feet (all floors)
table(train$LowQualFinSF)
sum(is.na(train$LowQualFinSF))
train2$LowQualFinSF[is.na(train$LowQualFinSF)] <- 0

train2$LowQualFinSF <- as.integer(train2$LowQualFinSF)
#Needs to be revised

#Check for Na values
cbind(
  lapply(
    lapply(train2, is.na)
    , sum)
)

#Mansonry features
#Mansonry type could be considered as a integer depending of the type of brick/stone
table(train$MasVnrArea)
sum(is.na(train$MasVnrArea))

train2$MasVnrArea <- ifelse(train$MasVnrType == "Stone" & train$MasVnrArea == 0, sample(16:1224,100),
                            ifelse(train$MasVnrType == "BrkFace" & train$MasVnrArea == 0, sample(3:1600,100),
                                          train$MasVnrArea))

train2$MasVnrArea[is.na(train$MasVnrArea)] <- 0

train2$MasVnrArea <- as.integer(train2$MasVnrArea)

table(train2$MasVnrArea)
sum(is.na(train2$MasVnrArea))

#MasVnrType: Masonry veneer type and Area Move to None
table(train$MasVnrType)
sum(is.na(train$MasVnrType))

values <- c("BrkCmn","BrkFace","Stone")

train2$MasVnrType <- ifelse(train$MasVnrType == "None" & train2$MasVnrArea != 0, sample(values,3),
                            train$MasVnrType)

train2$MasVnrType <- ifelse(is.na(train2$MasVnrType) & train2$MasVnrArea != 0, sample(values,3), 
                            train2$MasVnrType)

train2$MasVnrType[is.na(train2$MasVnrType)] <- "None"

train2$MasVnrType <- as.factor(train2$MasVnrType)

table(train2$MasVnrType)
sum(is.na(train2$MasVnrType))

#FireplaceQu: Fireplace quality
table(train$FireplaceQu)
sum(is.na(train$FireplaceQu))
train2$FireplaceQu[is.na(train$FireplaceQu)] <- 0 

train2$FireplaceQu[train$FireplaceQu == "Ex"] <- 5
train2$FireplaceQu[train$FireplaceQu == "Gd"] <- 4
train2$FireplaceQu[train$FireplaceQu == "TA"] <- 3
train2$FireplaceQu[train$FireplaceQu == "Fa"] <- 2
train2$FireplaceQu[train$FireplaceQu == "Po"] <- 1

train2$FireplaceQu <- as.integer(train2$FireplaceQu)

table(train2$FireplaceQu)

#Number of fireplaces
table(train$Fireplaces)
sum(is.na(train$Fireplaces))

train2$Fireplaces <- as.integer(train2$Fireplaces)

table(train2$Fireplaces)

#Exterior
#Exterior variables 1 and 2 layers
table(train$Exterior1st)
table(train$Exterior2nd)
sum(is.na(train$Exterior1st))
sum(is.na(train$Exterior2nd))

#We assign the missing value to the most common value
train2$Exterior1st[is.na(train$Exterior1st)] <- names(sort(-table(train$Exterior1st)))[1]
train2$Exterior2nd[is.na(train$Exterior2nd)] <- names(sort(-table(train$Exterior2nd)))[1]

train2$ExterCombine <- ifelse(train2$Exterior1st == train2$Exterior2nd, train2$Exterior1st)

train2 %>%
  mutate(ExteriorComn = case_when(
    Exterior1st == Exterior2nd ~ Exterior2nd,
    Exterior1st != Exterior2nd ~ paste(Exterior1st,Exterior2nd, collapse = "-")
  ))


train2$Exterior1st <- as.factor(train2$Exterior1st)
train2$Exterior2nd <- as.factor(train2$Exterior2nd)

sum(is.na(train2$Exterior1st))
sum(is.na(train2$Exterior2nd))
#Exterior quality
table(train$ExterQual)
sum(is.na(train$ExterQual))
train2$ExterQual[is.na(train$ExterQual)] <- 0

train2$ExterQual <- as.integer(plyr::revalue(train2$ExterQual,
                                             c(Po=1,Fa=2,TA=3,
                                               Gd=4,Ex=5)))

table(train2$ExterQual)

#Exterior Condition
table(train$ExterCond)
sum(is.na(train$ExterCondl))
train2$ExterCond[is.na(train$ExterCond)] <- 0

train2$ExterCond <- as.integer(plyr::revalue(train2$ExterCond,
                                             c(Po=1,Fa=2,TA=3,
                                               Gd=4,Ex=5)))

table(train2$ExterCond)

#Garages 
#Garages 81 missing to none garage
#Garage year build
table(train$GarageYrBlt)
sum(is.na(train$GarageYrBlt))
train2$GarageYrBlt[is.na(train$GarageYrBlt)] <- 0 

train2$GarageYrBlt <- as.integer(train2$GarageYrBlt)

table(train2$GarageYrBlt)

#Garage Type
table(train$GarageType)
sum(is.na(train$GarageType))
train2$GarageType[is.na(train$GarageType)] <- "Na"

train2$GarageType <- as.factor(train2$GarageType)

table(train2$GarageType)

#Garage Finish
table(train$GarageFinish)
sum(is.na(train$GarageFinish))
train2$GarageFinish[is.na(train$GarageFinish)] <- 0

train2$GarageFinish <- as.integer(plyr::revalue(train2$GarageFinish, 
                                          c(Unf=1,RFn=2,Fin=3)))

table(train2$GarageFinish)

#Garage Quality
table(train$GarageQual)
sum(is.na(train$GarageQual))
train2$GarageQual[is.na(train$GarageQual)] <- 0

train2$GarageQual <- as.integer(plyr::revalue(train2$GarageQual,
                                              c(Po=1,Fa=2,TA=3,
                                                Gd=4,Ex=5)))

table(train2$GarageQual)

#Garage condition
table(train$GarageCond)
sum(is.na(train$GarageCond))
train2$GarageCond[is.na(train$GarageCond)] <- 0

train2$GarageCond <- as.integer(plyr::revalue(train2$GarageCond,
                                              c(Po=1,Fa=2,TA=3,
                                                Gd=4,Ex=5)))

table(train2$GarageCond)

#Garage cars
table(train$GarageCars)
sum(is.na(train$GarageCars))
train2$GarageCars[is.na(train2$GarageCars)] <- 0 #Assuming that the same garage have area

train2$GarageCars <- as.integer(train2$GarageCars)

#Garage Area
table(train$GarageArea)
sum(is.na(train$GarageArea))
train2$GarageArea[is.na(train2$GarageArea)] <- 0

train2$GarageArea <- as.integer(train2$GarageArea)

#House with pool 
#Pool quality 1453 move to Na
table(train$PoolQC)
sum(is.na(train$PoolQC))
train2$PoolQC <- ifelse(is.na(train$PoolQC) & train$PoolArea != 0, 1,
                        ifelse(is.na(train$PoolQC), 0,1))


train2$PoolQC <- as.integer(train2$PoolQC)

table(train2$PoolQC)

#Pool Area
table(train$PoolArea)
sum(is.na(train$PoolArea))
train2$PoolArea[is.na(train$PoolArea)] <- 0

train2$PoolArea <- as.integer(train$PoolArea)

table(train2$PoolArea)

#Fence quality
table(train$Fence)
sum(is.na(train$Fence))
train2$Fence[is.na(train$Fence)] <- "Na"

train2$Fence <- as.factor(train2$Fence)

table(train2$Fence)

ggplot(train2[!is.na(train2$SalePrice),], aes(x=Fence, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 1000000, by=15000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

#Miscellaneous feauture not covered in other categories
table(train$MiscFeature)

#train2$MiscFeature[is.na(train$MiscFeature)] <- "Na"
train2$MiscFeature <- ifelse(is.na(train$MiscFeature) & train$MiscVal != 0, "Gar2",
                             ifelse(is.na(train$MiscFeature), "Na",
                                    train$MiscFeature))

train2$MiscFeature <- ifelse(train2$MiscFeature == "Na" & train2$PoolQC == 1, "Pool",
                             train2$MiscFeature)

train2$MiscFeature <- as.factor(train2$MiscFeature)

table(train2$MiscFeature)

#Miscellaneous value
table(train$MiscVal)
sum(is.na(train$MiscVal))

train2$MiscVal <- as.integer(train2$MiscVal)

table(train2$MiscVal)

#Zoning class
table(train$MSZoning)
sum(is.na(train$MSZoning))
train2$MSZoning[is.na(train$MSZoning)] <- "Na"

train2$MSZoning <- as.factor(train2$MSZoning)

table(train2$MSZoning)

ggplot(train2[!is.na(train2$SalePrice),], aes(x=MSZoning, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 1000000, by=15000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))
#Zoning subclass
table(train$MSSubClass)
sum(is.na(train$MSSubClass))
train2$MSSubClass <- as.factor(train2$MSSubClass)

train2$MSSubClass <- plyr::revalue(train2$MSSubClass, c('20'='1 story 1946+',
                                                                '30'='1 story 1945-',
                                                                '40'='1 story unf attic', 
                                                                '45'='1,5 story unf', 
                                                                '50'='1,5 story fin', 
                                                                '60'='2 story 1946+', 
                                                                '70'='2 story 1945-', 
                                                                '75'='2,5 story all ages', 
                                                                '80'='split/multi level', 
                                                                '85'='split foyer',
                                                                '90'='duplex all style/age', 
                                                                '120'='1 story PUD 1946+',
                                                                '150'='1,5 story PUD all', 
                                                                '160'='2 story PUD 1946+',
                                                                '180'='PUD multilevel', 
                                                                '190'='2 family conversion'))


table(train2$MSSubClass)

ggplot(train2[!is.na(train2$SalePrice),], aes(x=reorder(MSSubClass,SalePrice), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

#Kitchen
table(train$KitchenQual)
sum(is.na(train$KitchenQual))
train2$KitchenQual[is.na(train$KitchenQual)] <- 0

train2$KitchenQual <- as.integer(plyr::revalue(train2$KitchenQual,
                                              c(Po=1,Fa=2,TA=3,
                                                Gd=4,Ex=5)))
table(train2$KitchenQual)

#Kitchen ave
table(train$KitchenAbvGr)
sum(is.na(train$KitchenAbvGr))

train2$KitchenAbvGr[train$KitchenAbvGr == 0] <- 1

train2$KitchenAbvGr <- as.integer(train2$KitchenAbvGr)

table(train2$KitchenAbvGr)
#Bedroom and total rooms
table(train$BedroomAbvGr)
sum(is.na(train$BedroomAbvGr))

train2$BedroomAbvGr <- as.integer(train2$BedroomAbvGr)
train2$TotRmsAbvGrd <- as.integer(train2$TotRmsAbvGrd)

table(train$TotRmsAbvGrd)

#Porch Binary
table(train$EnclosedPorch)
sum(is.na(train$EnclosedPorch))

train2$EnclosedPorch[train$EnclosedPorch != 0] <- 1
train2$OpenPorchSF[train$OpenPorchSF != 0] <- 1
train2$X3SsnPorch[train$X3SsnPorch != 0] <- 1
train2$ScreenPorch[train$ScreenPorch != 0] <- 1
train2$WoodDeckSF[train$WoodDeckSF != 0] <- 1

train2$EnclosedPorch <- as.integer(train2$EnclosedPorch)
train2$OpenPorchSF <- as.integer(train2$OpenPorchSF)
train2$X3SsnPorch <- as.integer(train2$X3SsnPorch)
train2$ScreenPorch <- as.integer(train2$ScreenPorch)
train2$WoodDeckSF <- as.integer(train2$WoodDeckSF)

table(train2$EnclosedPorch)
table(train2$OpenPorchSF)
table(train2$X3SsnPorch)
table(train2$ScreenPorch)
table(train2$WoodDeckSF)
#Home functionality can be ordinal :)
table(train$Functional)
sum(is.na(train$Functional))
train2$Functional[is.na(train2$Functional)] <- "Typ"

train2$Functional <- as.integer(plyr::revalue(train2$Functional, c(Sal=0,Sev=1,
                                                                   Maj2=2,Maj1=3,
                                                                   Mod=4,Min2=5,
                                                                   Min1=6,Typ=7)))
table(train2$Functional)

####
#Sale type
table(train$SaleType)
sum(is.na(train$SaleType))
train2$SaleType[is.na(train2$SaleType)] <- "WD"

train2$SaleType <- as.factor(train2$SaleType)
table(train2$SaleType)
#Sale Condition
table(train$SaleCondition)
sum(is.na(train$SaleCondition))

train2$SaleCondition <- as.factor(train2$SaleCondition)

#Sale price
table(train$SalePrice)
sum(is.na(train$SalePrice))

train2$SalePrice <- as.integer(train2$SalePrice)

#Foundation of the house
table(train$Foundation)
sum(is.na(train$Foundation))

train2$Foundation <- as.factor(train2$Foundation)

#Roof
#Type of roof
table(train$RoofStyle)
sum(is.na(train$RoofStyle))

train2$RoofStyle <- as.factor(train2$RoofStyle)

ggplot(train2[!is.na(train2$SalePrice),], aes(x=RoofStyle, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 1000000, by=15000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

#roof materials
table(train$RoofMatl)
sum(is.na(train$RoofMatl))

train2$RoofMatl <- as.factor(train2$RoofMatl)

ggplot(train2[!is.na(train2$SalePrice),], aes(x=RoofMatl, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 1000000, by=15000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

#Dwelling
#Type of dwelling 
table(train$BldgType)
sum(is.na(train$BldgType))

train2$BldgType <- as.factor(train2$BldgType)

ggplot(train2[!is.na(train2$SalePrice),], aes(x=BldgType, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 1000000, by=15000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

#House style
table(train$HouseStyle)
sum(is.na(train$HouseStyle))

train2$HouseStyle <- as.factor(plyr::revalue(train2$HouseStyle, 
                                             c("1Story"= "1story or 1 1/2",
                                               "1.5Fin"="1story or 1 1/2",
                                               "1.5Unf"="1story or 1 1/2",
                                               "2Story"="2story or 2 1/2",
                                               "2.5Fin"="2story or 2 1/2",
                                               "2.5Unf"="2story or 2 1/2",
                                               "SFoyer"="Shared areas",
                                               "SLvl"="Shared areas")))

ggplot(train2[!is.na(train2$SalePrice),], aes(x=HouseStyle, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='green') +
  scale_y_continuous(breaks= seq(0, 1000000, by=15000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))
                                              
table(train2$HouseStyle)
sum(is.na(train2$HouseStyle))


#The hood
table(train$Neighborhood)
sum(is.na(train$Neighborhood))

train2$Neighborhood <- as.factor(train2$Neighborhood)

ggplot(train2[!is.na(train2$SalePrice),], aes(x=Neighborhood, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 1000000, by=15000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

#Proximity to nice stuff 1 or more
table(train$Condition1)
table(train$Condition2)
sum(is.na(train$Condition1))
sum(is.na(train$Condition2))

train2$Condition1 <- as.factor(train2$Condition1)
train2$Condition2 <- as.factor(train2$Condition2)

ggplot(train2[!is.na(train2$SalePrice),], aes(x=Condition1, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 1000000, by=15000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

#Months as factor as the month should not affect the sell price like the years
train2$MoSold <- as.factor(train2$MoSold)

#######################################################################
#Feature engineering
#Creating a total of bathrooms
train2$TotBathrooms <- train2$FullBath + (train2$HalfBath*0.5) + train2$BsmtFullBath + (train2$BsmtHalfBath*0.5)

table(train2$TotBathrooms)

ggplot(train2[!is.na(train2$SalePrice),], aes(x=reorder(TotBathrooms,SalePrice), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 1000000, by=15000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

cor(train2$SalePrice[!is.na(train2$SalePrice)], train2$TotBathrooms[!is.na(train2$SalePrice)])

sum(is.na(train2$TotBathrooms))

#House Age / Remodeled/ House is new
train2$Remod <- ifelse(train2$YearBuilt==train2$YearRemodAdd, 0, 1) #0=No Remodeling, 1=Remodeling
train2$Age <- as.numeric(train2$YrSold)-train2$YearRemodAdd
train2$IsNew <- ifelse(train2$YrSold==train2$YearBuilt, 1, 0)
table(train2$IsNew)

table(train2$Age)
cor(train2$SalePrice[!is.na(train2$SalePrice)], train2$Age[!is.na(train2$SalePrice)])

train2$YrSold <- as.factor(train2$YrSold)

#Total Porch
train2$TotalPorchSF <- train2$WoodDeckSF + train2$OpenPorchSF + train2$EnclosedPorch + train2$X3SsnPorch + train2$ScreenPorch
table(train2$TotalPorchSF)

ggplot(train3[!is.na(train3$SalePrice),], aes(x=TotalPorchSF, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 1000000, by=15000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

train2$TotalPorchSF <- as.integer(train2$TotalPorchSF)

cor(train2$SalePrice, train2$TotalPorchSF, use= "pairwise.complete.obs")
#Total Rooms including bathroom and basement bathrooms
train2$TotalRooms <- train2$TotRmsAbvGrd - train2$FullBath - 
  (train2$HalfBath*0.5) + train2$BsmtFullBath + (train2$BsmtHalfBath*0.5) - 
  train2$KitchenAbvGr

ggplot(train3[!is.na(train3$SalePrice),], aes(x=TotalRooms, y=Age)) +
  geom_bar(stat='summary', fun.y = "median", fill='red') +
  scale_y_continuous(breaks= seq(0, 1000000, by=15000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

cor(train2$SalePrice, train2$TotalRooms, use= "pairwise.complete.obs")

#Total quality of house
train2$TotalQualHouse <- train2$OverallQual + (train2$OverallCond*0.5)


ggplot(train3[!is.na(train3$SalePrice),], aes(x=TotalQualHouse, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='red') +
  scale_y_continuous(breaks= seq(0, 1000000, by=15000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

cor(train2$SalePrice, train2$TotalQualHouse, use= "pairwise.complete.obs")

#######
#Conditions of the house
train2$ConditionAll <- paste(train2$Condition1,train2$Condition2)

train2$ConditionAll <- as.factor(train2$ConditionAll)

ggplot(train3[!is.na(train3$SalePrice),], aes(x=reorder(ConditionAll,SalePrice), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='red') +
  scale_y_continuous(breaks= seq(0, 1000000, by=15000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

train2$Condition1 <- NULL
train2$Condition2 <- NULL

#Exterior of the house combine
train2$ExteriorAll <- paste(train2$Exterior1st,train2$Exterior2nd)

train2$ExteriorAll <- as.factor(train2$ExteriorAll)

ggplot(train2[!is.na(train2$SalePrice),], aes(x=reorder(ExteriorAll,SalePrice), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='green') +
  scale_y_continuous(breaks= seq(0, 1000000, by=15000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

train2$Exterior1st <- NULL
train2$Exterior2nd <- NULL

#Roof Style and Material combine
train2$RoofAll <- paste(train2$RoofStyle, train2$RoofMatl)
train2$RoofAll <- as.factor(train2$RoofAll)

ggplot(train2[!is.na(train2$SalePrice),], aes(x=reorder(RoofAll,SalePrice), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='green') +
  scale_y_continuous(breaks= seq(0, 1000000, by=15000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

train2$RoofMatl <- NULL
train2$RoofStyle <- NULL
#Neighborhood bin
train2$NeighRich[train2$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 2
train2$NeighRich[!train2$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale', 'StoneBr', 'NridgHt', 'NoRidge')] <- 1
train2$NeighRich[train2$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale')] <- 0

#Max Quality
train2$MaxQuality <- train2$ExterQual + (train2$ExterCond*0.5) +
  train2$BsmtQual + (train2$BsmtCond*0.5) + train2$HeatingQC +
  train2$KitchenQual + train2$FireplaceQu + train2$PoolQC + 
  train2$GarageQual + (train2$GarageCond*0.5) + train2$OverallQual + (train2$OverallCond*0.5)

ggplot(train2[!is.na(train2$SalePrice),], aes(x=MaxQuality, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='red') +
  scale_y_continuous(breaks= seq(0, 1000000, by=15000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

cor(train2$SalePrice, train2$MaxQuality, use= "pairwise.complete.obs")

library(ggrepel)

train2$FinalSqft <- train2$GarageArea + train2$GrLivArea + train2$TotalBsmtSF

ggplot(data=train2[!is.na(train2$SalePrice),], aes(x=FinalSqft, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_text_repel(aes(label = ifelse(train2$TotalSqFeet[!is.na(train2$SalePrice)]>4500, rownames(train2), '')))

cor(train2$SalePrice, train2$FinalSqft, use= "pairwise.complete.obs")

table(train2$NeighRich)
#Check for Na values
cbind(
  lapply(
    lapply(train2, is.na)
    , sum)
)

#Consider to remove outliers
cor(train2$SalePrice[-c(524, 1299)], train2$TotalSqFeet[-c(524, 1299)], use= "pairwise.complete.obs")

train2 <- train2[-c(524,1299), ]

train3 <- train2
#index vector
numericVars <- which(sapply(train2, is.numeric)) #index vector numeric variables
factorVars <- which(sapply(train2, is.factor)) #index vector factor variables
cat('There are', length(numericVars), 'numeric variables, and', length(factorVars), 'categoric variables')

#Drop highly correlated variables
#dropVars <- c("GrLivArea",'GarageArea','TotalRmsAbvGrd',"TotalBsmtSF")
#train2 <- train2[,!(names(train2) %in% dropVars)]

numericVarNames <- names(numericVars)
numericVarNames <- numericVarNames[!(numericVarNames %in% c('MSSubClass', 'MoSold', 'YrSold', 'SalePrice', 'OverallQual', 'OverallCond'))] #numericVarNames was created before having done anything
numericVarNames <- append(numericVarNames, c("TotalRooms","Age", "TotBathrooms",'TotalPorchSF'
                                             ,"FinalSqft","TotalQualHouse","MaxQuality"))

#########
DFnumeric <- train2[, names(train2) %in% numericVarNames]

DFfactors <- train2[, !(names(train2) %in% numericVarNames)]
DFfactors <- DFfactors[, names(DFfactors) != 'SalePrice']

cat('There are', length(DFnumeric), 'numeric variables, and', length(DFfactors), 'factor variables')

for(i in 1:ncol(DFnumeric)){
  if (abs(skew(DFnumeric[,i]))>0.8){
    DFnumeric[,i] <- log(DFnumeric[,i] +1)
  }
}
library(caret)
PreNum <- preProcess(DFnumeric, method=c("center", "scale"))
print(PreNum)

DFnorm <- predict(PreNum, DFnumeric)
dim(DFnorm)

DFdummies <- as.data.frame(model.matrix(~.-1, DFfactors))
dim(DFdummies)

ZerocolTest <- which(colSums(DFdummies[(nrow(train2[!is.na(train2$SalePrice),])+1):nrow(train2),])==0)
colnames(DFdummies[ZerocolTest])

DFdummies <- DFdummies[,-ZerocolTest] #removing predictor

ZerocolTrain <- which(colSums(DFdummies[1:nrow(train2[!is.na(train2$SalePrice),]),])==0)
colnames(DFdummies[ZerocolTrain])

DFdummies <- DFdummies[,-ZerocolTrain] #removing predictor

fewOnes <- which(colSums(DFdummies[1:nrow(train2[!is.na(train2$SalePrice),]),])<10)
colnames(DFdummies[fewOnes])

DFdummies <- DFdummies[,-fewOnes] #removing predictors
dim(DFdummies)

combined <- cbind(DFnorm, DFdummies) #combining all (now numeric) predictors into one dataframe

skew(train2$SalePrice)


qqnorm(train2$SalePrice)
qqline(train2$SalePrice)

train2$SalePrice <- log(train2$SalePrice) #default is the natural logarithm, "+1" is not necessary as there are no 0's
skew(train2$SalePrice)

qqnorm(train2$SalePrice)
qqline(train2$SalePrice)
########################################################

train12 <- combined[!is.na(train2$SalePrice),]
test12 <- combined[is.na(train2$SalePrice),]

set.seed(27042018)
my_control <-trainControl(method="cv", number=5)
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))

lasso_mod <- train(x=train12, y=train2$SalePrice[!is.na(train2$SalePrice)], 
                   method='glmnet', 
                   trControl= my_control, tuneGrid=lassoGrid) 
lasso_mod$bestTune

min(lasso_mod$results$RMSE)
##########################################
lassoVarImp <- varImp(lasso_mod,scale=F)
lassoImportance <- lassoVarImp$importance

varsSelected <- length(which(lassoImportance$Overall!=0))
varsNotSelected <- length(which(lassoImportance$Overall==0))

cat('Lasso uses', varsSelected, 'variables in its model, and did not select', varsNotSelected, 'variables.')

LassoPred <- predict(lasso_mod, test12)
predictions_lasso <- exp(LassoPred) #need to reverse the log to the real values
head(predictions_lasso)

prediction <- data.frame(Id = test_labels, SalePrice = predictions_lasso)
head(prediction)
#write.csv(train2, "DataExploration.csv", row.names = FALSE)
#write.csv(prediction,"prediction.csv", row.names = FALSE)
#########################

#Bagging Essemble

xgb_grid = expand.grid(
  nrounds = 1000,
  eta = c(0.1, 0.05, 0.01),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree=1,
  min_child_weight=c(1, 2, 3, 4 ,5),
  subsample=1
)


label_train <- train2$SalePrice[!is.na(train2$SalePrice)]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = as.matrix(train12), label= label_train)
dtest <- xgb.DMatrix(data = as.matrix(test12))

default_param<-list(
  objective = "reg:linear",
  booster = "gbtree",
  eta=0.05, #default = 0.3
  gamma=0,
  max_depth=3, #default=6
  min_child_weight=4, #default=1
  subsample=1,
  colsample_bytree=1
)
set.seed(27042018)
xgbcv <- xgb.cv( params = default_param, data = dtrain, nrounds = 700, 
                 nfold = 5, showsd = T, stratified = T, print_every_n = 40, 
                 early_stopping_rounds = 10, maximize = F)

#train the model using the best iteration found by cross validation
xgb_mod <- xgb.train(data = dtrain, params=default_param, nrounds = xgbcv$best_iteration)

XGBpred <- predict(xgb_mod, dtest)
predictions_XGB <- exp(XGBpred) #need to reverse the log to the real values
head(predictions_XGB)

#view variable importance plot
library(Ckmeans.1d.dp) #required for ggplot clustering
mat <- xgb.importance (feature_names = colnames(train12),model = xgb_mod)
xgb.ggplot.importance(importance_matrix = mat[1:20], rel_to_first = TRUE)


sub_avg <- data.frame(Id = test_labels, SalePrice = (predictions_XGB+2*predictions_lasso)/3)
head(sub_avg)

#################################################
train13 <- combined[!is.na(train2$SalePrice),]
test13 <- combined[is.na(train2$SalePrice),]
set.seed(999)
# Model Building : Elastic Net Regression
control <- caret::trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 5,
                        search = "random",
                        verboseIter = TRUE)
# Training ELastic Net Regression model
model_net <- caret::train(x=train12, y=train2$SalePrice[!is.na(train2$SalePrice)],
                          label= label_train,
                          method = "glmnet", tuneLength = 10, 
                          trControl = control)

model_net
model_net$bestTune 

#predict
SalePrice_Net <- predict(model_net, test12)
#unscale prediction
#SalePrice_Net2 <- predict(model_net, test12) * sd() + mean(SalePrice_out)
SalePrice_Net_2 <- exp(SalePrice_Net) #need to reverse the log to the real values
head(SalePrice_Net_2)

sub_avg_2 <- data.frame(Id = test_labels, SalePrice = (2*SalePrice_Net + predictions_XGB + 2*predictions_lasso)/3)
head(sub_avg_2)
