rm(list=ls())
train <- read.csv("E:/R/housing price/train.csv",stringsAsFactors=FALSE)
test <- read.csv("E:/R/housing price/test (1).csv",stringsAsFactors=FALSE)
colSums(is.na(train))
colSums(is.na(test))

#removing the columns that contains inappropiate number of NA Values
train$LotFrontage <- NULL #17 %
train$Alley <- NULL # 93 %
train$FireplaceQu <- NULL  #47%
train$PoolQC <- NULL #almost 100%
train$Fence <- NULL
train$MiscFeature <- NULL

test$LotFrontage <- NULL
test$Alley <- NULL
test$FireplaceQu <- NULL
test$PoolQC <- NULL
test$Fence <- NULL
test$MiscFeature <- NULL

summary(train)

#now treat Time for Univariate Analysis

#Id <- as this column never help us to detect the price of houses so we remove this column.
train$Id <- NULL
test$Id <- NULL

#  taking  MSSubClass
bx <- boxplot(train$MSSubClass)
bx$stats
#this contains 3 outliers so we have to remove it.
quantile(train$MSSubClass, seq(0,1,0.02))
train$MSSubClass <- ifelse(train$MSSubClass > 120,120,train$MSSubClass)
boxplot(train$MSSubClass)


#  now take MSZoning , as this is a ctegorical variable so we have to convert dummy variables of it to perform our analysis.
summary(train$MSZoning)
summary(test$MSZoning)
test$MSZoning[(is.na(test$MSZoning))] <- "RL" #used mode value for NA treatment.

library(fastDummies)

dum_MS <- dummy_cols(train$MSZoning,remove_most_frequent_dummy = T)
dum_MST <- dummy_cols(test$MSZoning, remove_most_frequent_dummy = T)
dum_MS$.data <- NULL
dum_MST$.data <- NULL
train <- data.frame(train,dum_MS)
test <- data.frame(test,dum_MST)
train$MSZoning <- NULL
test$MSZoning <- NULL


summary(train$Street)
train$Street <- ifelse(train$Street == "Grvl",1,0)
test$Street <- ifelse(test$Street == "Grvl",1,0)
train$Street <- NULL
test$Street <- NULL


summary(train$LotShape)
lot_shape <- dummy_cols(train$LotShape,remove_most_frequent_dummy = T)
lot_shape$.data <- NULL
train <- data.frame(train,lot_shape)
lot_shape <- dummy_cols(test$LotShape,remove_most_frequent_dummy = T)
lot_shape$.data <- NULL
test <- data.frame(test,lot_shape)
train$LotShape <- NULL
test$LotShape <- NULL

summary(train$LandContour)
dumm <- dummy_cols(train$LandContour,remove_most_frequent_dummy = T)
dumm$.data <- NULL
train <- data.frame(train,dumm)
dumm <- dummy_cols(test$LandContour,remove_most_frequent_dummy = T)
dumm$.data <- NULL
test <- data.frame(test,dumm)
train$LandContour <- NULL
test$LandContour <- NULL



summary(train$Utilities)
train$Utilities <- ifelse(train$Utilities == "NoSeWa",0,1)
test$Utilities <- ifelse(test$Utilities == "NoSeWa",0,1)
train$Utilities <- NULL
test$Utilities <- NULL



summary(train$LotConfig)
dumm <- dummy_cols(train$LotConfig,remove_most_frequent_dummy = T)
dumm$.data <- NULL
train <- data.frame(train,dumm)
dumm <- dummy_cols(test$LotConfig,remove_most_frequent_dummy = T)
dumm$.data <- NULL
test <- data.frame(test,dumm)
train$LotConfig <- NULL
test$LotConfig <- NULL


summary(train$LandSlope)
dumm <- dummy_cols(train$LandSlope,remove_most_frequent_dummy = T)
dumm$.data <- NULL
train <- data.frame(train,dumm)
dumm <- dummy_cols(test$LandSlope,remove_most_frequent_dummy = T)
dumm$.data <- NULL
test <- data.frame(test,dumm)
train$LandSlope <- NULL
test$LandSlope <- NULL
summary(train)

plot(train$LotArea,train$SalePrice)
scatterplot(train$Neighborhood,train$SalePrice)


dumm <- dummy_cols(train$Neighborhood,remove_most_frequent_dummy = T)
dumm$.data <- NULL
train <- data.frame(train,dumm)
dumm <- dummy_cols(test$Neighborhood,remove_most_frequent_dummy = T)
dumm$.data <- NULL
test <- data.frame(test,dumm)
train$Neighborhood <- NULL
test$Neighborhood <- NULL

summary(train)

library(dplyr)
#nbhdprice <- summarize(group_by(train, Neighborhood),
                       #mean(SalePrice, na.rm=T))

#nbhdprice_low <- filter(nbhdprice, nbhdprice$`mean(SalePrice, na.rm = T)` < 140000)

#nbhdprice_med <- filter(nbhdprice, nbhdprice$`mean(SalePrice, na.rm = T)` < 200000 &
                          #nbhdprice$`mean(SalePrice, na.rm = T)` >= 140000 )

#nbhdprice_hi <- filter(nbhdprice, nbhdprice$`mean(SalePrice, na.rm = T)` >= 200000)

#train$nbhd_price_level[train$Neighborhood %in% nbhdprice_low$Neighborhood] <- 1
#train$nbhd_price_level[train$Neighborhood %in% nbhdprice_med$Neighborhood] <- 2
#train$nbhd_price_level[train$Neighborhood %in% nbhdprice_hi$Neighborhood] <- 3

#View(train$nbhd_price_level)

roofstyle_price <- summarize(group_by(train, RoofStyle),
                             mean(SalePrice, na.rm=T))
View(roofstyle_price)


train$roof_hip_shed[train$RoofStyle %in% c("Hip", "Shed")] <- 1
train$roof_hip_shed[!train$RoofStyle %in% c("Hip", "Shed")] <- 0

roofmatl_price <- summarize(group_by(train, RoofMatl),
                            mean(SalePrice, na.rm=T))

train$roof_matl_hi[train$RoofMatl %in% c("Membran", "WdShake", "WdShngl")] <- 1
train$roof_matl_hi[!train$RoofMatl %in% c("Membran", "WdShake", "WdShngl")] <- 0


price <- summarize(group_by(train, Exterior1st),
                   mean(SalePrice, na.rm=T))

matl_lo_1 <- filter(price, price$`mean(SalePrice, na.rm = T)` < 140000)
matl_med_1<- filter(price, price$`mean(SalePrice, na.rm = T)` < 200000 &
                      price$`mean(SalePrice, na.rm = T)` >= 140000 )
matl_hi_1 <- filter(price, price$`mean(SalePrice, na.rm = T)` >= 200000)

train$exterior_1[train$Exterior1st %in% matl_lo_1$Exterior1st] <- 1
train$exterior_1[train$Exterior1st %in% matl_med_1$Exterior1st] <- 2
train$exterior_1[train$Exterior1st %in% matl_hi_1$Exterior1st] <- 3


price <- summarize(group_by(train, Exterior2nd),
                   mean(SalePrice, na.rm=T))

matl_lo <- filter(price, price$`mean(SalePrice, na.rm = T)` < 140000)
matl_med <- filter(price, price$`mean(SalePrice, na.rm = T)` < 200000 &
                     price$`mean(SalePrice, na.rm = T)` >= 140000 )
matl_hi <- filter(price, price$`mean(SalePrice, na.rm = T)` >= 200000)

train$exterior_2[train$Exterior2nd %in% matl_lo$Exterior2nd] <- 1
train$exterior_2[train$Exterior2nd %in% matl_med$Exterior2nd] <- 2
train$exterior_2[train$Exterior2nd %in% matl_hi$Exterior2nd] <- 3

price <- summarize(group_by(train, MasVnrType),
                   mean(SalePrice, na.rm=T))

train$exterior_mason_1[train$MasVnrType %in% c("Stone", "BrkFace") | is.na(train$MasVnrType)] <- 1
train$exterior_mason_1[!train$MasVnrType %in% c("Stone", "BrkFace") & !is.na(train$MasVnrType)] <- 0


price <- summarize(group_by(train, ExterQual),
                   mean(SalePrice, na.rm=T))

train$exterior_cond[train$ExterQual == "Ex"] <- 4
train$exterior_cond[train$ExterQual == "Gd"] <- 3
train$exterior_cond[train$ExterQual == "TA"] <- 2
train$exterior_cond[train$ExterQual == "Fa"] <- 1


price <- summarize(group_by(train, ExterCond),
                   mean(SalePrice, na.rm=T))

train$exterior_cond2[train$ExterCond == "Ex"] <- 5
train$exterior_cond2[train$ExterCond == "Gd"] <- 4
train$exterior_cond2[train$ExterCond == "TA"] <- 3
train$exterior_cond2[train$ExterCond == "Fa"] <- 2
train$exterior_cond2[train$ExterCond == "Po"] <- 1


price <- summarize(group_by(train, Foundation),
                   mean(SalePrice, na.rm=T))

train$found_concrete[train$Foundation == "PConc"] <- 1
train$found_concrete[train$Foundation != "PConc"] <- 0


price <- summarize(group_by(train, BsmtQual),
                   mean(SalePrice, na.rm=T))

train$bsmt_cond1[train$BsmtQual == "Ex"] <- 5
train$bsmt_cond1[train$BsmtQual == "Gd"] <- 4
train$bsmt_cond1[train$BsmtQual == "TA"] <- 3
train$bsmt_cond1[train$BsmtQual == "Fa"] <- 2
train$bsmt_cond1[is.na(train$BsmtQual)] <- 1


price <- summarize(group_by(train, BsmtCond),
                   mean(SalePrice, na.rm=T))

train$bsmt_cond2[train$BsmtCond == "Gd"] <- 5
train$bsmt_cond2[train$BsmtCond == "TA"] <- 4
train$bsmt_cond2[train$BsmtCond == "Fa"] <- 3
train$bsmt_cond2[is.na(train$BsmtCond)] <- 2
train$bsmt_cond2[train$BsmtCond == "Po"] <- 1


price <- summarize(group_by(train, BsmtExposure),
                   mean(SalePrice, na.rm=T))

train$bsmt_exp[train$BsmtExposure == "Gd"] <- 5
train$bsmt_exp[train$BsmtExposure == "Av"] <- 4
train$bsmt_exp[train$BsmtExposure == "Mn"] <- 3
train$bsmt_exp[train$BsmtExposure == "No"] <- 2
train$bsmt_exp[is.na(train$BsmtExposure)] <- 1


price <- summarize(group_by(train, BsmtFinType1),
                   mean(SalePrice, na.rm=T))

train$bsmt_fin1[train$BsmtFinType1 == "GLQ"] <- 5
train$bsmt_fin1[train$BsmtFinType1 == "Unf"] <- 4
train$bsmt_fin1[train$BsmtFinType1 == "ALQ"] <- 3
train$bsmt_fin1[train$BsmtFinType1 %in% c("BLQ", "Rec", "LwQ")] <- 2
train$bsmt_fin1[is.na(train$BsmtFinType1)] <- 1



price <- summarize(group_by(train, BsmtFinType2),
                   mean(SalePrice, na.rm=T))

train$bsmt_fin2[train$BsmtFinType2 == "ALQ"] <- 6
train$bsmt_fin2[train$BsmtFinType2 == "Unf"] <- 5
train$bsmt_fin2[train$BsmtFinType2 == "GLQ"] <- 4
train$bsmt_fin2[train$BsmtFinType2 %in% c("Rec", "LwQ")] <- 3
train$bsmt_fin2[train$BsmtFinType2 == "BLQ"] <- 2
train$bsmt_fin2[is.na(train$BsmtFinType2)] <- 1

price <- summarize(group_by(train, Heating),
                   mean(SalePrice, na.rm=T))


train$gasheat[train$Heating %in% c("GasA", "GasW")] <- 1
train$gasheat[!train$Heating %in% c("GasA", "GasW")] <- 0


price <- summarize(group_by(train, HeatingQC),
                   mean(SalePrice, na.rm=T))

train$heatqual[train$HeatingQC == "Ex"] <- 5
train$heatqual[train$HeatingQC == "Gd"] <- 4
train$heatqual[train$HeatingQC == "TA"] <- 3
train$heatqual[train$HeatingQC == "Fa"] <- 2
train$heatqual[train$HeatingQC == "Po"] <- 1


price <- summarize(group_by(train, CentralAir),
                   mean(SalePrice, na.rm=T))

train$air[train$CentralAir == "Y"] <- 1
train$air[train$CentralAir == "N"] <- 0


price <- summarize(group_by(train, Electrical),
                   mean(SalePrice, na.rm=T))

train$standard_electric[train$Electrical == "SBrkr" | is.na(train$Electrical)] <- 1
train$standard_electric[!train$Electrical == "SBrkr" & !is.na(train$Electrical)] <- 0


price <- summarize(group_by(train, KitchenQual),
                   mean(SalePrice, na.rm=T))

train$kitchen[train$KitchenQual == "Ex"] <- 4
train$kitchen[train$KitchenQual == "Gd"] <- 3
train$kitchen[train$KitchenQual == "TA"] <- 2
train$kitchen[train$KitchenQual == "Fa"] <- 1





price <- summarize(group_by(train, GarageType),
                   mean(SalePrice, na.rm=T))

train$gar_attach[train$GarageType %in% c("Attchd", "BuiltIn")] <- 1
train$gar_attach[!train$GarageType %in% c("Attchd", "BuiltIn")] <- 0


price <- summarize(group_by(train, GarageFinish),
                   mean(SalePrice, na.rm=T))

train$gar_finish[train$GarageFinish %in% c("Fin", "RFn")] <- 1
train$gar_finish[!train$GarageFinish %in% c("Fin", "RFn")] <- 0


price <- summarize(group_by(train, GarageQual),
                   mean(SalePrice, na.rm=T))

train$garqual[train$GarageQual == "Ex"] <- 5
train$garqual[train$GarageQual == "Gd"] <- 4
train$garqual[train$GarageQual == "TA"] <- 3
train$garqual[train$GarageQual == "Fa"] <- 2
train$garqual[train$GarageQual == "Po" | is.na(train$GarageQual)] <- 1


price <- summarize(group_by(train, GarageCond),
                   mean(SalePrice, na.rm=T))

train$garqual2[train$GarageCond == "Ex"] <- 5
train$garqual2[train$GarageCond == "Gd"] <- 4
train$garqual2[train$GarageCond == "TA"] <- 3
train$garqual2[train$GarageCond == "Fa"] <- 2
train$garqual2[train$GarageCond == "Po" | is.na(train$GarageCond)] <- 1


price <- summarize(group_by(train, PavedDrive),
                   mean(SalePrice, na.rm=T))

train$paved_drive[train$PavedDrive == "Y"] <- 1
train$paved_drive[!train$PavedDrive != "Y"] <- 0
train$paved_drive[is.na(train$paved_drive)] <- 0

price <- summarize(group_by(train, Functional),
                   mean(SalePrice, na.rm=T))

train$housefunction[train$Functional %in% c("Typ", "Mod")] <- 1
train$housefunction[!train$Functional %in% c("Typ", "Mod")] <- 0






#This doesn't seem worth using at the moment. May adjust later.


price <- summarize(group_by(train, SaleType),
                   mean(SalePrice, na.rm=T))

# price[order(price$`mean(SalePrice, na.rm = T)`),]

train$sale_cat[train$SaleType %in% c("New", "Con")] <- 5
train$sale_cat[train$SaleType %in% c("CWD", "ConLI")] <- 4
train$sale_cat[train$SaleType %in% c("WD")] <- 3
train$sale_cat[train$SaleType %in% c("COD", "ConLw", "ConLD")] <- 2
train$sale_cat[train$SaleType %in% c("Oth")] <- 1


price <- summarize(group_by(train, SaleCondition),
                   mean(SalePrice, na.rm=T))

# price[order(price$`mean(SalePrice, na.rm = T)`),]

train$sale_cond[train$SaleCondition %in% c("Partial")] <- 4
train$sale_cond[train$SaleCondition %in% c("Normal", "Alloca")] <- 3
train$sale_cond[train$SaleCondition %in% c("Family","Abnorml")] <- 2
train$sale_cond[train$SaleCondition %in% c("AdjLand")] <- 1


train$Street <- NULL
train$LotShape <- NULL
train$LandContour <- NULL
train$Utilities <- NULL
train$LotConfig <- NULL
train$LandSlope <- NULL
train$Neighborhood <- NULL
train$Condition1 <- NULL
train$Condition2 <- NULL
train$BldgType <- NULL
train$HouseStyle <- NULL
train$RoofStyle <- NULL
train$RoofMatl <- NULL

train$Exterior1st <- NULL
train$Exterior2nd <- NULL
train$MasVnrType <- NULL
train$ExterQual <- NULL
train$ExterCond <- NULL

train$Foundation <- NULL
train$BsmtQual <- NULL
train$BsmtCond <- NULL
train$BsmtExposure <- NULL
train$BsmtFinType1 <- NULL
train$BsmtFinType2 <- NULL

train$Heating <- NULL
train$HeatingQC <- NULL
train$CentralAir <- NULL
train$Electrical <- NULL
train$KitchenQual <- NULL
train$FireplaceQu <- NULL

train$GarageType <- NULL
train$GarageFinish <- NULL
train$GarageQual <- NULL
train$GarageCond <- NULL
train$PavedDrive <- NULL

train$Functional <- NULL
train$PoolQC <- NULL
train$Fence <- NULL
train$MiscFeature <- NULL
train$SaleType <- NULL
train$SaleCondition <- NULL
train$MSZoning <- NULL
train$Alley <- NULL


train$TotalBsmtSF[is.na(train$TotalBsmtSF)] <- 992
train$GarageYrBlt[is.na(train$GarageYrBlt)] <- 1977
train$MasVnrArea[is.na(train$MasVnrArea)] <- 0
model <- lm(SalePrice ~ ., data = train)
summary(model)

View(train$TotalBsmtSF)
step(model)

model2 <-lm(formula = SalePrice ~ MSSubClass + LotArea + OverallQual + 
              OverallCond + YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtUnfSF + 
              X1stFlrSF + X2ndFlrSF + LowQualFinSF + BsmtFullBath + BsmtHalfBath + 
              FullBath + BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + Fireplaces + 
              GarageCars + WoodDeckSF + ScreenPorch + MoSold + .data_RM + 
              .data_C..all. + .data_IR1 + .data_IR3 + .data_Bnk + .data_FR2 + 
              .data_CulDSac + .data_FR3 + .data + .data_Veenker + .data_Crawfor + 
              .data_NoRidge + .data_Somerst + .data_NWAmes + .data_BrkSide + 
              .data_NridgHt + .data_Edwards + .data_StoneBr + .data_NPkVill + 
              roof_hip_shed + roof_matl_hi + exterior_1 + exterior_2 + 
              exterior_mason_1 + exterior_cond + bsmt_cond1 + bsmt_cond2 + 
              bsmt_exp + kitchen + garqual2 + housefunction + sale_cat + 
              sale_cond, data = train)
summary(model2)

model3 <- lm(formula = SalePrice ~ MSSubClass + LotArea + OverallQual + 
               OverallCond + YearBuilt + MasVnrArea +  
               X1stFlrSF + X2ndFlrSF  + BsmtFullBath  + 
               FullBath + BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + Fireplaces + 
               GarageCars + WoodDeckSF + ScreenPorch +  
               .data_C..all. + .data_IR1  + .data_Bnk  + 
               .data_CulDSac  + .data_Veenker + .data_Crawfor + 
               .data_NoRidge + .data_Somerst  + .data_BrkSide + 
               .data_NridgHt + .data_Edwards + .data_StoneBr  + 
               roof_hip_shed + roof_matl_hi + exterior_1 + exterior_2 + 
                exterior_cond + bsmt_cond1 +  
               bsmt_exp + kitchen  + housefunction + sale_cat 
               , data = train)
summary(model3)

model4 <- lm(formula = SalePrice ~ MSSubClass + LotArea + OverallQual + 
               OverallCond + YearBuilt + MasVnrArea +  
               X1stFlrSF + X2ndFlrSF  + BsmtFullBath  + 
               FullBath + BedroomAbvGr  + TotRmsAbvGrd + Fireplaces + 
               GarageCars + WoodDeckSF + ScreenPorch +  
               .data_C..all.   + .data_Bnk  + 
               .data_CulDSac  + .data_Veenker + .data_Crawfor + 
               .data_NoRidge + .data_Somerst  + .data_BrkSide + 
               .data_NridgHt + .data_Edwards + .data_StoneBr  + 
               roof_hip_shed + roof_matl_hi +  exterior_2 + 
               exterior_cond + bsmt_cond1 +  
               bsmt_exp + kitchen  + housefunction + sale_cat 
             , data = train)
summary(model4)

model5 <- lm(formula = SalePrice ~ MSSubClass + LotArea + OverallQual + 
               OverallCond + YearBuilt + MasVnrArea +  
               X1stFlrSF + X2ndFlrSF  + BsmtFullBath  + 
               FullBath + BedroomAbvGr  + TotRmsAbvGrd + Fireplaces + 
               GarageCars + WoodDeckSF + ScreenPorch +  
                .data_Bnk  + 
               .data_CulDSac  + .data_Veenker + .data_Crawfor + 
               .data_NoRidge + .data_Somerst  + .data_BrkSide + 
               .data_NridgHt + .data_Edwards + .data_StoneBr  + 
               roof_hip_shed + roof_matl_hi +  
               exterior_cond + bsmt_cond1 +  
               bsmt_exp + kitchen  + housefunction + sale_cat 
             , data = train)
summary(model5)

model6 <- lm(formula = SalePrice ~ MSSubClass + LotArea + OverallQual + 
               OverallCond + YearBuilt + MasVnrArea +  
               X1stFlrSF + X2ndFlrSF  + BsmtFullBath  + 
               FullBath + BedroomAbvGr  + TotRmsAbvGrd + Fireplaces + 
               GarageCars + WoodDeckSF + ScreenPorch +  
               .data_Bnk  + 
               .data_CulDSac  + .data_Veenker + .data_Crawfor + 
               .data_NoRidge + .data_Somerst  + .data_BrkSide + 
               .data_NridgHt  + .data_StoneBr  + 
               roof_hip_shed + roof_matl_hi +  
               exterior_cond + bsmt_cond1 +  
               bsmt_exp + kitchen  + housefunction + sale_cat 
             , data = train)

summary(model6)
install.packages("lmtest")
library(lmtest)
par(mfrow=c(2,2))
plot(model6)

quantile(train$SalePrice , seq(0,1,0.02))
final_data_new = train[(train$SalePrice >= 91500.0 & train$SalePrice  <= 318028.1),]
model7 <-  lm(formula = SalePrice ~ MSSubClass + LotArea + OverallQual + 
                OverallCond + YearBuilt + MasVnrArea +  
                X1stFlrSF + X2ndFlrSF  + BsmtFullBath  + 
                FullBath + BedroomAbvGr  + TotRmsAbvGrd + Fireplaces + 
                GarageCars + WoodDeckSF + ScreenPorch +  
                .data_Bnk  + 
                .data_CulDSac  + .data_Veenker + .data_Crawfor + 
                .data_NoRidge + .data_Somerst  + .data_BrkSide + 
                .data_NridgHt  + .data_StoneBr  + 
                roof_hip_shed + roof_matl_hi +  
                exterior_cond + bsmt_cond1 +  
                bsmt_exp + kitchen  + housefunction + sale_cat 
              , data = final_data_new)

summary(model7)

model8 <- lm(formula = SalePrice ~ MSSubClass + LotArea + OverallQual + 
               OverallCond + YearBuilt  +  
               X1stFlrSF + X2ndFlrSF  + BsmtFullBath  + 
               FullBath    + Fireplaces + 
               GarageCars + WoodDeckSF + ScreenPorch +  
               .data_Bnk  + 
               .data_CulDSac  + .data_Veenker + .data_Crawfor + 
               .data_NoRidge + .data_Somerst  + .data_BrkSide + 
               .data_NridgHt  + .data_StoneBr  + 
               roof_matl_hi +  
               exterior_cond + bsmt_cond1 +  
               bsmt_exp + kitchen  + housefunction 
             , data = final_data_new)
summary(model8)


plot(model8)
durbinWatsonTest(model8)

hist(residuals(model8))
library(randomForest)
model_rf <- randomForest(SalePrice ~ .,data = train, do.trace = T)

str(train)

train$.data <- NULL

summary(test)




test$pos_features_1[test$Condition1 %in% c("PosA", "PosN")] <- 1
test$pos_features_1[!test$Condition1 %in% c("PosA", "PosN")] <- 0

test$pos_features_2[test$Condition1 %in% c("PosA", "PosN")] <- 1
test$pos_features_2[!test$Condition1 %in% c("PosA", "PosN")] <- 0


test$twnhs_end_or_1fam[test$BldgType %in% c("1Fam", "TwnhsE")] <- 1
test$twnhs_end_or_1fam[!test$BldgType %in% c("1Fam", "TwnhsE")] <- 0

test$house_style_level[test$HouseStyle %in% housestyle_lo$HouseStyle] <- 1
test$house_style_level[test$HouseStyle %in% housestyle_med$HouseStyle] <- 2
test$house_style_level[test$HouseStyle %in% housestyle_hi$HouseStyle] <- 3


test$roof_hip_shed[test$RoofStyle %in% c("Hip", "Shed")] <- 1
test$roof_hip_shed[!test$RoofStyle %in% c("Hip", "Shed")] <- 0

test$roof_matl_hi[test$RoofMatl %in% c("Membran", "WdShake", "WdShngl")] <- 1
test$roof_matl_hi[!test$RoofMatl %in% c("Membran", "WdShake", "WdShngl")] <- 0

test$exterior_1[test$Exterior1st %in% matl_lo_1$Exterior1st] <- 1
test$exterior_1[test$Exterior1st %in% matl_med_1$Exterior1st] <- 2
test$exterior_1[test$Exterior1st %in% matl_hi_1$Exterior1st] <- 3

test$exterior_2[test$Exterior2nd %in% matl_lo$Exterior2nd] <- 1
test$exterior_2[test$Exterior2nd %in% matl_med$Exterior2nd] <- 2
test$exterior_2[test$Exterior2nd %in% matl_hi$Exterior2nd] <- 3


test$exterior_mason_1[test$MasVnrType %in% c("Stone", "BrkFace") | is.na(test$MasVnrType)] <- 1
test$exterior_mason_1[!test$MasVnrType %in% c("Stone", "BrkFace") & !is.na(test$MasVnrType)] <- 0

test$exterior_cond[test$ExterQual == "Ex"] <- 4
test$exterior_cond[test$ExterQual == "Gd"] <- 3
test$exterior_cond[test$ExterQual == "TA"] <- 2
test$exterior_cond[test$ExterQual == "Fa"] <- 1

test$exterior_cond2[test$ExterCond == "Ex"] <- 5
test$exterior_cond2[test$ExterCond == "Gd"] <- 4
test$exterior_cond2[test$ExterCond == "TA"] <- 3
test$exterior_cond2[test$ExterCond == "Fa"] <- 2
test$exterior_cond2[test$ExterCond == "Po"] <- 1


test$found_concrete[test$Foundation == "PConc"] <- 1
test$found_concrete[test$Foundation != "PConc"] <- 0


test$bsmt_cond1[test$BsmtQual == "Ex"] <- 5
test$bsmt_cond1[test$BsmtQual == "Gd"] <- 4
test$bsmt_cond1[test$BsmtQual == "TA"] <- 3
test$bsmt_cond1[test$BsmtQual == "Fa"] <- 2
test$bsmt_cond1[is.na(test$BsmtQual)] <- 1

test$bsmt_cond2[test$BsmtCond == "Gd"] <- 5
test$bsmt_cond2[test$BsmtCond == "TA"] <- 4
test$bsmt_cond2[test$BsmtCond == "Fa"] <- 3
test$bsmt_cond2[is.na(test$BsmtCond)] <- 2
test$bsmt_cond2[test$BsmtCond == "Po"] <- 1


test$bsmt_exp[test$BsmtExposure == "Gd"] <- 5
test$bsmt_exp[test$BsmtExposure == "Av"] <- 4
test$bsmt_exp[test$BsmtExposure == "Mn"] <- 3
test$bsmt_exp[test$BsmtExposure == "No"] <- 2
test$bsmt_exp[is.na(test$BsmtExposure)] <- 1


test$bsmt_fin1[test$BsmtFinType1 == "GLQ"] <- 5
test$bsmt_fin1[test$BsmtFinType1 == "Unf"] <- 4
test$bsmt_fin1[test$BsmtFinType1 == "ALQ"] <- 3
test$bsmt_fin1[test$BsmtFinType1 %in% c("BLQ", "Rec", "LwQ")] <- 2
test$bsmt_fin1[is.na(test$BsmtFinType1)] <- 1


test$bsmt_fin2[test$BsmtFinType2 == "ALQ"] <- 6
test$bsmt_fin2[test$BsmtFinType2 == "Unf"] <- 5
test$bsmt_fin2[test$BsmtFinType2 == "GLQ"] <- 4
test$bsmt_fin2[test$BsmtFinType2 %in% c("Rec", "LwQ")] <- 3
test$bsmt_fin2[test$BsmtFinType2 == "BLQ"] <- 2
test$bsmt_fin2[is.na(test$BsmtFinType2)] <- 1

test$gasheat[test$Heating %in% c("GasA", "GasW")] <- 1
test$gasheat[!test$Heating %in% c("GasA", "GasW")] <- 0

test$heatqual[test$HeatingQC == "Ex"] <- 5
test$heatqual[test$HeatingQC == "Gd"] <- 4
test$heatqual[test$HeatingQC == "TA"] <- 3
test$heatqual[test$HeatingQC == "Fa"] <- 2
test$heatqual[test$HeatingQC == "Po"] <- 1


test$air[test$CentralAir == "Y"] <- 1
test$air[test$CentralAir == "N"] <- 0

test$standard_electric[test$Electrical == "SBrkr" | is.na(test$Electrical)] <- 1
test$standard_electric[!test$Electrical == "SBrkr" & !is.na(test$Electrical)] <- 0


test$kitchen[test$KitchenQual == "Ex"] <- 4
test$kitchen[test$KitchenQual == "Gd"] <- 3
test$kitchen[test$KitchenQual == "TA"] <- 2
test$kitchen[test$KitchenQual == "Fa"] <- 1

test$fire[test$FireplaceQu == "Ex"] <- 5
test$fire[test$FireplaceQu == "Gd"] <- 4
test$fire[test$FireplaceQu == "TA"] <- 3
test$fire[test$FireplaceQu == "Fa"] <- 2
test$fire[test$FireplaceQu == "Po" | is.na(test$FireplaceQu)] <- 1


test$gar_attach[test$GarageType %in% c("Attchd", "BuiltIn")] <- 1
test$gar_attach[!test$GarageType %in% c("Attchd", "BuiltIn")] <- 0


test$gar_finish[test$GarageFinish %in% c("Fin", "RFn")] <- 1
test$gar_finish[!test$GarageFinish %in% c("Fin", "RFn")] <- 0

test$garqual[test$GarageQual == "Ex"] <- 5
test$garqual[test$GarageQual == "Gd"] <- 4
test$garqual[test$GarageQual == "TA"] <- 3
test$garqual[test$GarageQual == "Fa"] <- 2
test$garqual[test$GarageQual == "Po" | is.na(test$GarageQual)] <- 1


test$garqual2[test$GarageCond == "Ex"] <- 5
test$garqual2[test$GarageCond == "Gd"] <- 4
test$garqual2[test$GarageCond == "TA"] <- 3
test$garqual2[test$GarageCond == "Fa"] <- 2
test$garqual2[test$GarageCond == "Po" | is.na(test$GarageCond)] <- 1


test$paved_drive[test$PavedDrive == "Y"] <- 1
test$paved_drive[!test$PavedDrive != "Y"] <- 0
test$paved_drive[is.na(test$paved_drive)] <- 0

test$housefunction[test$Functional %in% c("Typ", "Mod")] <- 1
test$housefunction[!test$Functional %in% c("Typ", "Mod")] <- 0


test$pool_good[test$PoolQC %in% c("Ex")] <- 1
test$pool_good[!test$PoolQC %in% c("Ex")] <- 0

test$priv_fence[test$Fence %in% c("GdPrv")] <- 1
test$priv_fence[!test$Fence %in% c("GdPrv")] <- 0

test$sale_cat[test$SaleType %in% c("New", "Con")] <- 5
test$sale_cat[test$SaleType %in% c("CWD", "ConLI")] <- 4
test$sale_cat[test$SaleType %in% c("WD")] <- 3
test$sale_cat[test$SaleType %in% c("COD", "ConLw", "ConLD")] <- 2
test$sale_cat[test$SaleType %in% c("Oth")] <- 1

test$sale_cond[test$SaleCondition %in% c("Partial")] <- 4
test$sale_cond[test$SaleCondition %in% c("Normal", "Alloca")] <- 3
test$sale_cond[test$SaleCondition %in% c("Family","Abnorml")] <- 2
test$sale_cond[test$SaleCondition %in% c("AdjLand")] <- 1





test$Street <- NULL
test$LotShape <- NULL
test$LandContour <- NULL
test$Utilities <- NULL
test$LotConfig <- NULL
test$LandSlope <- NULL
test$Neighborhood <- NULL
test$Condition1 <- NULL
test$Condition2 <- NULL
test$BldgType <- NULL
test$HouseStyle <- NULL
test$RoofStyle <- NULL
test$RoofMatl <- NULL

test$Exterior1st <- NULL
test$Exterior2nd <- NULL
test$MasVnrType <- NULL
test$ExterQual <- NULL
test$ExterCond <- NULL

test$Foundation <- NULL
test$BsmtQual <- NULL
test$BsmtCond <- NULL
test$BsmtExposure <- NULL
test$BsmtFinType1 <- NULL
test$BsmtFinType2 <- NULL

test$Heating <- NULL
test$HeatingQC <- NULL
test$CentralAir <- NULL
test$Electrical <- NULL
test$KitchenQual <- NULL
test$FireplaceQu <- NULL

test$GarageType <- NULL
test$GarageFinish <- NULL
test$GarageQual <- NULL
test$GarageCond <- NULL
test$PavedDrive <- NULL

test$Functional <- NULL
test$PoolQC <- NULL
test$Fence <- NULL
test$MiscFeature <- NULL
test$SaleType <- NULL
test$SaleCondition <- NULL
test$MSZoning <- NULL
test$Alley <- NULL

#Fix some NAs

test$GarageYrBlt[is.na(test$GarageYrBlt)] <- 0
test$MasVnrArea[is.na(test$MasVnrArea)] <- 0
test$LotFrontage[is.na(test$LotFrontage)] <- 0
test$BsmtFinSF1[is.na(test$BsmtFinSF1)] <- 0
test$BsmtFinSF2[is.na(test$BsmtFinSF2)] <- 0
test$BsmtUnfSF[is.na(test$BsmtUnfSF)] <- 0
test$TotalBsmtSF[is.na(test$TotalBsmtSF)] <- 0

test$BsmtFullBath[is.na(test$BsmtFullBath)] <- 0
test$BsmtHalfBath[is.na(test$BsmtHalfBath)] <- 0
test$GarageCars[is.na(test$GarageCars)] <- 0
test$GarageArea[is.na(test$GarageArea)] <- 0
test$pubutil[is.na(test$pubutil)] <- 0

SalePrice <- predict.lm(model8,test)
test$SalePrice <- predict.lm(model8,test)

test_original <- read.csv("E:/R/housing price/test (1).csv")

Id <- test_original$Id
submission <- data.frame(Id = Id,SalePrice)
View(submission)

write.csv(submission,"E:/R/housing price/submission_rf.csv",row.names = F)
final_rf <- predict(model_rf,test)

SalePrice <- final_rf

submission_rf <- data.frame(Id = Id,SalePrice)
write.csv(submission_rf,"E:/R/housing price/submission_rf.csv",row.names = F)
any(is.na(submission))
summary(submission)
submission$SalePrice[is.na(submission$SalePrice)] = 163595
