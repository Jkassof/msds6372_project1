# Import data

train <- read.csv('data/train1.csv')
test <- read.csv('data/test.csv')

#Drop unwanted columns
train_drop = c('LandContour','Utilities','LandSlope','Condition2',
                 'SaleType','Street','RoofMatl','X3SsnPorch', 'Functio.1l')

test_drop = c('LandContour','Utilities','LandSlope','Condition2',
                 'SaleType','Street','RoofMatl','X3SsnPorch','PoolQC', 'Fence',
                 'MiscFeature', 'Alley', 'Functional')

train <- train[, !(names(train) %in% train_drop)]
test  <- test[, !(names(test) %in% test_drop)]

# Fix column names
names(train)[names(train)=='X1stFlrSF'] = 'FirstFlrSF'
names(train)[names(train)=='X2ndFlrSF'] = 'SecFlrSF'
names(train)[names(train)=='Kitche.1bvGr'] = 'KitchenAbvGr'
names(train)[names(train)=='Functio.1l'] = 'Functional'
names(train)[names(train)=='ScreenPorch'] = 'ScreenPorchSF'

names(test)[names(test)=='X1stFlrSF'] = 'FirstFlrSF'
names(test)[names(test)=='X2ndFlrSF'] = 'SecFlrSF'
names(test)[names(test)=='Functio.1l'] = 'Functional'
names(test)[names(test)=='ScreenPorch'] = 'ScreenPorchSF'

# Create None factor level for FireplaceQu to represent the NA values
levels(test$FireplaceQu) = c(levels(test$FireplaceQu),'None')

#Identify source of missing data and reassign values
#The custom function factor.adjust will handle both categorical and
# continuous variables

level.Fix =  rbind.data.frame(
  c('MSZoning','C (all)','C'),
  c('LotFrontage','-1','0'),
  c('Neighborhood','-1mes','Other'),
  c('MasVnrType','-1','None'),
  c('MasVnrArea','-1','0'),
  c('BsmtQual','-1','None'),
  c('BsmtCond','-1','None'),
  c('BsmtExposure','-1','None'),
  c('BsmtFinType1','-1','None'),
  c('BsmtFinType2','-1','None'),
  c('Electrical','-1','SBrkr'),
  c('FireplaceQu','-1','None'),
  c('GarageType','-1','None'),
  c('GarageYrBlt','-1','0'),
  c('GarageFinish','-1','None'),
  c('GarageQual','-1','None'),
  c('GarageCond','-1','None')
  )

level.Fix2 =  rbind.data.frame(
  c('MSZoning','C (all)','C'),
  c('Neighborhood','-1mes','Other')
)

#Assign missing values to existing factor levels

level.Fix3 = rbind.data.frame(
  c('MSZoning','NA','RL'),
  c('Exterior1st','NA','VinylSd'),
  c('Exterior2nd','NA','VinylSd'),
  c('MasVnrType','NA','None'),
  c('MasVnrArea','NA','0'),
  c('BsmtQual','NA','TA'),
  c('BsmtCond','NA','TA'),
  c('BsmtExposure','NA','No'),
  c('BsmtFinSF1','NA','0'),
  c('BsmtFinSF2','NA','0'),
  c('BsmtFinType2','NA','Unf'),
  c('BsmtUnfSF','NA','0'),
  c('TotalBsmtSF','NA','0'),
  c('BsmtHalfBath','NA','0'),
  c('KitchenQual','NA','TA'),
  c('FireplaceQu','NA','None'),
  c('GarageCars','NA','2'),
  c('GarageQual','NA','TA'),
  c('GarageCond','NA','TA')
)

names(level.Fix) = c('Feature','Replace','With')

train = factor.Adjust(data=train,adj=level.Fix)
test = factor.Adjust(data=test,adj=level.Fix2)
test = factor.Adjust(data=test,adj=level.Fix3,narep=TRUE)



# Create a predicted SalePrice for the train. Use this line if exporting to
# SAS
test$SalePrice = -1

# Indicator variables which are required for correct parameterization of 
# continuous variables which have no value for certain homes
# e.g. 81 homes have no garage, so we use hasGarage x GarageYrBlt
# instead of dropping GarageYrBlt because of the homes with value 0

new.Indicators.Indices = 
  list(
    'idxhasG' = which(train$GarageYrBlt != 0),
    'idxhasMV' = which(train$MasVnrArea != 0),
    'idxhasFB1' = which(train$BsmtFinSF1 != 0),
    'idxhasFB2' = which(train$BsmtFinSF2 != 0),
    'idxhasB' = which(train$TotalBsmtSF != 0),
    'idxhasSF' = which(train$SecFlrSF != 0),
    'idxhasPool' = which(train$PoolArea != 0),
    'idxhasLF' = which(train$LotFrontage != 0),
    'idxhasLQF' = which(train$LowQualFinSF != 0),
    'idxhasWD' = which(train$WoodDeckSF != 0),
    'idxhasOP' = which(train$OpenPorchSF != 0),
    'idxhasEP' = which(train$EnclosedPorch != 0),
    'idxhasSP' = which(train$ScreenPorchSF != 0),
    'idxhasMV' = which(train$MiscVal != 0)
  )

new.Indicators.Indices2 = 
  list(
    'idxhasG' = which(test$GarageYrBlt != 0),
    'idxhasMV' = which(test$MasVnrArea != 0),
    'idxhasFB1' = which(test$BsmtFinSF1 != 0),
    'idxhasFB2' = which(test$BsmtFinSF2 != 0),
    'idxhasB' = which(test$TotalBsmtSF != 0),
    'idxhasSF' = which(test$SecFlrSF != 0),
    'idxhasPool' = which(test$PoolArea != 0),
    'idxhasLF' = which(test$LotFrontage != 0),
    'idxhasLQF' = which(test$LowQualFinSF != 0),
    'idxhasWD' = which(test$WoodDeckSF != 0),
    'idxhasOP' = which(test$OpenPorchSF != 0),
    'idxhasEP' = which(test$EnclosedPorch != 0),
    'idxhasSP' = which(test$ScreenPorchSF != 0),
    'idxhasMV' = which(test$MiscVal != 0)
  )

train = indicator.Add(data=train,indices = new.Indicators.Indices)
test = indicator.Add(data=test,indices = new.Indicators.Indices2)


#Imputing by sampling
test$LotFrontage = impute.bySampling(data=test$LotFrontage)
test$BsmtFinType1 = impute.bySampling(data=test$BsmtFinType1)
test$BsmtFullBath = impute.bySampling(data=test$BsmtFullBath)
test$GarageType = impute.bySampling(data=test$GarageType)
test$GarageYrBlt = impute.bySampling(data=test$GarageYrBlt)
test$GarageFinish = impute.bySampling(data=test$GarageFinish)
test$GarageArea = impute.bySampling(data=test$GarageArea)

# Variable Transformations

log.Transform = c('LotFrontage', 'LotArea', 'MasVnrArea', 'BsmtFinSF1', 
                  'TotalBsmtSF', 'FirstFlrSF', 'SecFlrSF', 'GrLivArea',  
                  'WoodDeckSF', 'OpenPorchSF', 'MiscVal', 'SalePrice')

log.Transform2 = c('LotFrontage', 'LotArea', 'MasVnrArea', 'BsmtFinSF1', 
                  'TotalBsmtSF', 'FirstFlrSF', 'SecFlrSF', 'GrLivArea',  
                  'WoodDeckSF', 'OpenPorchSF', 'MiscVal')

# Log transform skewed variables
train[,log.Transform] = log(train[,log.Transform] + .001)
test[,log.Transform2] = log(test[,log.Transform2] + .001)


# Merge training and test files, including '.' for empty salePrices for SAS to predict
merge = rbind.data.frame(train,test)

write.csv(train,'data/train1_clean.csv',row.names=FALSE)
write.csv(test,'data/test_clean.csv',row.names=FALSE)
write.csv(merge,'data/merge_clean.csv',row.names=FALSE)


# Below code can be uncommented and run if desired. It generates a PDF with 
# histograms and scatter/box plots (against sale price) for each variable in the data set.

# pdf('plots.pdf')
# feature.Levels = lapply(X=train,table)
# for (i in seq(feature.Levels)) {
# f.num = i
# barplot(feature.Levels[[f.num]],main=names(feature.Levels[f.num]))
# plot(x=train[,names(feature.Levels[f.num])],y=log(train$SalePrice),
#      xlab=names(feature.Levels[f.num]))
# }
# dev.off()
# fit = lm(log(SalePrice) ~ train[,names(feature.Levels[f.num])],data=train)
# 
