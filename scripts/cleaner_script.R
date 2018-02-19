# Import data

train = read.csv('data/train1.csv')
test = read.csv('data/test.csv')

#Drop unwanted columns
train_drop = c('LandContour','Utilities','LandSlope','Condition2',
                 'SaleType','Street','RoofMatl','X3SsnPorch', 'Functio.1l')

test_drop = c('LandContour','Utilities','LandSlope','Condition2',
                 'SaleType','Street','RoofMatl','X3SsnPorch','PoolQC', 'Fence',
                 'MiscFeature', 'Alley', 'Functional')

train = train[, !(names(train) %in% train_drop)]
test = test[, !(names(test) %in% test_drop)]

#Fix column names
names(train.Clean)[names(train.Clean)=='X1stFlrSF'] = 'FirstFlrSF'
names(train.Clean)[names(train.Clean)=='X2ndFlrSF'] = 'SecFlrSF'
names(train.Clean)[names(train.Clean)=='Kitche.1bvGr'] = 'KitchenAbvGr'
names(train.Clean)[names(train.Clean)=='Functio.1l'] = 'Functional'
names(train.Clean)[names(train.Clean)=='ScreenPorch'] = 'ScreenPorchSF'

names(test.Clean)[names(test.Clean)=='X1stFlrSF'] = 'FirstFlrSF'
names(test.Clean)[names(test.Clean)=='X2ndFlrSF'] = 'SecFlrSF'
names(test.Clean)[names(test.Clean)=='Functio.1l'] = 'Functional'
names(test.Clean)[names(test.Clean)=='ScreenPorch'] = 'ScreenPorchSF'

#Create None factor level for FireplaceQu to represent the NA values
levels(test.Clean$FireplaceQu) = c(levels(test.Clean$FireplaceQu),'None')

#Identify source of missing data and reassign values
#The custom function factor.adjust will handle both categorical and continuous
#continuous variables

level.Fix =  rbind.data.frame(
  c('MSZoning','C (all)','C'),
  c('LotFrontage','-1','0'),
  c('Neighborhood','-1mes','NAmes'),
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
  c('GarageCond','-1','None'),
  c('LotFrontage','313','150'),
  c('LotFrontage','174','150'),
  c('LotFrontage','200','150'),
  c('LotFrontage','168','150'),
  c('LotFrontage','182','150'),
  c('LotFrontage','160','150'),
  c('LotFrontage','152','150'),
  c('LotFrontage','153','150')
)

level.Fix2 =  rbind.data.frame(
  c('MSZoning','C (all)','C')
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

train.Clean = factor.Adjust(data=train.Clean,adj=level.Fix)
test.Clean = factor.Adjust(data=test.Clean,adj=level.Fix2)
test.Clean = factor.Adjust(data=test.Clean,adj=level.Fix3,narep=TRUE)


#New features based on Age
train.Clean$HomeAge = train.Clean$YrSold - train.Clean$YearBuilt
train.Clean$RemodAge = train.Clean$YrSold - train.Clean$YearRemodAdd

test.Clean$HomeAge = test.Clean$YrSold - test.Clean$YearBuilt
test.Clean$RemodAge = test.Clean$YrSold - test.Clean$YearRemodAdd

#Create a predicted SalePrice for the train
test.Clean$SalePrice = -1

#Indicator variables which are required for correct parameterization of 
#continuous variables which have no value for certain homes
#e.g. 81 homes have no garage, so we use hasGarage x GarageYrBlt
#instead of dropping GarageYrBlt because of the homes with value 0

new.Indicators.Indices = 
  list(
    'idxhasG' = which(train.Clean$GarageYrBlt != 0),
    'idxhasMV' = which(train.Clean$MasVnrArea != 0),
    'idxhasFB1' = which(train.Clean$BsmtFinSF1 != 0),
    'idxhasFB2' = which(train.Clean$BsmtFinSF2 != 0),
    'idxhasB' = which(train.Clean$TotalBsmtSF != 0),
    'idxhasSF' = which(train.Clean$SecFlrSF != 0),
    'idxhasPool' = which(train.Clean$PoolArea != 0),
    'idxhasLF' = which(train.Clean$LotFrontage != 0),
    'idxhasLQF' = which(train.Clean$LowQualFinSF != 0),
    'idxhasWD' = which(train.Clean$WoodDeckSF != 0),
    'idxhasOP' = which(train.Clean$OpenPorchSF != 0),
    'idxhasEP' = which(train.Clean$EnclosedPorch != 0),
    'idxhasSP' = which(train.Clean$ScreenPorchSF != 0),
    'idxhasMV' = which(train.Clean$MiscVal != 0),
    'idxisOld' = which(train.Clean$YearBuilt < 2000),
    'idxhasRem' = which(train.Clean$YearRemodAdd != train.Clean$YearBuilt),
    'idxisNew' = which(train.Clean$YearBuilt >= 2000)
  )

new.Indicators.Indices2 = 
  list(
    'idxhasG' = which(test.Clean$GarageYrBlt != 0),
    'idxhasMV' = which(test.Clean$MasVnrArea != 0),
    'idxhasFB1' = which(test.Clean$BsmtFinSF1 != 0),
    'idxhasFB2' = which(test.Clean$BsmtFinSF2 != 0),
    'idxhasB' = which(test.Clean$TotalBsmtSF != 0),
    'idxhasSF' = which(test.Clean$SecFlrSF != 0),
    'idxhasPool' = which(test.Clean$PoolArea != 0),
    'idxhasLF' = which(test.Clean$LotFrontage != 0),
    'idxhasLQF' = which(test.Clean$LowQualFinSF != 0),
    'idxhasWD' = which(test.Clean$WoodDeckSF != 0),
    'idxhasOP' = which(test.Clean$OpenPorchSF != 0),
    'idxhasEP' = which(test.Clean$EnclosedPorch != 0),
    'idxhasSP' = which(test.Clean$ScreenPorchSF != 0),
    'idxhasMV' = which(test.Clean$MiscVal != 0),
    'idxisOld' = which(test.Clean$YearBuilt < 2000),
    'idxhasRem' = which(test.Clean$YearRemodAdd != test.Clean$YearBuilt),
    'idxisNew' = which(test.Clean$YearBuilt >= 2000)    
  )

train.Clean = indicator.Add(data=train.Clean,indices = new.Indicators.Indices)
test.Clean = indicator.Add(data=test.Clean,indices = new.Indicators.Indices2)


#Imputing by sampling
test.Clean$LotFrontage = impute.bySampling(data=test.Clean$LotFrontage)
test.Clean$BsmtFinType1 = impute.bySampling(data=test.Clean$BsmtFinType1)
test.Clean$BsmtFullBath = impute.bySampling(data=test.Clean$BsmtFullBath)
test.Clean$GarageType = impute.bySampling(data=test.Clean$GarageType)
test.Clean$GarageYrBlt = impute.bySampling(data=test.Clean$GarageYrBlt)
test.Clean$GarageFinish = impute.bySampling(data=test.Clean$GarageFinish)
test.Clean$GarageArea = impute.bySampling(data=test.Clean$GarageArea)

#Variable Transformations

log.Transform = c('LotArea', 'MasVnrArea', 'BsmtFinSF1', 
                  'TotalBsmtSF', 'FirstFlrSF', 'SecFlrSF', 'GrLivArea',  
                  'WoodDeckSF', 'OpenPorchSF', 'MiscVal', 'SalePrice')

log.Transform2 = c('LotArea', 'MasVnrArea', 'BsmtFinSF1', 
                   'TotalBsmtSF', 'FirstFlrSF', 'SecFlrSF', 'GrLivArea',  
                   'WoodDeckSF', 'OpenPorchSF', 'MiscVal')

#ln(.001) ~ -7
train.Clean[,log.Transform] = log(train.Clean[,log.Transform] + .0001)
test.Clean[,log.Transform2] = log(test.Clean[,log.Transform2] + .0001)

#Drop outlier records - irregular lot homes with unusually large lot space
outlier.idx = c(31,969,524,1299,899,1063,711,496,89)
train.Clean = train.Clean[-outlier.idx,]

#Merge training and test files, including '-1' for empty salePrices for SAS to predict
merge.Clean = rbind.data.frame(train.Clean,test.Clean)

write.csv(train.Clean,'data/train1_clean.csv',row.names=FALSE)
write.csv(test.Clean,'data/test_clean.csv',row.names=FALSE)
write.csv(merge.Clean,'data/merge_clean.csv',row.names=FALSE)

# 
# pdf('plots.pdf')
# feature.Levels = lapply(X=train.Clean,table)
# for (i in seq(feature.Levels)) {
# f.num = i
# barplot(feature.Levels[[f.num]],main=names(feature.Levels[f.num]))
# plot(x=train.Clean[,names(feature.Levels[f.num])],y=train.Clean$SalePrice,
#      xlab=names(feature.Levels[f.num]),col='black')
# points(x=train.Clean[outlier.idx,names(feature.Levels[f.num])],y=train.Clean[outlier.idx,'SalePrice'],
#      xlab=names(feature.Levels[f.num]),col='red')
# text(x=train.Clean[outlier.idx,names(feature.Levels[f.num])],y=train.Clean[outlier.idx,'SalePrice'],
#      labels = row.names(train.Clean[outlier.idx,]), pos = 1)
# }
# dev.off()
# fit = lm(log(SalePrice) ~ train.Clean[,names(feature.Levels[f.num])],data=train.Clean)
# 