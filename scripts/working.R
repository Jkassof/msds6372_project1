# Useful for realtor will be info readily found out or available and easy to
# interpret what the coefficient means

# Location location location vs whats on the outside vs whats on the inside

location <- train %>%
  select(MSSubClass,
         MSZoning,
         LotFrontage,
         LotArea,
         Neighborhood,
         Condition1)

outside <- train %>%
  select(LotConfig,
         BldgType,
         HouseStyle,
         RoofStyle,
         Exterior1st,
         Exterior2nd,
         MasVnrType,
         MasVnrArea,
         ExterQual,
         ExterCond)

inside <- train %>%
  select(Foundation,
         BsmtFinType1)

MSSubClass
Lot Area
Utilities
Neighborhood
Bldg Type
Overall Quality
Overall Condition
Year Remod/Add
Exterior1
Exterior2
BsmtQual
1st Flr SF (Continuous): First Floor square feet
2nd Flr SF (Continuous)	: Second floor square feet
Low Qual Fin SF (Continuous): Low quality finished square feet (all floors)
KitchenQual
GarageFinish
Pool