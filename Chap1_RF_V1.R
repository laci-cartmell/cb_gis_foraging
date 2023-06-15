##
# 13 June 2023
# Code for Chapter 1 of my dissertation thesis
##
# VERSION 1.1
#   A. Data Visualization
#   B. Regression Analysis
#   C. Random Forest
##
# Using NLCD Land Use data from 2001-2019, created a dataset of the porportion 
#   of land use type within a 1-km foraging radius of a colony site
#  Other variables: Active or non-active; gps coordinates; year; # of nests;
#   temperature variability; Avg. temp over nesting/non-breeding season;
#   drought severity; irrigation use; farm ponds; simpsons index/colony; 
#
########################

setwd("C:/Users/Owner/Documents/GitHub/cb_gis_foraging")


# Required libraries
#install.packages("readxl")
library(readxl) #reads excel to table  !check sheets & data type for columns
library(readr)  #csv files

#read in file
GIS_Landuse_master <- read_excel("GIS_Landuse_master.xlsx", 
                                 sheet = "Wetlands Combined")
datatable <- GIS_Landuse_master
glimpse(datatable)
##########################
# Random Forest with Statology
#########################

install.packages("randomForest")
install.packages('caret')
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("cowplot")

library(randomForest)
library(datasets)
library(caret) # confusion Matrix
library(tidyverse)
library(ggplot2)
library(cowplot)
library(gridExtra)

# fit the rf model
#practice with data set -- GIS_Landuse_master, datatable
view(datatable)
# 1362x19

featurelist <- colnames(datatable)
#pull out vars with order
features <- c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width', 'Species')
df <- datatable[,featurelist] 

# add random column
#random from uniform distribution for same rows as df
df['random'] <- runif(nrow(df)) 
glimpse(df)


## Plotting Functions
create_rfplot <- function(rf,type){
  imp <- importance(rf, type=type, scale = F)
  featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])
  
  p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
    geom_bar(stat="identity", fill="#53cfff", width = 0.65) +
    coord_flip() + 
    theme_light(base_size=20) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_text(size = 15, color = "black"),
          axis.text.y = element_text(size = 15, color = "black")) 
  return(p)
}

# create ggplot
create_ggplot <- function(featureImportance){
  p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
    geom_bar(stat="identity", fill="#53cfff", width = 0.65) +
    coord_flip() + 
    theme_light(base_size=20) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_text(size = 15, color = "black"),
          axis.text.y = element_text(size = 15, color = "black")) 
  return(p)
}

glimpse(df)
### Type 1 - Mean decrease in MSE by **Permutation
# without random column
rf1 <- randomForest(Size ~ ., data = df, mtry=4,
                    ntree = 40, importance=T)
importance(rf1, scale=F)

p1 <- create_rfplot(rf1, type = 1)
#ggsave('../article/images/regr_permute_R.svg',plot = p1, device = 'svg', height = 4, width = 6)
p1

# with random column
rf2 <- randomForest(Size~., data = df, mtry = 4,
                    ntree = 40, importance=T)
importance(rf2, scale=F)
p2 <- create_rfplot(rf2, type = 1)
#ggsave('../article/images/regr_permute_random_R.svg',
p2
#plot = p2, device = 'svg', height = 4, width = 6)
imp1 <- data.frame(importance(rf2, type = 1, scale=F))
write.csv(imp1, file="./data/imp_R_regr_MSE.csv")

## Examine Costs by Drop-Column Importance
# PARAMS : ntree = 40, mtry = 2, nodesize = 1

get_drop_imp <- function(df, columns){
  X <- df[,c(columns, 'Size')] # data
  rf <- randomForest(price~., data = X,
                     ntree = 40, mtry=2, nodesize=1, importance=T)
  full_rsq <- mean(rf$rsq) # R-squared
  
  imp <- c()
  for (c in columns){
    X_sub <- X[, !(colnames(X) == c)]
    rf <- randomForest(price~., data = X_sub,
                       ntree = 40, mtry=2, nodesize=1, importance=T)
    sub_rsq <- mean(rf$rsq) # R-squared
    diff_rsq <- full_rsq - sub_rsq
    imp <- c(imp, diff_rsq)
  }
  featureImportance <- data.frame(Feature=columns, Importance=imp)
  return(featureImportance)
}

columns <- c('bathrooms', 'bedrooms', 'longitude', 'latitude')
featureImportance <- get_drop_imp(df, columns)
p1 <- create_ggplot(featureImportance)
#ggsave('../article/images/regr_drop_R.svg',
#plot = p1, device = 'svg', height = 4, width = 6)

columns <- c('bathrooms', 'bedrooms', 'longitude', 'latitude', 'random')
featureImportance <- get_drop_imp(df, columns)
p2 <- create_ggplot(featureImportance)
#ggsave('../article/images/regr_drop_random_R.svg',
#plot = p2, device = 'svg', height = 4, width = 6)

write.csv(featureImportance, file="./data/imp_R_regr_drop.csv")




### CLASSIFICATION - Target Var must be factored


#set species as a factor
data$Species <- as.factor(data$Species)
table(data$Species)
  # factor data needs to be balanced

# set.seed
set.seed(3)

# set test & train data
# sample data & create 2 datasets with replacement with 70 in one and 30% in other
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

# Random Forest in R
rf <- randomForest(Species ~ ., data=train, proximity=TRUE) # non-linear multiple regression
print(rf)
rf   

# Train Data -- let's see if we can lower 4.72%
p1 <- predict(rf, train)


confusionMatrix(p1, train$ Species) 
# calculates a cross-tabulation of obs & predicted classes with assoc. stats

# Test Data 
p2 <- predict(rf, test)
confusionMatrix(p2, test$ Species)

# Plot Error Rate
plot(rf)

## model has high accuracy, but can still tune it

#Tune
t <- tuneRF(train[,-5], train[,5],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 150,
            trace = TRUE,
            improve = 0.05)
# No. of Nodes
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")


#Variable Importance
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf) #MeanDecreaseGini

# Partial Dependence Plot
partialPlot(rf, train, Petal.Width, "setosa")

# Multi-dimensional Scaling Plot of Proximity Matrix
MDSplot(rf, train$Species)



# REGRESSION - Continuous variable 

rf.fit <- randomForest(Species ~ ., data = data, ntree = 1000,
                       keep.forest=FALSE, importance = TRUE)
# Get variable importance from the model fit
ImpData <- as.data.frame(importance(rf.fit))
ImpData$Var.Names <- row.names(ImpData)

ggplot(ImpData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )



#https://www.r-bloggers.com/2021/04/random-forest-in-r/



# practice 1
str(airquality)

#find number of rows with missing values
sum(!complete.cases(airquality))

# Fill in missing values with col. median
for (i in 1:ncol(airquality)) {
  airquality[ , i][is.na(airquality[ , i])] <- median(airquality[ , i], na.rm=TRUE)
}

# Fit a random forest model in R
#make reproducible
set.seed(1)

#fit the model
model <- randomForest(
  formula = Ozone ~ .,
  data = airquality
)

#display fitted model
model

# number of trees that produce lowest test MSE
which.min(model$mse)

#find RMSE of best model
sqrt(model$mse[which.min(model$mse)])
#   #AVG diff btw predicted and observed

# plot test MSE based on # of trees used
plot(model)

# plot with importance of each predictor var 
varImpPlot(model) 
#x-axis= avg.incr in node purity of regression trees y-axis=splitting of various predictors

#tune the model & plots OOB vs mxy
model_tuned <- tuneRF(
  x=airquality[,-1], #define predictor variables
  y=airquality$Ozone, #define response variable
  ntreeTry=500,
  mtryStart=4, 
  stepFactor=1.5,
  improve=0.01,
  trace=FALSE #don't show real-time progress
)

#   #AVG diff btw predicted and observed

# Use Final Model to Make Predictions
#define new observation
new <- data.frame(Solar.R=150, Wind=8, Temp=70, Month=5, Day=5)
#use bagged model to predict ozone
predict(model, newdata = new)
##################
############

rf = RandomForestRegressor(
  n_estimators = 300,         # # of decision trees
  max_features = 'sqrt',      # max features per tree = sqrt of number of params
  max_depth = 5,              # Max depth of each tree
  random_state = 18,           # keeps things standard
).fit(x_train, y_train)
  
# Model Performance

# Set prediction
prediction = rf.predict(x_test)
mse = mean_squared_error(y_test, prediction)
rmse = mse**.5

print(mse, rmse)