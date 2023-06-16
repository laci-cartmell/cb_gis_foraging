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



##########################
# Random Forest with Statology
#########################



install.packages("randomForest")
install.packages('caret')
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("cowplot")
install.packages("e1071")

library(randomForest)
library(datasets)
library(caret) # confusion Matrix
library(tidyverse)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(e1071)



# fit the rf model
#practice with data set -- GIS_Landuse_master, datatable


#read in file
GIS_Landuse_master <- read_excel("GIS_Landuse_master.xlsx", 
                                 sheet = "Wetlands Combined")
datatable <- GIS_Landuse_master
glimpse(datatable)
view(datatable)
# 1362x19


# factor Colony Name, Substrate, Year
#  SPECIFY SUBSTRATE/YEAR/COLONYNAME AS FACTOR VARS, INDICATE BASE FOR SUBSTRATE AND YEAR
datatable$COLONYNAME <- as.factor(datatable$COLONYNAME)
datatable$Substrate <- as.factor(datatable$Substrate)
datatable$Year <- as.factor(datatable$Year)
is.factor(datatable$Substrate)

#Remove colony ID
features <- colnames(datatable[,2:19])
columns <- colnames(df)

df <- datatable[,features] 
glimpse(features)
glimpse(df)
# mtry = 1/3 *p
mtry_r =(17/3)


#################
# MODELTRAINING #
###################
#split dataset into training (70%) testing (30%)
train_index <- createDataPartition(y=df$Size, p=0.7, list=FALSE)

#subset data
training_set <- df[train_index, ]
testing_set <- df[-train_index, ]




#https://www.guru99.com/r-random-forest-tutorial.html


# Set seed for reproducibility 
#set.seed(4)

#k-validation
#repeated cv with 5 folds, 3 repeats
repeat_cv <- trainControl(method ='repeatedcv', number=10,repeats=3, search = "grid")
trControl <- trainControl(method = "repeatedcv", 
                          number=10,
                          repeats=3,
                          search="grid" #random search
)


# use caret to evaluate model
rf_default <- train(Size ~.,
                    data = training_set,
                    method = "rf",
                    metric="RMSE",
                    trControl = trControl)

print(rf_default)

#search for best mtry
# Construct vector with value 1-10
tuneGrid <- expand.grid(.mtry = c(1: 10)) 
rfmtry <- train(Size ~., 
                data = training_set,
                methods='rf',
                #add repeated cv
                #accuracy measurement
                metric='RMSE',
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 14,
                ntree = 300)
print(rfmtry)

# best value of mtry
rfmtry$bestTune$mtry
max(rfmtry$results$Accuracy)
best_mtry <-  rfmtry$bestTune$mtry

#Find best maxnodes

#create list where model results are stored
store_maxnode <- list()
#use best mtry
tuneGrid <- expand.grid(.mtry = best_mtry)
#loop to evaluate maxnode values 5-15
for (maxnodes in c(5: 15)) {
 # set.seed(1234)
  rf_maxnode <- train(Size~.,
                      data = training_set,
                      method = "rf",
                      metric = "RMSE",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)

### trying higher values
for (maxnodes in c(20: 30)) {
  # set.seed(1234)
  rf_maxnode <- train(Size~.,
                      data = training_set,
                      method = "rf",
                      metric = "RMSE",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  key <- toString(maxnodes)
  store_maxnode[[key]] <- rf_maxnode
}
results_node <- resamples(store_maxnode)
summary(results_node)

## Best number of trees
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(5678)
  rf_maxtrees <- train(Size~.,
                       data = training_set,
                       method = "rf",
                       metric = "RMSE",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = 24,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

## Now model is tuned and we can train it!
# Train random forest with new params
fit_rf <- train(survived~.,
                data_train,
                method = "rf",
                metric = "RMSE",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 14,
                ntree = 800,
                maxnodes = 24)

# Evaluate Model with caret
# Use prediction to compute -----
prediction <- predict(fit_rf, training_set)
#compare fit
confusionMatrix(prediction,
                testing_set$Size)

#Visualize results
varImpPlot(fit_rf)

#https://www.guru99.com/r-random-forest-tutorial.html








# Variable Importance Goes Here !

#### Performance on Testing Data
#Generate Predictions
y_predict <- predict(
  object=forest,
  newdata=testing_set[, -1]  # data for predictions, no Size
)

# Accuracy of model 
accuracy <- mean(y_predict == testing_set$Size)*100
cat('Accuracy on testing data: ', round(accuracy, 2), '%',  sep='')

#####################
# MODEL TUNING ####
##########################
library(randomForest)
install.packages("mlbench")
library(mlbench)
data(BostonHousing)
train <- BostonHousing
train[,12]
#subset data
training_set[,1]
#Tune
t <- tuneRF(train[,-5], train[,5],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 150,
            trace = TRUE,
            improve = 0.05)
rf_tuned <- tuneRF(training_set[,1], training_set[,-1],
                   ntreeTry=500,
                  mtry = 2, #mtrystart
                   stepFactor = .5,
                   plot = TRUE,
                   trace = FALSE,
                   improve = 0.001)
##
tr = dt
rf_tuned
#
tuneRF_res = lapply(1:10,function(i){
  
  tr = tuneRF(training_set[,1], training_set[,-1],mtryStart=2,step=0.9,ntreeTry = 100,trace = TRUE,improve=1e-5)
  tr = data.frame(tr)
  tr$RMSE = sqrt(tr[,2])
  tr
})

tuneRF_res = do.call(rbind,tuneRF_res)

control <- trainControl(method="cv", number=10,returnResamp="all")
tunegrid <- expand.grid(.mtry=c(2:7))
caret_res <- train(Size ~., data=train, method="rf", metric="RMSE", 
                   tuneGrid=tunegrid, ntree = 100, trControl=control)

library(ggplot2)
df = rbind(
  data.frame(tuneRF_res[,c("mtry","RMSE")],test="tuneRF"),
  data.frame(caret_res$resample[,c("mtry","RMSE")],test="caret")
)
df = df[df$mtry!=1,]

ggplot(df,aes(x=mtry,y=RMSE,col=test))+
  stat_summary(fun.data=mean_se,geom="errorbar",width=0.2) +
  stat_summary(fun=mean,geom="line") + facet_wrap(~test)















model <- randomForest(Size ~ ., data = df, ntree = 1000,
                      keep.forest=FALSE, importance = TRUE)
#display fitted model
rf
# number of trees that produce lowest test MSE
which.min(rf$mse)

#find RMSE of best model
sqrt(rf$mse[which.min(rf$mse)])
#   #AVG diff btw predicted and observed

# plot test MSE based on # of trees used
plot(rf)

# plot with importance of each predictor var 
varImpPlot(rf) 
#x-axis= avg.incr in node purity of regression trees y-axis=splitting of various predictors

#tune the model & plots OOB vs mxy
model_tuned <- tuneRF(
  x <- df[,c(columns, 'Size')], #define predictor variables
  y=df$Size, #define response variable
  ntreeTry=500,
  mtryStart=4, 
  stepFactor=1.5,
  improve=0.01,
  trace=FALSE #don't show real-time progress
)
varImpPlot(model_tuned) 
importance(model_tuned, scale=F)






#########################
## DROP-COLUMN IMPORTANCE
###############################




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
# add random column
#random from uniform distribution for same rows as df
df['random'] <- runif(nrow(df)) 
glimpse(df)

##########
#

### Type 1 - Mean decrease in MSE by **Permutation
# without random column

# random forest
rf1 <- randomForest(Size ~ ., data = df[, 1:18], mtry=mtry_r,
                    ntree = 1000, importance=T)
#feature importance
importance(rf1, scale=F)
#
p1 <- create_rfplot(rf1, type = 1)  # type 1 - importances, regression
#ggsave('../article/images/regr_permute_R.svg',plot = p1, device = 'svg', height = 4, width = 6)
p1

# with random column

rf2 <- randomForest(Size~., data = df, mtry = mtry_r,
                    ntree = 1000, importance=T)
importance(rf2, scale=F)
p2 <- create_rfplot(rf2, type = 1)
#ggsave('../article/images/regr_permute_random_R.svg',
p2
#plot = p2, device = 'svg', height = 4, width = 6)
imp1 <- data.frame(importance(rf2, type = 1, scale=F))
imp1
write.csv(imp1, file="./imp_R_regr_MSE.csv")

#########################
## DROP-COLUMN IMPORTANCE
###############################

## Examine Costs by Drop-Column Importance

# PARAMS ######## use tuned params: ntree = 40, mtry = 2, nodesize = 1

get_drop_imp <- function(df, columns){
  X <- df[,c(columns, 'Size')] # data
  rf <- randomForest(Size ~., data = X,
                     ntree = 500, mtry=mtry_r, nodesize=5, importance=T)
  full_rsq <- mean(rf$rsq) # R-squared
  
  imp <- c()
  for (c in columns){
    X_sub <- X[, !(colnames(X) == c)]
    rf <- randomForest(Size~., data = X_sub,
                       ntree = 500, mtry=mtry_r, nodesize=5, importance=T)
    sub_rsq <- mean(rf$rsq) # R-squared
    diff_rsq <- full_rsq - sub_rsq
    imp <- c(imp, diff_rsq)
  }
  featureImportance <- data.frame(Feature=columns, Importance=imp)
  return(featureImportance)
}

columns <- colnames(df[2:18])

featureImportance <- get_drop_imp(df, columns)

p500 <- create_ggplot(featureImportance)
p500
#ggsave('../article/images/regr_drop_R.svg',
#plot = p1, device = 'svg', height = 4, width = 6)

columns <- colnames(df[2:19])
featureImportance <- get_drop_imp(df, columns)
p2 <- create_ggplot(featureImportance)
#ggsave('../article/images/regr_drop_random_R.svg',
#plot = p2, device = 'svg', height = 4, width = 6)

write.csv(featureImportance, file="./data/imp_R_regr_drop.csv")


featureImportance <- get_drop_imp(df, columns)
p2 <- create_ggplot(featureImportance)
#ggsave('../article/images/regr_drop_random_R.svg',
#plot = p2, device = 'svg', height = 4, width = 6)

write.csv




########################
# VAR IMPORTANCE - TEST/TRAIN SET
###########################
# choose model params - node, max tree, mtry
# Check performance of those params by comparing a test/train set to look at accuracy
# Then use those params in determining Feature Importance
# With Feature Importance, able to begin writing results
#
#

















### CLASSIFICATION - Target Var must be factored

# set.seed
set.seed(3)
df <- (df[1:18])
# set test & train data
# sample data & create 2 datasets with replacement with 70 in one and 30% in other
ind <- sample(2, nrow(df), replace = TRUE, prob = c(0.7, 0.3))
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

rf.fit <- randomForest(Size ~ ., data = df, ntree = 500,
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
sum(!complete.cases(datatable))

# Fill in missing values with col. median
for (i in 1:ncol(airquality)) {
  airquality[ , i][is.na(airquality[ , i])] <- median(airquality[ , i], na.rm=TRUE)
}

# Fit a random forest model in R
#make reproducible
set.seed(1)


model <- randomForest(Size ~ ., data = df, ntree = 1000,
                       keep.forest=FALSE, importance = TRUE)
#display fitted model
model



#   #AVG diff btw predicted and observed

##################