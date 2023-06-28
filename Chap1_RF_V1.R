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



install.packages("Metrics")
library(Metrics)
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
library(readxl)


# fit the rf model
#practice with data set -- GIS_Landuse_master, datatable
#read in file 
# All variables
GIS_Landuse_master <- read_excel("GIS_Landuse_master.xlsx", 
                                 sheet = "Sheet5")
# 1362x23

# After correlation matrix - Developed was combined
GIS_Landuse_master <- read_excel("GIS_Landuse_master.xlsx", 
                                 sheet = "Developed")
datatable <- GIS_Landuse_master
glimpse(datatable)
view(datatable)
# 1362x20

# factor Colony Name, Substrate, Year
#  SPECIFY SUBSTRATE/YEAR/COLONYNAME AS FACTOR VARS, INDICATE BASE FOR SUBSTRATE AND YEAR
datatable$COLONYNAME <- as.factor(datatable$COLONYNAME)
datatable$Substrate <- as.factor(datatable$Substrate)
datatable$Year <- as.factor(datatable$Year)
is.factor(datatable$Substrate)

## COORDINATES
colcoords <- read_excel("F:\nlcd_land_cover_l48_20210604\ColonyData\NLCD_Coordinates.xlsx",
                        sheet = "sheet1")
#unique rows only
datatable <- unique(colcoords)

#view table - if replicate coords exist then
# keep only unique coordinates & the related name
datatable <- unique(colcoords[,1])

#create correlation matrix of predictor vars
install.packages("corrplot")
library("corrplot")
#define variables
corrvars <- colnames(datatable[,5:19])
print(corrvars)
data <- datatable[,5:19]


#"Open_Water", "Developed", "Barren_Land", "Deciduous_Forest","Evergreen_Forest", "Mixed_Forest", "Shrub_Scrub", "Herbaceous", "Hay_Pasture", "Cultivated_Crops", "Wetlands")]
#corelation matrix
M2 <- cor(data)
head(round(M,2))

#visualizing correlogram
#as circle
corrplot(M2, method="circle", method="pie", method="color", method="number")
# upper triangular matrix
corrplot(M2, method= "number", type="upper")

# reordering
# correlogram with hclust reordering
corrplot(M2, type = "upper", order = "hclust")
corrplot(M, method="number", type = "upper", order = "hclust")

## customize the correlogram
# Using different color spectrum
# changing colour of the correlogram
library(RColorBrewer)
library(corrplot)
col <- colorRampPalette(c("#BB4444", "#EE9988", 
                          "#FFFFFF", "#77AADD",
                          "#4477AA"))
corrplot(M2, type="upper", order = "hclust", col = col(200))


col<- colorRampPalette(c("red", "white", "blue"))(20)

corrplot(M, type="upper", order = "hclust", col = col)

corrplot(M, type="upper", order = "hclust", 
         col=brewer.pal(n = 8, name = "RdBu"))

## Correlation matrix with pvalues
# mat : is a matrix of data
# ... : further arguments to pass 
# to the native R cor.test function
cor.mtest <- function(mat, ...) 
{
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) 
  {
    for (j in (i + 1):n)
    {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

# matrix of the p-value of the correlation
p.mat <- cor.mtest(data)
head(p.mat[, 1:5])
# Specialized the insignificant value
# according to the significant level
corrplot(M2, type = "upper", order = "hclust", col = col(200),
         tl.col="black", tl.srt = 45, #text labels, with rotation
                  p.mat = p.mat, sig.level = 0.01)

# Leave blank on no significant coefficient
corrplot(M2, type = "upper", order = "hclust", col = col(200), method = 'color',
         tl.col="black", tl.srt = 45, #text labels, with rotation
         p.mat = p.mat, 
         addCoef.col = 'black', number.cex = .8, tl.cex = .8,
         insig = "blank") # significane levels

## add significant level stars
corrplot(M2, p.mat = p.mat, method = 'color', diag = FALSE, type = 'upper', 
         col = col(200), tl.col="black", tl.srt = 45, #text labels, with rotation
         sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.8,
         insig = 'label_sig', pch.col = 'grey20', order = 'hclust',
         tl.cex = 0.8)


## add significant level stars
corrplot(M, p.mat = p.mat, method = 'color', diag = FALSE, type = 'upper',
         sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
         insig = 'label_sig', pch.col = 'grey20', order = 'hclust',
         tl.col="black", tl.srt = 45, # Text label color and rotation
         tl.cex =0.8
         )
## Perform needed combinations, variable reduction to minimilaize correlation
# I saved dataset and used excel to combine all developed cats with simple 
#  addition. Go back to the top of script and change datatable to refelct
#Normality check
#histogram
hist(datatable$Size)
hist(datatable$Open_Water)
hist(datatable$Developed_combined)
hist(datatable$Barren_Land)
hist(datatable$Deciduous_Forest)
hist(datatable$Evergreen_Forest)
hist(datatable$Mixed_Forest)
hist(datatable$Shrub_Scrub)
hist(datatable$Herbaceous)  # ONLY ONE W/NORMAL DISTRIBUTION
hist(datatable$Hay_Pasture)
hist(datatable$Cultivated_Crops)
hist(datatable$Wetlands)
hist(datatable$Emergent_Herbaceous_Wetlands)

#Other checks for normality
#density plot
ggdensity(datatable$Size, fill = "lightgray")
#QQ plot
ggqqplot(datatable$Size)
#

## Check linearity
plot(Size ~ D, data = datatable)

cor(datatable$Size, datatable$D)


#################
# MODELTRAINING #
###################

glimpse(datatable)
#Remove colony ID
features <- colnames(datatable[-2])
df <- datatable[,features] 
columns <- colnames(df)

glimpse(features)
glimpse(df)
# mtry = 1/3 *p
mtry_r =(ncol(df)/3)
mtry_r

# Set seed for reproducibility 
set.seed(1273)

#split dataset into training (70%) testing (30%)
train_index <- createDataPartition(y=df$Size, p=0.8, list=FALSE)

#subset data
training_set <- df[train_index, ]
testing_set <- df[-train_index, ]


#https://www.guru99.com/r-random-forest-tutorial.html


#k-validation
#repeated cv with 5 folds, 3 repeats
repeat_cv <- trainControl(method ="repeatedcv", 
                          number=10,repeats=3, 
                          search = "grid")
trControl <- trainControl(method = "repeatedcv", 
                          number=10, repeats=3,
                          search="grid" #random search
)


# use caret to evaluate model

#search for best mtry 
#A large mtry ensures (with high probability) at least one strong variable in the set of mtry candidate variables
# but may mask moderate varaibles - choose lower for when vars arent strong
# Construct vector with value 1-10

tuneGrid <- expand.grid(.mtry = c(3: 10))  
rfmtry <- train(Size ~., 
                data = training_set,
                methods='rf',
                #add repeated cv
                #accuracy measurement
                metric='RMSE',
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 5,
                ntree = 301)
print(rfmtry)

# best value of mtry
rfmtry$bestTune$mtry
max(rfmtry$results$RMSE)
best_mtry <-  rfmtry$bestTune$mtry
best_mtry

 ##Find best maxnodes

#create list where model results are stored
store_maxnode <- list()
#use best mtry
tuneGrid <- expand.grid(.mtry = best_mtry)
#loop to evaluate maxnode values 5-15
for (maxnodes in c(20: 35)) {
  rf_maxnode <- train(Size~.,
                      data = training_set,
                      method = "rf",
                      metric = "RMSE",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 4,
                      maxnodes = maxnodes,
                      ntree = 301)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
print(rf_maxnode)
results_mAX <- resamples(store_maxnode)
summary(results_mAX)

 ## 32 - 332.0108
 ## 31 - 328

#node size - larger value is more general tree - of 1 = matched but overfitt
### trying node size values
store_nodesize <- list()
#use best mtry
tuneGrid <- expand.grid(.mtry = best_mtry)

for (nodesize in c(3: 18)) {
  rf_nodesize <- train(Size~.,
                      data = training_set,
                      method = "rf",
                      metric = "RMSE",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = nodesize,
                      maxnodes = 31,
                      ntree = 301)
  key <- toString(nodesize)
  store_nodesize[[key]] <- rf_nodesize
}
print(rf_nodesize)
results_nodesize <- resamples(store_nodesize)
summary(results_nodesize)
#7

## Best number of trees
set.seed(61273)
store_maxtrees <-   list()
for (ntree in c(151, 251, 301, 351, 401, 451, 501, 551, 601, 801, 1001, 1501)) {
  set.seed(61273)
  rf_maxtrees <- train(Size~.,
                       data = training_set,
                       method = "rf",
                       metric = "RMSE",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance =  TRUE,
                       nodesize = 7,
                       maxnodes = 31,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

print(rf_maxnode)
print(rf_default$results)
print(rf_maxtrees)


## Now model is tuned and we can train it!
# Train random forest with new params

#fit_rf <- train(Size ~.,
#                training_set,
#                method = "rf",
#                metric = "RMSE",
#                tuneGrid = tuneGrid,
#                trControl = trControl,
#                importance = TRUE,
#                nodesize = 6,
#                ntree = 800,
#                maxnodes = 14)

fit_rf <- train(Size ~.,
                training_set,
                method = "rf",
                metric = "RMSE",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 7,
                ntree = 1001,
                maxnodes = 31)

fit_rf$results 

# Performance on Testing Data

#Generate Predictions w/ testing set
y_predict <- predict(
  object=fit_rf,
  newdata=testing_set[, -1]  # data for predictions, no Size
)

# compare actual values of testing set with ones predicted from model

##calculate RMSE of models
RMSE_2 <- rmse(testing_set$Size, y_predict)
corr_2 <- cor(testing_set$Size, y_predict) ^ 2

RMSE_2
corr_2

RMSE_1 <- rmse(testing_set$Size, y_predict)
corr_1 <- cor(testing_set$Size, y_predict) ^ 2


# https://www.guru99.com/r-random-forest-tutorial.html





### Check variable importance 

# trained model
fit_rf
#tested model
testrf <- randomForest(Size ~.,
                       testing_set,
                       metric = "RMSE",
                       importance = TRUE,
                       mtry = best_mtry,
                       nodesize = 7,
                       ntree = 1001,
                       maxnodes = 31)

testrf 

#plot importance - %MEI
p2 <- create_rfplot(testrf, type = 1)
p2
testrf$importance

# plot test MSE based on # of trees used
plot(testrf)

# plot with importance of each predictor var 
varImpPlot(testrf, type = 1)
#x-axis= avg.incr in node purity of regression trees
#y-axis=splitting of various predictors
#Variable Importance
varImpPlot(testrf,
           sort = T,
        #  n.var = 10,
           type = 1,
           main = "Variable Importance")


# Get variable importance from the model fit
ImpData <- as.data.frame(importance(testrf))
ImpData$Var.Names <- row.names(ImpData)

ggplot(ImpData, aes(x=reorder(Var.Names, `%IncMSE`), y=`%IncMSE`)) +
  geom_segment( aes(x=reorder(Var.Names, `%IncMSE`), xend=reorder(Var.Names, `%IncMSE`), y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(), 
    axis.ticks.y = element_blank()
  )


hist(treesize(testrf),
     main = "No. of Nodes for the Trees",
     col = "green")




###### ETYYYYYYYYY
#############################################################        ###########
##############################################################################
#############################################################################
#####################





#default model tuning

model_tuned <- tuneRF(
  x=df[,-1], #define predictor variables
  y=df$Size, #define response variable
  ntreeTry=800,
  mtryStart=3, 
  stepFactor=3,
  improve=0.001,
  importance = TRUE,
  trace=FALSE #don't show real-time progress
)
tuneRF
y_predict <- predict(
  object=model_tuned,
  newdata=testing_set[, -1]  # data for predictions, no Size
)

install.packages("Metrics")
library(Metrics)
##calculate RMSE of models
rmse(testing_set$Size, y_predict)
cor(testing_set$Size, y_predict) ^ 2

# Variable Importance Goes Here !


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
t <- tuneRF(training_set[,-1], training_set[,1],
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

# plottest MSE based on # of trees used
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
#Final Model
testrf
#
### Type 1 - Mean decrease in MSE by **Permutation
# without random column

# random forest
rf1 <- randomForest(Size ~ ., data = df[, 1:18], mtry=11,
                    nodesize = 8, maxnodes = 23, ntree = 800, importance=T)
#feature importance
importance(rf1, scale=F)
#
p1 <- create_rfplot(rf1, type = 1)  # type 1 - importances, regression
#ggsave('../article/images/regr_permute_R.svg',plot = p1, device = 'svg', height = 4, width = 6)
p1

# with random column

rf2 <- randomForest(Size~., data = df, mtry=11,
                    nodesize = 8, maxnodes = 23, ntree = 800, importance=T)
importance(rf2, scale=F)
p2 <- create_rfplot(rf2, type = 1)
#ggsave('../article/images/regr_permute_random_R.svg',
p2
#plot = p2, device = 'svg', height = 4, width = 6)
imp1 <- data.frame(importance(rf2, type = 1, scale=F))
imp1
write.csv(imp1, file="./imp_R_regr_MSE.csv")
c

#########################
## DROP-COLUMN IMPORTANCE
###############################

## Examine Costs by Drop-Column Importance

# PARAMS ######## use tuned params: ntree = 40, mtry = 2, nodesize = 1

get_drop_imp <- function(df, columns){
  X <- df[,c(columns, 'Size')] # data
  rf3 <- randomForest(Size ~., data = X, mtry=8,
                     nodesize = 8, maxnodes = 22, ntree = 1000, importance=T)
  full_rsq <- mean(rf3$rsq) # R-squared
  imp <- c()
  for (c in columns){
    X_sub <- X[, !(colnames(X) == c)]
    rf3 <- randomForest(Size~., data = X_sub, mtry=8,
                       nodesize = 8, maxnodes = 22, ntree = 1000, importance=T)
    sub_rsq <- mean(rf3$rsq) # R-squared
    diff_rsq <- full_rsq - sub_rsq
    imp <- c(imp, diff_rsq)
  }
  featureImportance <- data.frame(Feature=columns, Importance=imp)
  return(featureImportance)
}
rf3
columns <- colnames(df[2:18])

get_drop_imp
featureImportance <- get_drop_imp(df, columns)

rf3

ptuned <- create_ggplot(featureImportance)
ptuned

## R2 value
full_rsq

ggsave('../article/images/regr_drop_R.svg',
plot = ptuned, device = 'svg', height = 4, width = 6)

columns <- colnames(df[2:19])
featureImportance <- get_drop_imp(df, columns)
p2 <- create_ggplot(featureImportance)
#ggsave('../article/images/regr_drop_random_R.svg',
#plot = p2, device = 'svg', height = 4, width = 6)

write.csv(featureImportance, file="./imp_regr_drop.csv")


featureImportance <- get_drop_imp(df, columns)
p2 <- create_ggplot(featureImportance)
#ggsave('../article/images/regr_drop_random_R.svg',
#plot = p2, device = 'svg', height = 4, width = 6)

write.csv




########################
# VAR IMPORTANCE - Compare Model Builds Importance, RMSE, Ranking, Save to Interpret
###########################
# choose model params - node, max tree, mtry
# Check performance of those params by comparing a test/train set to look at accuracy
# Then use those params in determining Feature Importance
# With Feature Importance, able to begin writing results
#
#

###### Compare Model Builds

# 

#default model tuning

model_tuned <- tuneRF(
  x=df[,-1], #define predictor variables
  y=df$Size, #define response variable
  ntreeTry=800,
  mtryStart=3, 
  stepFactor=0.5,
  improve=0.00001,
  trace=FALSE #don't show real-time progress
)
tunerf <- randomForest(Size ~.,
                       testing_set,
                       method = "rf",
                       metric = "RMSE",
                       mtry = 8,
                       importance = TRUE,
                       ntree = 800,
                   )
tunerf_RMSE_VAR <-  c("104055.1", "58.82")

#mtry = 6
# Get variable importance from the model fit
varImp(model_tuned)
ImpData <- importance(model_tuned)
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


# Output to be present as PNG file 
png(file = "randomForestRegression.png")

# Plot the error vs the number of trees graph
plot(ozone.rf)

# Saving the file
dev.off()

# No. of Nodes
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")

# Partial Dependence Plot
partialPlot(rf, train, Petal.Width, "setosa")

# Multi-dimensional Scaling Plot of Proximity Matrix
MDSplot(rf, train$Species)

#https://www.r-bloggers.com/2021/04/random-forest-in-r/

#############
# Tuned Model with cross-validation - tuneGrid
##
# default model 

# use caret to evaluate model
rf_default <- train(Size ~.,
                    data = training_set,
                    method = "rf",
                    metric="RMSE",
                    trControl = trControl)

print(rf_default)

### after cross-validation
tunedr <- randomForest(Size ~.,
                       training_set,
                       method = "rf",
                       metric = "RMSE",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 8,
                       ntree = 800,
                       maxnodes = 23)
print(tunedr)
tunedr

tunedr_RMSE_VAR <-  c("135671.4", "46.31")

tunedimp <- importance(tunedr, type=1, scale=F) # doesnt return / se

tunedGrid_rf <- varImpPlot(tunedr,
           sort = TRUE,# SORTING ORDER
           main = "Variable Importance using Tunedr")

glimpse(tunedimp)


## Column-Drop Importance -  uses tunedr
df
columns <- colnames(df[2:18])
tunedr_RMSE_VAR <-  c("135671.4", "46.31")

#uses column drop
featureImportance <- get_drop_imp(df, columns)

ptuned <- create_ggplot(featureImportance)

ptuned
 














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

# Partial Dependence Plot
partialPlot(rf, train, Petal.Width, "setosa")

# Multi-dimensional Scaling Plot of Proximity Matrix
MDSplot(rf, train$Species)

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
# Multi-dimensional Scaling Plot of Proximity Matrix
MDSplot(rf, train$Species)

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


# No. of Nodes
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")

# Partial Dependence Plot
partialPlot(rf, train, Petal.Width, "setosa")

# Multi-dimensional Scaling Plot of Proximity Matrix
MDSplot(rf, train$Species)

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