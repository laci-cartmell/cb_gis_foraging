####
install.packages("beepr")
library("beepr")
beep(03)

install.packages("Metrics")
install.packages("randomForest")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("cowplot")
install.packages("e1071")
install.packages('caret')
install.packages("dplyr")
library(dplyr)
library(randomForest)
library(datasets)
library(caret) # confusion Matrix
library(tidyverse)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(e1071)
library(readxl)
library(Metrics)


# install the development version of vivid:
install.packages("devtools")
install.packages("ISLR")
install.packages("mlr3")
install.packages("randomForest")
install.packages("condvis2")
install.packages("kknn")
install.packages("mlr3learners")
install.packages('sna')

library(devtools)
devtools::install_github("AlanInglis/vivid")

# Load relevant packages:
if(!require(network)){
  install.packages("network")
}
if(!require(sp)){
  install.packages("sp")
}
if (!requireNamespace("graph", quietly = TRUE)){
  install.packages("BiocManager")
  BiocManager::install("graph")
}
install.packages("zenplots")
install.packages("ranger")
library(ranger)
library("vivid") # for visualisations
library("ISLR") # for data
library("mlr3") # to create model
library("mlr3learners") # to create model
library("randomForest") # to create model
library("condvis2") # for predict function
library("kknn") # to get knn model
library("dplyr")
library("sna")

# Load data:
collegeData <- College
View(collegeData)
# Taking log values of skewed data:
collegeData <- collegeData %>%
  mutate(log(collegeData[,c(2:4,7:12)]))

# Load Data
library(readxl)
setwd("~/R/R_Class/cb_gis_foraging")

GIS_Landuse_master <- read_excel("GIS_Landuse_master.xlsx", 
                                 sheet = "Developed")
datatable <- GIS_Landuse_master
datatable$COLONYNAME <- as.factor(datatable$COLONYNAME)
datatable$Substrate <- as.factor(datatable$Substrate)
datatable$Year <- as.factor(datatable$Year)

#Check for NaN or Inf and replace with NA if present
#is all a variable NA
all(is.na(datatable$Size))
#Replace NaN & Inf with NA
datatable[is.na(datatable) | datatable=="-Inf"] = NA
View(datatable)
#Remove colony ID
datatable <- datatable[-3]
features <- colnames(datatable[2:18])
df <- datatable[,features] 
columns <- colnames(df)


# Set seed for reproducibility 
set.seed(1273)

#split dataset into training (80%) testing (20%) based on location so that if only 1 location available then goes to sample
train_index <- createDataPartition(y=df$Size, p=0.8, list=FALSE)
#subset data
training_set <- df[train_index, ]
testing_set <- df[-train_index, ]
xTest <- testing_set[,-1]  #remove size
yTest <- testing_set$Size

#Model fitting:

#Fit a random forest an k-nearest neighbor models
set.seed(1273)
install.packages("gridExtra")
library(caret)

#k-validation
#repeated cv with 5 folds, 3 repeats
repeat_cv <- trainControl(method ="repeatedcv", 
                          number=10,repeats=3, 
                          search = "grid")
trControl <- trainControl(method = "repeatedcv", 
                          number=10, repeats=3,
                          search="grid" #random search
)
best_mtry = 6
tuneGrid <- expand.grid(.mtry = best_mtry)

fit_rf <- train(Size ~.,
                training_set,
                method = "rf",
                metric = "RMSE",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 7,
                ntree = 1001,
                maxnodes = 31,
                proximity=TRUE, 
)


fit_rf$results 


#Generate Predictions w/ testing set
y_predict <- predict(
  object=fit_rf,
  newdata=testing_set[, -1]  # data for predictions, no Size
)

# compare actual values of testing set with ones predicted from model

##calculate RMSE of models
RMSE_2 <- rmse(testing_set$Size, y_predict)
corr_2 <- cor(testing_set$Size, y_predict) ^ 2
RMSE_2 #379.624
corr_2 # r^2 0.7085
##

## Fit a random forest model
## Used throughout Section 2:

## Check mse for rf model:
predRf <- predict(fit_rf, newdata = testing_set)
mspeRf <- mean((yTest - predRf)^2)
Rsq <- 1 - sum((yTest - predRf)^2)/sum((yTest - mean(yTest))^2)
Rsq # 0.554
mspeRf # 379.624

# Fit an mlr3 knn model
# Used in Section 2.3:
knnT <- TaskRegr$new(id = "knn", backend = training_set, target = "Size")
knnL <- lrn("regr.kknn")
knnMod <- knnL$train(knnT)


# Check mse for knn model:
pred <- predict(knnMod, newdata = testing_set)
mspe <- mean((yTest - pred)^2)
mspe



## Create vivid matrix 

# Create unsorted vivid matrix for random forest fit:

# Used for Figure 1(a):
set.seed(1273)
# Specify importance type
vividMatrixRF <- vivi(fit_rf,
                      data = training_set,
                      response = "Size",
                      importanceType = "%IncMSE",
                      gridSize = 20, 
                      reorder = FALSE,
                      )

# Sort matrix:
# Used for Figure 1(b):
vividMatrixRFSorted <- vividReorder(vividMatrixRF)

# May switch embedded rf imp values with "%IncMSE"

# Get agnostic VImp values instead of using random forests embedded VImps
# Used for Figure 2(b):
sizeVImps <- vivid:::vividImportance.default(fit_rf,
                                                training_set,
                                                "Size",
                                                importanceType = "%IncMSE",
                                                predictFun = CVpredict # returns vector of predictions
                                             )

# Update the matrix with the new VImp values and sort:
vividMatrixRFSorted_1 <- viviUpdate(vividMatrixRFSorted, sizeVImps)
vividMatrixRFSorted_1 <- vividReorder(vividMatrixRFSorted_1)

# # Create vivid matrix for mlr3 knn fit using agnostic VImp
# Used for Figure 2(a):
set.seed(1273)
knnMat <- vivi(
  fit = knnMod,
  data = training_set,
  response = "Size",
  gridSize = 20,
  importanceType = "%IncMSE"
)



# Visualisations for Section 2 --------------------------------------------
#### 
  
# Figure 1(a):
viviHeatmap(vividMatrixRF, 
            angle = 45 # rotate the x=-axis label
              ) # unsorted heatmap
# Figure 1(b):
viviHeatmap(vividMatrixRFSorted, angle = 45) # sorted heatmap


## Figure 2(a) & 2(b):
# 
# # Figure 2(a): # knn dont need to compare
# viviHeatmap(knnMat, angle = 45, impLims = c(0, 0.6), intLims = c(0, 0.08)) # setting same VImp limits as Figure 2(b)

# Figure 2(b)
viviHeatmap(vividMatrixRFSorted_1,
            angle = 45, 
            impLims = c(30, 120), 
            intLims = c(12, 30)) # agnostic VImp measures

## Figure 3(a) & 3(b):

# Figure 3(a)
install.packages("intergraph")
library(intergraph)
viviNetwork(vividMatrixRFSorted)
# Figure 3(b)
intVals <- as.dist(vividMatrixRFSorted)
intVals <- as.matrix(intVals)

sv <- which(diag(vividMatrixRFSorted) > 50 |apply(intVals, 1,max) > .01)
h <- hclust(-as.dist(vividMatrixRFSorted[sv,sv]),
            method="single")
#network plot
viviNetwork(vividMatrixRFSorted[sv,sv], 
            intThreshold = 0.01, 
            removeNode = T, 
            cluster = cutree(h,6))  #cuts clustered data into a desired number of groups


set.seed(1701)
# clustered and filtered network for rf
intVals <- vividMatrixRF
diag(intVals) <- NA 


# select VIVI values in top 20%
impTresh <- quantile(diag(vividMatrixRF),.8)
intThresh <- quantile(intVals,.8,na.rm=TRUE)
sv <- which(diag(vividMatrixRF) > impTresh |
              apply(intVals, 1, max, na.rm=TRUE) > intThresh)

h <- hclust(-as.dist(vividMatrixRF[sv,sv]), method="single")

viviNetwork(vividMatrixRF[sv,sv],
            cluster = cutree(h, k = 3), # specify number of groups
            layout = igraph::layout_as_star)


# Visualisation for Section 3.2 ---------------------------------------------

## Figure 4:
#onstructs a grid of univariate PDPs with ICE curves for selected variables.
## Used for Figure 2(b):
sizeVImps <- vivid:::vividImportance.default(fit_rf,
                                             training_set,
                                             "Size",
                                             importanceType = "%IncMSE",
                                             predictFun = CVpredict # returns vector of predictions
)

# Update the matrix with the new VImp values and sort:
vividMatrixRFSorted_1 <- viviUpdate(vividMatrixRFSorted, sizeVImps)
vividMatrixRFSorted_1 <- vividReorder(vividMatrixRFSorted_1)

top5 <- colnames(vividMatrixRFSorted_1)[1:5]
pdpVars(data = training_set,
        fit = fit_rf,
        response = "Size",
        vars = top5,
        nIce = 100)


# Filter matrix:0
nam <- colnames(vividMatrixRFSorted) # get names
nam <- nam[1:7] # filter names

# Create GPDP for Figure 4:
pdpPairs(training_set,
         fit_rf, "Size",
         gridSize = 20,
         vars = nam,
         convexHull = TRUE
)




# Visualisation for Section 3.3 ---------------------------------------------


install.packages("zenplots")
install.packages("Bioconductor")
library(Bioconductor)
library(zenplots)
# Calculate the zpath using same threshold as Figure 3(b):
zpath <- zPath(vividMatrixRFSorted, 0.01)

# Create ZPDP using zpath for Figure 5:
set.seed(101)
pdpZen(training_set,
       fit_rf,
       "Size",
       nmax = 500,
       gridSize = 20,
       zpath = zpath,
       convexHull = T
)

