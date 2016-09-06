### https://www.r-bloggers.com/r-setup-a-grid-search-for-xgboost/

library(caret)
library(magrittr)
library(dplyr)
require(data.table)
require(Matrix)
require(xgboost)

data_train=data[tst==0,]
data_test=data[tst==1,]

searchGridSubCol <- expand.grid(subsample = c(0.5, 0.75, 1),
                                colsample_bytree = c(0.6, 0.8, 1),
                                max_depth = c(6,7,8,9,10,11,12,13))
ntrees <- 100

# And we just sample to have a managable working set
data_sample=data_train[sample(nrow(data_train),10),] #391000),]

sparse_matrix<- sparse.model.matrix(Demanda_uni_equil~., data = data_sample)

#Build a xgb.DMatrix object
# DMMatrixTrain <- xgb.DMatrix(data = sparse_matrix, label = Demanda_uni_equil)

rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){

  #Extract Parameters to test
  currentSubsampleRate <- parameterList[["subsample"]]
  currentColsampleRate <- parameterList[["colsample_bytree"]]
  currentMaxDepth <- parameterList[["max_depth"]]

  xgboostModelCV <- xgb.cv(data =  sparse_matrix, label = data_sample$Demanda_uni_equil,nrounds = ntrees, nfold = 5, showsd = TRUE,
                           metrics = "rmse", verbose = TRUE, "eval_metric" = "rmse",
                           "objective" = "reg:linear", "max.depth" = 15, "eta" = 2/ntrees,
                           "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate,
                           "currentMaxDepth" = currentMaxDepth)

  xvalidationScores <- as.data.frame(xgboostModelCV)
  #Save rmse of the last iteration
  rmse <- tail(xvalidationScores$test.rmse.mean, 1)

  return(c(rmse, currentSubsampleRate, currentColsampleRate, currentMaxDepth))
})
currentMaxDepth

plot(rmseErrorsHyperparameters)
