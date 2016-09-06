# http://topepo.github.io/caret/training.html#custom
# http://topepo.github.io/caret/training.html
# http://topepo.github.io/caret/random.html

require(mlbench)
require(caret)
require(xgboost)
require(gbm)

data_explore=data[tst==0,]

# And we just sample to have a managable working set
data_sample=data_explore[sample(nrow(data_explore),120000),]

data_sample$tst<-NULL
data_sample$id<-NULL

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

# xgbGrid <- expand.grid(n.trees = 150, )

set.seed(825)
xgbTreeFit <- train(Demanda_uni_equil ~ ., data = data_sample,
                method = "xgbTree",
                trControl = fitControl,
                ## This last option is actually one
                ## for xgb() that passes through
                verbose = FALSE,
                na.action = na.pass)
xgbTreeFit
gc()

# remove(data_sample)
