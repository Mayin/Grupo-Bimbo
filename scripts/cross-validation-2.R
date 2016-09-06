# Setting working dir env, use one
setwd("~/workspace/Bimbo")                 # dropbox
# setwd("C:/Users/mtalaver/workspace/Bimbo") # local

require(data.table)
require(Matrix)
require(xgboost)

data=fread('csv/1-getting-data.csv', header = T)

# Drop V1 put on by export to file
data[,V1:=NULL]

# Remember we need to examine train set... the set with answer
data_train=data[tst==0,]
data_test=data[tst==1,]

# And we just sample to have a managable working set
data_sample=data_train[sample(nrow(data_train),1000000),] #391000),]

sparse_matrix<- sparse.model.matrix(Demanda_uni_equil~., data = data_sample)
gc()

set.seed(1234)

params_cv = list(objective   = "reg:linear",
                 eta         = 0.3,
                 max.depth   = 10,
                 eval_metric = "rmse")

# Lets figure out how many iterations to use to build our model
bst.cv <- xgb.cv(params           = params_cv,
                 data             = sparse_matrix,
                 label            = data_sample$Demanda_uni_equil,
                 missing          = 0,
                 nfold            = 5,
                 nround           = 3, #000,
                 early.stop.round = 5,
                 maximize         = F)

remove(data_sample)
gc()
# eta Best w/ 1m sample, 5 early stop, 5 nfold, rmse
# 0.3 543, 204, 278
# 0.2 479, 582, 589
# 0.1 1053, 1176
print(bst.cv[0:81])
plot(bst.cv$train.rmse,bst.cv$test.rmse)
# Can we delete the following?
# remove(bst.cv)
# remove(train_sample)
# remove(params_cv)
# remove(sparse_matrix)

# write.csv(data, file = "csv/2-cross-validation.csv")
