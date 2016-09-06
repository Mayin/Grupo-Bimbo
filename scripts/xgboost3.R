# Setting working dir env
setwd("~/workspace/Bimbo/csv")

require(xgboost)
require(data.table)
require(plyr)

# Only bring in attributes we are going to use
train=fread('train.csv', select = c("Semana", 'Cliente_ID', 'Producto_ID', 'Agencia_ID', 'Ruta_SAK', 'Demanda_uni_equil'),
            header = T, stringsAsFactors = F)
test=fread('test.csv', select   = c("Semana", 'Cliente_ID', 'Producto_ID', 'Agencia_ID', 'Ruta_SAK', 'id'),
           header = T, stringsAsFactors = F)

town_state=fread('town_state.csv', select = c('Agencia_ID', 'Town', 'State'),
           header = T, stringsAsFactors = F)
town_state <- transform(town_state, Town_ID = as.numeric(interaction(Town, drop = T)))
town_state$Town <- NULL
town_state <- transform(town_state, State_ID = as.numeric(interaction(State, drop = T)))
town_state$State <- NULL

# A bit about weeks
# unique(train$Semana)
# unique(test$Semana)

# Gives 7 weeks of train data (3,4,5,6,7,8,9) and 2 weeks to predict (10,11)
# > 6 gives 2 weeks of lag available to compute (9 to 8 to 7)
# 7 gives     8,9 - one week
# 6 gives   7,8,9 - two weeks
# 5 gives 6,7,8,9 - three weeks
train=train[Semana>5,]

# Adding id to train
train$id=0
# Adding tst (not predicting on train set)
train[,tst:=0]

# Adding target to test
test$Demanda_uni_equil=0
# Adding tst to test (yes predicting on test set)
test[,tst:=1]

data=rbind(train,test)
remove(test)
remove(train)
gc()

data=merge(data,town_state, by = "Agencia_ID")
remove(town_state)
gc()

# Town_state
ClientsByTown <- ddply(data, ~ Town_ID,summarise,NCliente_IDByTown_ID=length(unique(Cliente_ID)))
data=merge(data,ClientsByTown, by = "Town_ID")
remove(ClientsByTown)
gc()
# Ruta
ClientsByRuta_SAK <- ddply(data, ~ Ruta_SAK,summarise,NCliente_IDByRuta_SAK=length(unique(Cliente_ID)))
data=merge(data,ClientsByRuta_SAK, by = "Ruta_SAK")
remove(ClientsByRuta_SAK)
gc()
# Three Week Lag
data3<-data[,.(Semana=Semana+3,Cliente_ID,Producto_ID,Demanda_uni_equil)]
data=merge(data,data3[Semana>8,.(lag3week=mean(Demanda_uni_equil)), by=.(Semana,Cliente_ID,Producto_ID)],all.x=T, by=c("Semana","Cliente_ID","Producto_ID"))
data=data[Semana>6,]
remove(data3)
gc()
# Two Week Lag
data2<-data[,.(Semana=Semana+2,Cliente_ID,Producto_ID,Demanda_uni_equil)]
data=merge(data,data2[Semana>8,.(lag2week=mean(Demanda_uni_equil)), by=.(Semana,Cliente_ID,Producto_ID)],all.x=T, by=c("Semana","Cliente_ID","Producto_ID"))
data=data[Semana>7,]
remove(data2)
gc()
# One Week Lag
data1<-data[,.(Semana=Semana+1,Cliente_ID,Producto_ID,Demanda_uni_equil)]
data=merge(data,data1[Semana>8,.(lag1week=mean(Demanda_uni_equil)), by=.(Semana,Cliente_ID,Producto_ID)],all.x=T, by=c("Semana","Cliente_ID","Producto_ID"))
data=data[Semana>8,]
remove(data1)
gc()

# Creating frequency features for all factor variables
nAgencia_ID=data[,.(nAgencia_ID=.N),by=.(Agencia_ID,Semana)]
nAgencia_ID=nAgencia_ID[,.(nAgencia_ID=mean(nAgencia_ID,na.rm=T)),by=Agencia_ID]
data=merge(data,nAgencia_ID,by='Agencia_ID',all.x=T)
remove(nAgencia_ID)
gc()
nRuta_SAK=data[,.(nRuta_SAK=.N),by=.(Ruta_SAK,Semana)]
nRuta_SAK=nRuta_SAK[,.(nRuta_SAK=mean(nRuta_SAK,na.rm=T)),by=Ruta_SAK]
data=merge(data,nRuta_SAK,by='Ruta_SAK',all.x=T)
remove(nRuta_SAK)
gc()
nCliente_ID=data[,.(nCliente_ID=.N),by=.(Cliente_ID,Semana)]
nCliente_ID=nCliente_ID[,.(nCliente_ID=mean(nCliente_ID,na.rm=T)),by=Cliente_ID]
data=merge(data,nCliente_ID,by='Cliente_ID',all.x=T)
remove(nCliente_ID)
gc()
nProducto_ID=data[,.(nProducto_ID=.N),by=.(Producto_ID,Semana)]
nProducto_ID=nProducto_ID[,.(nProducto_ID=mean(nProducto_ID,na.rm=T)),by=Producto_ID]
data=merge(data,nProducto_ID,by='Producto_ID',all.x=T)
remove(nProducto_ID)
gc()
data$Demanda_uni_equil=log(data$Demanda_uni_equil+1)

data_train=data[tst==0,]
data_test=data[tst==1,]
# remove(data)
gc()

set.seed(825)
wltst=sample(nrow(data_train),120000)

# #####################
# Up to here we can just save and restart from
write.csv(data, file = "full_data.csv")
write.csv(wltst, file = "full_wltst.csv")
# #####################

features=names(data_train)[!(names(data_train) %in% c('id',"Demanda_uni_equil",'tst'))]

dval<-xgb.DMatrix(data=data.matrix(data_train[wltst,features,with=FALSE]),
                  label=data.matrix(data_train[wltst,Demanda_uni_equil]),missing=NA)
watchlist<-list(dval=dval)
gc()
# saved
param <- list(  objective="reg:linear",
                booster = "gbtree",
                eta= 0.2,
                max_depth=13,
                subsample= 0.85,
                colsample_bytree=0.59,
                min_child_weight= 0.94
            )

date()
clf <- xgb.train(params = param ,
                 data = xgb.DMatrix(data=data.matrix(data_train[-wltst,features,with=FALSE]),
                                    label = data.matrix(data_train[-wltst,Demanda_uni_equil]),missing=NA),
                 nrounds               = 150,
                 nthread               = 4,
                 verbose               = 1,
                 print_every_n         = 5,
                 early_stopping_rounds = 3,
                 watchlist             = watchlist,
                 maximize              = FALSE
                )
gc()
date()

# Make prediction for the 10th week
data_test1=data_test[Semana==10,]
pred<-predict(clf,xgb.DMatrix(data.matrix(data_test1[,features,with=FALSE]),missing=NA))
res=exp(round(pred,5))-1

# Create lagged values of target variable which will be used as a feature for the 11th week prediction
data_test_lag1=data_test1[,.(Cliente_ID,Producto_ID)]
data_test_lag1$targetl1=res
data_test_lag1=data_test_lag1[,.(targetl1=mean(targetl1)), by=.(Cliente_ID,Producto_ID)]

results=data.frame(id=data_test1$id,Demanda_uni_equil=res)

data_test2=data_test[Semana==11,]
data_test2[,targetl1:=NULL]

# Merge lagged values of target variable to test the set for the 11th week
data_test2=merge(data_test2,data_test_lag1,all.x=T,by=c('Cliente_ID','Producto_ID'))
pred<-predict(clf,xgb.DMatrix(data.matrix(data_test2[,features,with=FALSE]),missing=NA))
res=exp(round(pred,5))-1
res.df=data.frame(id=data_test2$id,Demanda_uni_equil=res)
results=rbind(results, res.df)

results[results[,2]<0,2]=0
results[,2]=round(results[,2],1)
results[,1]=as.integer(results[,1])
class(results[,1])='int32'
options(digits=18)
write.csv(results,file='120k-0.2-13-0.85-0.59-0.94-150.csv',row.names=F)
