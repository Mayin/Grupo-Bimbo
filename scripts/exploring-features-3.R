# Setting working dir env, use one
setwd("~/workspace/Bimbo")                 # dropbox
# setwd("C:/Users/mtalaver/workspace/Bimbo") # local

require(data.table)
require(Matrix)
require(xgboost)

data=fread('csv/2-cross-validation.csv', header = T)

# Drop V1 put on by export to file
data[,V1:=NULL]

# Feature selection
# Correlation coeff
# num rounds estimation

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

data_train=data[tst==0,]
data_test=data[tst==1,]

remove(data)
gc()

features=names(data_train)[!(names(data_train) %in% c('id',"Demanda_uni_equil",'tst'))]

wltst = sample(nrow(data_train),120000) #391000)

dval<-xgb.DMatrix(data=data.matrix(data_train[wltst,features,with=FALSE]),
                   label=data.matrix(data_train[wltst,Demanda_uni_equil]),missing=NA)

watchlist<-list(dval=dval)

params_bst <- list(objective="reg:linear",
                   booster = "gbtree",
                   eta= 0.4,
                   max_depth=3,
                   subsample= 0.85,
                   colsample_bytree=0.6,
                   min_child_weight= 1.0)

bst <- xgb.train(params = params_bst ,
                 data = xgb.DMatrix( data=data.matrix(data_train[-wltst,features,with=FALSE]),
                                     label = data.matrix(data_train[-wltst,Demanda_uni_equil]),missing=NA),
                 nrounds               = 150,
                 verbose               = 1,
                 print_every_n         = 5,
                 early_stopping_rounds = 3,
                 watchlist             = watchlist,
                 maximize              = FALSE
)
gc()

# Make prediction for the 10th week
data_test1=data_test[Semana==10,]
pred<-predict(bst,xgb.DMatrix(data.matrix(data_test1[,features,with=FALSE]),missing=NA))
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
pred<-predict(bst,xgb.DMatrix(data.matrix(data_test2[,features,with=FALSE]),missing=NA))
res=exp(round(pred,5))-1
res.df=data.frame(id=data_test2$id,Demanda_uni_equil=res)
results=rbind(results, res.df)

results[results[,2]<0,2]=0
results[,2]=round(results[,2],1)
results[,1]=as.integer(results[,1])
class(results[,1])='int32'
options(digits=18)
write.csv(results,file='submissions/results19.csv',row.names=F)


