# Setting working dir env, use one
setwd("~/workspace/Bimbo")
# setwd("C:/Users/mtalaver/workspace/Bimbo") # local

require(data.table)
require(plyr)

train=fread('csv/train.csv', header = T, stringsAsFactors = F)
test=fread('csv/test.csv', header = T, stringsAsFactors = F)

# Only keeping records we are going to use to predict.
# 7 gives     8,9 - one week
# 6 gives   7,8,9 - two weeks
# 5 gives 6,7,8,9 - three weeks
train=train[Semana>5]

# Lets make both sets equal
train$Venta_uni_hoy<-NULL
train$Venta_hoy<-NULL
train$Dev_proxima<-NULL
train$Dev_uni_proxima<-NULL
train[,tst:=0] # Adding tst (not predicting on train set)
train[,id:=0] # Adding id to train
test[,tst:=1] # Adding tst to test (yes predicting on test set)
test[,Demanda_uni_equil:=0] # Adding target to test

data=rbind(train,test)
remove(test)
remove(train)
gc()

town_state=fread('csv/town_state.csv', header = T, stringsAsFactors = F)

town_state <- transform(town_state, Town_ID = as.numeric(interaction(Town, drop = T)))
town_state <- transform(town_state, State_ID = as.numeric(interaction(State, drop = T)))

town_state$Town<-NULL
town_state$State<-NULL

# Merge town_state
data=merge(data,town_state, by = "Agencia_ID")
remove(town_state)

# client count by Town
ClientsByTown <- ddply(data, ~ Town_ID,summarise,ClientesByTown=length(unique(Cliente_ID)))
data=merge(data,ClientsByTown, by = "Town_ID")
remove(ClientsByTown)

# client count by Ruta
ClientsByRuta <- ddply(data, ~ Ruta_SAK,summarise,ClientesByRuta=length(unique(Cliente_ID)))
data=merge(data,ClientsByRuta, by = "Ruta_SAK")
remove(ClientsByRuta)
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
nCanal_ID=data[,.(nCanal_ID=.N),by=.(Canal_ID,Semana)]
nCanal_ID=nCanal_ID[,.(nCanal_ID=mean(nCanal_ID,na.rm=T)),by=Canal_ID]
data=merge(data,nCanal_ID,by='Canal_ID',all.x=T)
remove(nCanal_ID)
gc()
data$Demanda_uni_equil=log(data$Demanda_uni_equil+1)
gc()

# write.csv(data, file = "csv/1-getting-data.csv")
# save.image("C:/Users/mtalaver/workspace/Bimbo/rdata/1-getting-data.RData")
