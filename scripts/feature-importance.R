# Setting working dir env, use one
setwd("~/workspace/Bimbo")

require(data.table)
require(plyr)
require(Matrix)
require(xgboost)

train=fread('csv/train.csv', header = T, stringsAsFactors = F)

# Only keeping records we are going to use to predict.
# 7 gives     8,9 - one week
# 6 gives   7,8,9 - two weeks
# 5 gives 6,7,8,9 - three weeks
train=train[Semana>5]

# Lets make both sets equal
# train$Venta_uni_hoy<-NULL
# train$Venta_hoy<-NULL
# train$Dev_proxima<-NULL
# train$Dev_uni_proxima<-NULL
gc()

town_state=fread('csv/town_state.csv', header = T, stringsAsFactors = F)
town_state <- transform(town_state, Town_ID = as.numeric(interaction(Town, drop = T)))
town_state <- transform(town_state, State_ID = as.numeric(interaction(State, drop = T)))

town_state$Town<-NULL
town_state$State<-NULL

data = train
str(data)
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

set.seed(825)
# And we just sample to have a managable working set
data_sample=data[sample(nrow(data),60000),] #391000),]

sparse_matrix<- sparse.model.matrix(Demanda_uni_equil~., data = data_sample)

head(sparse_matrix)

output_vector <- data_sample$Demanda_uni_equil

bst <- xgboost(data=sparse_matrix,
               label=output_vector,
               max.depth=3,
               eta=.4,
               nthread=4,
               nround=500,
               objective="reg:linear",
               early_stopping_rounds = 3)

importance <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst)
head(importance)

importanceRaw <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst, data = sparse_matrix, label = output_vector)

# Cleaning for better display
importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]

head(importanceClean)

xgb.plot.importance(importance_matrix = importanceRaw)
