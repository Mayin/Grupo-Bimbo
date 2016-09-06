# Setting working dir env
setwd("~/workspace/Bimbo/csv")

# Loading the train and test data from csv
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)
# train <- read.csv("train_sample.csv", header = TRUE)
# test <- read.csv("test.csv", header = TRUE)
gc() 

# -----------------------------------------------------
# 'Equaling' Train and Test Sets Variables
# -----------------------------------------------------

# train needs: ID.
train <-data.frame(id = rep(0, nrow(train)), train[,])

# test needs: venta_uni_hoy, venta_hoy, dev_uni_proxima, dev_proxima, demanda_uni_equil
test <-data.frame(Venta_uni_hoy = rep(0, nrow(test)), test[,])
test <-data.frame(Venta_hoy = rep(0, nrow(test)), test[,])
test <-data.frame(Dev_uni_proxima = rep(0, nrow(test)), test[,])
test <-data.frame(Dev_proxima = rep(0, nrow(test)), test[,])
test <-data.frame(Demanda_uni_equil = rep(0, nrow(test)), test[,])

# -----------------------------------------------------
# Combining the Train and Test Sets
# 349963(test)+3709023(train) = 4058986(combined)
# -----------------------------------------------------

combined <- rbind(test,train)
gc() 
remove(train)
remove(test)
gc() 
# -----------------------------------------------------
# Because the order of the data frame matters, lets 
# create an 'order' variable to sort by.
# This ensures we can refer to rows by position.
# -----------------------------------------------------

# New variable to order by
combined$order  <- 1:nrow(combined)

# -----------------------------------------------------
# Sort test... Thi is only valid for sample.
# have to do equivalent for actual files
# -----------------------------------------------------

test[1,]
combined[1,]
test[432,]
combined[432,]
combined[349964:349969,]
train[1:6,]

# -----------------------------------------------------
# Sort test... Thi is only valid for sample.
# have to do equivalent for actual files
# -----------------------------------------------------

# combined[6999252:6999255,]
# train[1:4,]

# -----------------------------------------------------
# Merging town_state.csv
# Commented out was a feature that wasn't worth it.
# Merge and Sort... for all
# -----------------------------------------------------

town_state <- read.csv("town_state.csv", header = TRUE, fileEncoding="UTF-8-BOM")
# town_state$initial <- regexpr(' ', town_state$Town)
# town_state$final <- nchar(as.character(town_state$Town))
# town_state$Street <- substr(town_state$Town,town_state$initial,town_state$final)
# town_state$initial <- NULL
# town_state$final <- NULL

table(town_state$State)
# Lets change 'Queretaro de Arteaga' to 'QUERETARO' 
town_state$State <- as.character(town_state$State)
town_state$State[town_state$State == "Queretaro de Arteaga"] <- "QUERETARO"

# Join town_state to combined
combined <- merge(combined,town_state, by = "Agencia_ID")
combined <- combined[order(combined$order), ]
gc() 
remove(producto_tabla)
# -----------------------------------------------------
# Merging producto_tabla.csv
# Merge and Sort... for all
# -----------------------------------------------------

producto_tabla <- read.csv("producto_tabla.csv", header = TRUE)
combined <- merge(combined,producto_tabla, by = "Producto_ID")
combined <- combined[order(combined$order), ]
gc()

# -----------------------------------------------------
# Merging cliente_tabla.csv w/out dups
# Merge and Sort... for all
# -----------------------------------------------------

# Bringing in information from cliente_tabla
cliente_tabla <- read.csv("cliente_tabla.csv", header = TRUE)

# Drop dup ids
cliente_tabla <- cliente_tabla[ !duplicated(cliente_tabla$Cliente_ID) , ]

# Join cliente_tabla to combined
combined <- merge(combined,cliente_tabla, by = "Cliente_ID")
gc()
remove(cliente_tabla)
combined <- combined[order(combined$order), ]
gc() 

# -----------------------------------------------------
# Factors I can make
# -----------------------------------------------------

combined$Semana <- as.factor(combined$Semana)
combined$Canal_ID <- as.factor(combined$Canal_ID)
combined$State <- as.factor(combined$State)
gc() 
# I am saving it post sort! 
str(combined)
table(combined$Agencia_ID)
# -----------------------------------------------------
# Using random forests to ponder which variables are most
# valuable in predicting perfect inventory supply
# -----------------------------------------------------

library(randomForest)

# Variable combinations to try
var_list.1 <- c("Semana", "Canal_ID", "State") # 87.4
var_list.01 <- c("Ruta_SAK", "Canal_ID", "Produto","Dev_uni_proxima") # 87.6
var_list.2 <- c("Agencia_ID", "Semana") #  87.4
var_list.3 <- c("Dev_uni_proxima", "Ruta_SAK", "Cliente_ID", "Producto_ID") # 92.96
var_list.4 <- c("Agencia_ID", "Semana", "Canal_ID", "Ruta_SAK") # 89.34
var_list.5 <- c("State", "Ruta_SAK", "Cliente_ID", "Producto_ID") # 
var_list.6 <- c("Canal_ID", "Ruta_SAK", "Cliente_ID", "Producto_ID") # 93.1
var_list.7 <- c("Agencia_ID", "Ruta_SAK", "Cliente_ID", "Producto_ID") # 92.4
var_list.8 <- c("Dev_uni_proxima", "Ruta_SAK", "Cliente_ID", "Producto_ID") # 92.96
var_list.9 <- c("Canal_ID", "Ruta_SAK", "Cliente_ID", "Producto_ID","State") # 90.24
var_list.0 <- c("Agencia_ID", "Semana", "Canal_ID", "Ruta_SAK", "Cliente_ID") # 90.68

# Parameters for operation
active_var_list <- var_list.1
sample_size <- 5000
seed <- 1234
num_trees <- 1000
rf.data <- combined[1:sample_size,]

# The desired prediction is the adjusted demand
# 6999252
rf.predict <- as.factor(combined$Demanda_uni_equil[6999252:(6999252+sample_size-1)])
# rf.predict <- as.factor(combined$Demanda_uni_equil[349964:(349964+sample_size-1)])

# This is the train data
rf.train <- combined[1:sample_size, active_var_list]

# Setting a seed ensures the results are repeatable
set.seed(seed)

# Random forest call
combined.rf <- randomForest(x = rf.train, y = rf.predict, data = rf.data, importance=TRUE, ntree=num_trees)

# Check please
combined.rf

# Plot
varImpPlot(combined.rf)
gc()

levels(test$Canal_ID) <- levels(combined$Canal_ID)
levels(test$Semana) <- levels(combined$Semana)

# F remove state from combined for kicks
combined$State <- NULL
# Creating a Kaggle submission
predict_demand <- predict(combined.rf, test)
submit <- data.frame(id = test$id, demanda_uni_equil = predict_demand)
write.csv(submit, file = "kaggletwo.csv")

# -----------------------------------------------------
# Voy por aqui !!!!!!!!!!!!!!!!!!
# rf.train.1 <- combined[1:3709023, c("Agencia_ID", "Semana")]
# -----------------------------------------------------







# output.forest <- randomForest(Demanda_uni_equil ~ Semana + Canal_ID + State, data = combined[1:5000,], importance = TRUE, ntree = 1000)
# output.forest
# varImpPlot(output.forest)
# print(importance(output.forest,type = 2)) 

# set.seed(1234)
# combined.rf <- randomForest(as.factor(Demanda_uni_equil) ~ Canal_ID + Semana +  State,
#                             data=combined[1:5000,], 
#                             importance=TRUE, 
#                             ntree=1000)
# combined.rf
# varImpPlot(combined.rf)


