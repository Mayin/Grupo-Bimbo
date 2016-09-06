# Set working dir env
setwd("~/workspace/Bimbo/csv")

# Loads train,test data
train <- read.csv("train_sample.csv", header = TRUE)
test <- read.csv("test_sample.csv", header = TRUE)
gc() 

# Make both train and test sets have variables
# train needs: ID.
train <-data.frame(id = rep(0, nrow(train)), train[,])
# test needs: venta_uni_hoy,venta_hoy,dev_uni_proxima,dev_proxima,demanda_uni_equil
test <-data.frame(Venta_uni_hoy = rep(0, nrow(test)), test[,])
test <-data.frame(Venta_hoy = rep(0, nrow(test)), test[,])
test <-data.frame(Dev_uni_proxima = rep(0, nrow(test)), test[,])
test <-data.frame(Dev_proxima = rep(0, nrow(test)), test[,])
test <-data.frame(Demanda_uni_equil = rep(0, nrow(test)), test[,])

# Smash baby
combined <- rbind(test,train)
gc() 

# Factors I can make
combined$Semana <- as.factor(combined$Semana)
combined$Canal_ID <- as.factor(combined$Canal_ID)
gc() 

# Summary
head(combined,20)

# For now, lets now create smallest dataframe with only 
# Semana, Venta_uni_hoy, Dev_uni_proxima,Venta_hoy, Dev_proxima
sampled_stacked_bar <- combined
sampled_stacked_bar$Agencia_ID <- NULL
sampled_stacked_bar$Demanda_uni_equil <- NULL
sampled_stacked_bar$Canal_ID <- NULL
sampled_stacked_bar$Ruta_SAK <- NULL
sampled_stacked_bar$Cliente_ID <- NULL
sampled_stacked_bar$Producto_ID <- NULL
sampled_stacked_bar$id <- NULL

head(sampled_stacked_bar, 20)

class(sampled_stacked_bar)
str(sampled_stacked_bar)

library("plyr")

# Lets create an agg by week
agg_per_week <- ddply(sampled_stacked_bar, .(Semana), colwise(sum))
head(agg_per_week, 20)
gc()

# One for units
agg_units_per_week <- agg_per_week
agg_units_per_week$Dev_proxima <- NULL
agg_units_per_week$Venta_hoy <- NULL
agg_units_per_week$Total_Amounts <- NULL

head(agg_units_per_week, 20)
gc()

# One for amounts
agg_amounts_per_week <- agg_per_week
agg_amounts_per_week$Dev_uni_proxima <- NULL
agg_amounts_per_week$Venta_uni_hoy <- NULL

head(agg_amounts_per_week, 20)
str(agg_amounts_per_week)
gc()

# Transpose our amounts table
trans_agg_amounts_per_week <- t(agg_amounts_per_week[,2:ncol(agg_amounts_per_week)])
# Set the column headings, we need these for our x-axis labels
colnames(trans_agg_amounts_per_week) <- c("3","4","5","6","7","8","9","10","11")
# Lets switch row order
trans_agg_amounts_per_week <- trans_agg_amounts_per_week[nrow(trans_agg_amounts_per_week):1, ]
# And plot Unit Amounts Overview plot
barplot(trans_agg_amounts_per_week, main="$$$ Per Week",
        xlab = "Week", ylab = "$$$",
        col=c("lightgreen", "brown1"), ylim=c(0,40000000),
        legend=rownames(trans_agg_amounts_per_week))

# Similarly, for units
trans_agg_units_per_week <- t(agg_units_per_week[,2:ncol(agg_units_per_week)])
# Set the column headings, we need these for our x-axis labels
colnames(trans_agg_units_per_week) <- c("3","4","5","6","7","8","9","10","11")
# Lets switch row order
trans_agg_units_per_week <- trans_agg_units_per_week[nrow(trans_agg_units_per_week):1, ]
# And plot Unit Amounts Overview plot
barplot(trans_agg_units_per_week, main= "Units Per Week",
        xlab = "Week", ylab = "Units",
        col=c("lightgreen", "brown1"), ylim=c(0,5000000),
        legend=rownames(trans_agg_units_per_week))






