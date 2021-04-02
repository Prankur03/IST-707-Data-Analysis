
setwd('C:/Users/prank/OneDrive/Desktop/Sem2/Data Analytics/Data Analytics/Project/Data')
Data2 <- read.csv("household_2010.csv")

summary(Data2)

# Dropping column 'x'
Data2$X <- NULL

# Replacing ? with NA
Data2[,3:9][Data2[,3:9] == "?"] <- NA


#summary(Data2)

# Finding NAs
sum(is.na(Data2$Date))
sum(is.na(Data2$Time))
sum(is.na(Data2$Sub_metering_1))
sum(is.na(Data2$Sub_metering_2))
sum(is.na(Data2$Sub_metering_3))
sum(is.na(Data2$Global_active_power))
sum(is.na(Data2$Global_reactive_power))
sum(is.na(Data2$Voltage))


# Removing NAs
Data2 <- na.omit(Data2)
summary(Data2)

Data <- Data2[3:9]


Data$Sub_metering_1 <- as.numeric(as.character(Data$Sub_metering_1))
Data$Sub_metering_2 <- as.numeric(as.character(Data$Sub_metering_2))
Data$Sub_metering_3 <- as.numeric(Data$Sub_metering_3)
Data$Global_active_power <- as.numeric(as.character(Data$Global_active_power))
Data$Global_reactive_power <- as.numeric(as.character(Data$Global_reactive_power))
Data$Voltage <- as.numeric(as.character(Data$Voltage))
Data$Global_intensity <- as.numeric(as.character(Data$Global_intensity))


# COnverting kilo-watt to watt
Data$Global_active_power <- Data$Global_active_power*1000/60
Data$Global_reactive_power <- Data$Global_reactive_power*1000/60


Data$Global_active_power_dummy <- Data$Global_active_power
Data$Global_active_power_dummy <- ifelse((Data$Global_active_power > 0 & Data$Global_active_power <= 11.67) , 'Low',Data$Global_active_power_dummy)
Data$Global_active_power_dummy <- ifelse((Data$Global_active_power > 11.67 & Data$Global_active_power <= 40) , 'Medium',Data$Global_active_power_dummy)
Data$Global_active_power_dummy <- ifelse((Data$Global_active_power > 40) , 'High',Data$Global_active_power_dummy)

Data$Global_active_power_dummy <- as.factor(Data$Global_active_power_dummy)


str(Data)          
##############################################################################################################

Data <- Data[,2:8]

#summary(glm(Global_active_power_dummy ~. , data=Data, family=binomial))


# Decision tree

library(C50)
library(gmodels)
library(partykit)
library(grid)
library(libcoin)
library(mvtnorm)
library(RColorBrewer)
library(dplyr)
library(rpart)
library(rpart.plot)


set.seed(10)
Training_data <- createDataPartition(Data$Global_active_power_dummy, p = .80, list = FALSE)



set.seed(33)

# Unpruned tree
tree_UP_data <- rpart(Global_active_power_dummy ~ ., 
                 data = Data)

prp(tree_UP_data, faclen = 0, cex = 1, extra = 1, main = "Unpruned Tree")  

conf_matrix_UP_data <- table(Data$Global_active_power_dummy,
                             predict(tree_UP_data, type="class"))

conf_matrix_UP_data

# Adjusting the Minimum Split Size
tree_SS_data <- rpart(Global_active_power_dummy ~ ., 
                 data = Data, 
                 control = rpart.control(minsplit = 40))

prp(tree_SS_data, faclen = 0, cex = 1.0, extra = 1, main = "Minimum Split Size Tree")  

conf_matrix_SS_data <- table(Data$Global_active_power_dummy,
                             predict(tree_SS_data, type="class"))

conf_matrix_SS_data

# Adjusting the Minimum Bucket Size
tree_BS_data <- rpart(Global_active_power_dummy ~ ., 
                 data = Data, 
                 control = rpart.control(minbucket = 60))

prp(tree_BS_data, faclen = 0, cex = 1, extra = 1, main = "Minimum Bucket Size Tree")  


conf_matrix_BS_data <- table(Data$Global_active_power_dummy,
                             predict(tree_BS_data, type="class"))

conf_matrix_BS_data

# Adjusting the Maximum Depth Size
tree_DS_data <- rpart(Global_active_power_dummy ~ ., 
                 data = Data, 
                 control = rpart.control(maxdepth = 4))

prp(tree_DS_data, faclen = 0, cex = 1, extra = 1, main = "Maximum Depth Size Tree")  


conf_matrix_DS_data <- table(Data$Global_active_power_dummy,
                             predict(tree_DS_data, type="class"))

conf_matrix_DS_data


# Creating function for accuracy
accuracy <-function(x) { 
  (sum(diag(x))) / (sum(x)) * 100
}

# Creating function for precision (Positive Predictive Value = TP / (TP + FP))
precision <-function(x) { 
  (x[2,2]) / (sum(x[,2])) * 100
}

# Creating function for recall  (how complete the results are = TP / (TP + FN))
recall <-function(x) { 
  (x[2,2]) / (sum(x[2,])) * 100
}



# Accuracy
accuracy_UP <- accuracy(conf_matrix_UP_data)
accuracy_SS <- accuracy(conf_matrix_SS_data)
accuracy_BS <- accuracy(conf_matrix_BS_data)
accuracy_DS <- accuracy(conf_matrix_DS_data)

data.frame(accuracy_UP, accuracy_SS, accuracy_BS, accuracy_DS)


# Precision 
Precision_UP <- precision(conf_matrix_UP_data)
Precision_SS <- precision(conf_matrix_SS_data)
Precision_BS <- precision(conf_matrix_BS_data)
Precision_DS <- precision(conf_matrix_DS_data)

data.frame(Precision_UP, Precision_SS, Precision_BS, Precision_DS)


# Recall (Measure of how complete the results are = TP / (TP + FN)) same as Sensitivity
Recall_UP <- recall(conf_matrix_UP_data)
Recall_SS <- recall(conf_matrix_SS_data)
Recall_BS <- recall(conf_matrix_BS_data)
Recall_DS <- recall(conf_matrix_DS_data)

data.frame(Recall_UP, Recall_SS, Recall_BS, Recall_DS)

# FMeasure
FMeasure_UP <- (2 * Precision_UP * Recall_UP) / (Precision_UP + Recall_UP)
FMeasure_SS <- (2 * Precision_SS * Recall_SS) / (Precision_SS + Recall_SS)
FMeasure_BS <- (2 * Precision_BS * Recall_BS) / (Precision_BS + Recall_BS)
FMeasure_DS <- (2 * Precision_DS * Recall_DS) / (Precision_DS + Recall_DS)

data.frame(FMeasure_UP, FMeasure_SS, FMeasure_BS, FMeasure_DS)

