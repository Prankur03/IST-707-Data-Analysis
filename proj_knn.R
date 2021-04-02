
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
#summary(Data2)

Data <- Data2

Data$Sub_metering_1 <- as.numeric(as.character(Data$Sub_metering_1))
Data$Sub_metering_2 <- as.numeric(as.character(Data$Sub_metering_2))
Data$Sub_metering_3 <- as.numeric(Data$Sub_metering_3)
Data$Global_active_power <- as.numeric(as.character(Data$Global_active_power))
Data$Global_reactive_power <- as.numeric(as.character(Data$Global_reactive_power))
Data$Voltage <- as.numeric(as.character(Data$Voltage))
Data$Global_intensity <- as.numeric(as.character(Data$Global_intensity))

Data$Global_active_power <- Data$Global_active_power*1000/60
Data$Global_reactive_power <- Data$Global_reactive_power*1000/60

Data$sub_metering_remainder <- Data$Global_active_power - (rowSums(Data[,7:9]))

Data$Global_active_power_dummy <- Data$Global_active_power
Data$Global_active_power_dummy <- ifelse((Data$Global_active_power > 0 & Data$Global_active_power <= 11.67) , 'Low',Data$Global_active_power_dummy)
Data$Global_active_power_dummy <- ifelse((Data$Global_active_power > 11.67 & Data$Global_active_power <= 30) , 'Medium',Data$Global_active_power_dummy)
Data$Global_active_power_dummy <- ifelse((Data$Global_active_power > 30) , 'High',Data$Global_active_power_dummy)


Data$Global_active_power_dummy <- as.factor(Data$Global_active_power_dummy)

str(Data)
summary(Data)
#################################################################################################################
# Knn model


library(caret)
library(dplyr)
library(mlbench)
library(class)
library(gmodels)

data_knn <- Data[4:11]

# Inspecting our dataset    
str(data_knn)
#summary(data_knn)


# checking the proportion of classes
table(data_knn$Global_active_power)/nrow(data_knn)

data_knn2 <- data_knn

# Normalization data
data_knn_norm <- data_knn2[1:7] %>% 
  lapply(as.double) %>% 
  as.data.frame()

# Creating a min-max normalization function
norm_min_max <-function(x) { 
  ( x - min(x) ) / ( max(x) - min(x) )
}

data_knn3 <- data_knn_norm %>% 
  lapply(norm_min_max) %>% 
  as.data.frame()

global_act <- data_knn %>%
  select(Global_active_power_dummy)

data_knn4 <- cbind(data_knn3, global_act)

#data_knn4$Global_intensity <- NULL

sum(is.na(data_knn4))

############################################################################

# For running the grid search

set.seed(120)
train_index <- createDataPartition(data_knn4$Global_active_power_dummy, p = 0.8, list = FALSE)

data_train <- data_knn4[train_index, 1:7]
data_test <- data_knn4[-train_index, 1:7]

data_train_labels <- data_knn4[train_index,8]
data_test_labels <- data_knn4[-train_index,8]


# For KNN grid search
#data_test_pred <- train(Global_active_power_dummy ~ ., data = data_train, method = "knn")
#print(data_test_pred)


data_test_pred <- knn(train = data_train, 
                      test = data_test, 
                      cl = data_train_labels, 
                      k = 9)


# Testing accuracy
CrossTable(data_test_labels, data_test_pred,
           prop.chisq = F, prop.c = F, prop.r = F,
           dnn = c( 'ACTUAL  ', '             PREDICTED    '))

data_conf_matrix_knn <- table(data_test_labels, data_test_pred)


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
accuracy_knn <- accuracy(data_conf_matrix_knn)

# Precision 
precision_knn <- precision(data_conf_matrix_knn)

# Recall
recall_knn <- recall(data_conf_matrix_knn)

# FMeasure
FMeasure_knn <- (2 * precision_knn * recall_knn) / (precision_knn + recall_knn)

#Depicting performance of the Knn model in the dataframe
data.frame(accuracy_knn, precision_knn, recall_knn, FMeasure_knn)
