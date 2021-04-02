
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

# SVM model

library(e1071)
library(caret)
library(dplyr)
library(kernlab)
library(lattice)
library(ggplot2)
library(mlbench)
library(class)
library(gmodels)


set.seed(120)

Data_svm <- Data[4:11]



# Splitting dataset
Training_svm <- createDataPartition(Data_svm$Global_active_power_dummy, p = .80, list = FALSE)

svm_train <- Data_svm[Training_svm,]
svm_test  <- Data_svm[-Training_svm,] 

svm_test_labels <- Data_svm[-Training_svm, 8]
test_set_svm_final  <- Data_svm[-Training_svm,] %>% select(-Global_active_power_dummy)


# Building SVM model
svm_model_bm <- svm(formula = Global_active_power_dummy ~ ., 
                       data = svm_train,
                       type = 'C-classification',   
                       kernel = 'linear')


# Predictions on the test data
svm_pred_bm <- predict(svm_model_bm, test_set_svm_final)

# Confusion matrix
CrossTable(svm_test_labels, svm_pred_bm,
           prop.chisq = F, prop.c = F, prop.r = F,
           dnn = c( 'ACTUAL  ', '             PREDICTED    '))

conf_matrix_svm_bm <- table(svm_pred_bm, svm_test_labels)

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
accuracy_svm_bm <- accuracy(conf_matrix_svm_bm)

# Precision 
precision_svm_bm <- precision(conf_matrix_svm_bm)

# Recall (Measure of how complete the results are = TP / (TP + FN)) same as Sensitivity
recall_svm_bm <- recall(conf_matrix_svm_bm)

# FMeasure
FMeasure_svm_bm <- (2 * precision_svm_bm * recall_svm_bm) / (precision_svm_bm + recall_svm_bm)


#Depicting performance of the Knn model in the dataframe
data.frame(accuracy_svm_bm, precision_svm_bm, recall_svm_bm, FMeasure_svm_bm)

#################################################################################################################

