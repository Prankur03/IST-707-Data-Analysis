##################################################################################################
# Data <- read.csv2("household_power_consumption.txt", header = TRUE, sep = ";")
# View(Data)

# summary(Data)

# new_data <- Data[1:21996,] #2006
# new_data <- Data[21997:547596,] #2007
# new_data <- Data[547597:1074636,] #2008
# new_data <- Data[1074637:1600236,] #2009
# new_data <- Data[1600237:2075259,] #2010
 
# View(new_data)

###################################################################################################
# Writing the new_data into a new csv file named household_new with first 70K rows

# write.csv(new_data,"C:/Users/prank/OneDrive/Desktop/Sem2/Data Analytics/Data Analytics/Project/Data\\household_2010.csv")

###################################################################################################

setwd('C:/Users/prank/OneDrive/Desktop/Sem2/Data Analytics/Data Analytics/Project/Data')
Data2 <- read.csv("household_2010.csv")
#View(new_data)  
#Data2 <- new_data 
summary(Data2)
str(Data2)

# Dropping column 'x'
Data2$X <- NULL
str(Data2)

# Replacing ? with NA
Data2[,3:9][Data2[,3:9] == "?"] <- NA


summary(Data2)

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


# Converting Factor to integer
Data2$Sub_metering_1 <- as.numeric(as.character(Data2$Sub_metering_1))
Data2$Sub_metering_2 <- as.numeric(as.character(Data2$Sub_metering_2))
Data2$Sub_metering_3 <- as.numeric(as.character(Data2$Sub_metering_3))
Data2$Global_active_power <- as.numeric(as.character(Data2$Global_active_power))

# Calculating the remainder of watt-hours apart from the sub meters
Data2$Total_sub_metering <- Data2$Global_active_power*1000/60
Data2$sub_metering_remainder <- Data2$Total_sub_metering - (rowSums(Data2[,7:9]))

#View(Data2)
str(Data2)

####################################################################################################
# Changing the data type and format for columns


# Changing date format
Data2$Date <- as.Date.character(Data2$Date)

# Adding new column for week number, month and day of the week
Data2$Week <- format(as.Date(Data2$Date, format="%Y/%m/%d"), format= "%W")
Data2$Month <- months(as.Date(Data2$Date))
Data2$Day <- weekdays(as.Date(Data2$Date))

str(Data2)

####################################################################################################
# Visualizations

Data3 <- Data2
str(Data3)

library(ggplot2)

# Plot for monthly consumption
Data3$Month = factor(Data3$Month,levels=c("January","February","March", "April","May","June","July","August","September", "October","November","December"),ordered=TRUE)

ggplot(Data3, aes(x = Data3$Month, y = Data3$Total_sub_metering, size=3, colour='blue')) +
  geom_line(color="#69b3a2", alpha=0.9) +
  ggtitle("Monthly power consumption") +
  xlab("Month") +
  ylab("Total Power consumption")
  


# Plot for consumption on the basis of days of week
Data3$Day = factor(Data3$Day,levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),ordered=TRUE)

ggplot(Data3, aes(x = Data3$Day, y = Data3$Total_sub_metering, size=3, colour='blue')) +
  geom_line(color="#69b3a2", alpha=0.9) +
  ggtitle("Daily power consumption") +
  xlab("Day of the week") +
  ylab("Total Power consumption")

# Plot for consumption on the basis weeks in year
Data3$Day = factor(Data3$Day,levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),ordered=TRUE)

ggplot(Data3, aes(x = Data3$Week, y = Data3$Total_sub_metering, size=3, colour='blue')) +
  geom_line(color="#69b3a2", alpha=0.9) +
  ggtitle("Daily power consumption") +
  xlab("Week of the year") +
  ylab("Total Power consumption")


####################################################################################################

# K means Clustering

library(stats)
library(dplyr)

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms

str(Data3)

#Data3$Day <- as.factor(Data3$Day)
#Data3$Month <- as.factor(Data3$Month)
#Data3$Week <- as.factor(Data3$Week)
#Data3$Date <- as.factor(Data3$Date)
#Data3$Sub_metering_1 <- as.factor(as.character(Data3$Sub_metering_1))
#Data3$Sub_metering_2 <- as.factor(as.character(Data3$Sub_metering_2))
#Data3$Sub_metering_3 <- as.factor(as.character(Data3$Sub_metering_3))
#Data3$Total_sub_metering <- as.factor(as.character(Data3$Total_sub_metering))
#Data3$Global_active_power <- as.factor(as.character(Data3$Global_active_power))
#Data3$sub_metering_remainder <- as.factor(Data3$sub_metering_remainder)

data_k <- Data2
data_k$Week <- NULL
data_k$Day <- NULL
data_k$Month <- NULL
data_k$Date <- NULL
data_k$Time <- NULL

str(data_k)

data_k$Global_reactive_power <- as.numeric(data_k$Global_reactive_power)
data_k$Voltage <- as.numeric(data_k$Voltage)
data_k$Global_intensity <- as.numeric(data_k$Global_intensity)

#View(data_k_mean)
data_k %>% scale()

data_k1 <- data_k[1:45739, ]
str(data_k1)

# Checking for NA's
is.na(data_k1) %>% which()


# Plotting the Elbow plot for our data
#elbow_plot <- data_k1 %>%
#  fviz_nbclust(kmeans, method = 'wss')

# Visualizing the plot
#elbow_plot

# Looking at the plot, we can assume that the optimal number of clusters k is 3 since there is a clear elbow at k = 3.
# At k = 3, we have a small SSE, but that the SSE tends to decrease toward 0 as we increase k.

# Average Silhouette Method
# I tried anothet method just to make sure my answer is correct
fviz_nbclust(data_k1, kmeans, method = "silhouette")

# The resul is the same as elbow, i.e. k = 3

# Applying k-meanns algorithm
set.seed(1)

data_F_km <- data_F %>% 
  kmeans(centers = 3, nstart = 25)

fviz_cluster(data_F_km, data_F)

plot2 <- fviz_cluster(data_F_km, geom = "point",  data = data_F) + ggtitle("k = 3")
plot2


str(data_F_km)
# cluster: A vector of integers (from 1:k) indicating the cluster to which each point is allocated.
# centers: A matrix of cluster centers.
# totss: The total sum of squares.
# withinss: Vector of within-cluster sum of squares, one component per cluster.
# tot.withinss: Total within-cluster sum of squares, i.e. sum(withinss).
# betweenss: The between-cluster sum of squares, i.e. $totss-tot.withinss$.
# size: The number of points in each cluster.

# The size of the cluster is 193,207,199

data_F_km
# Cluster means:
#    Age        Spending
# 1  1.2356308 -0.01148828
# 2 -0.1293509  1.10122216
# 3 -1.0638246 -1.13435049

class(data_F_km$cluster)


test <- data_F_km$cluster %>%
  as.data.frame()


test1 <- cbind(test, data_F)
test1

####################################################################################################
