  
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
  
  #############################################################################################################
  
  #install.packages(c('dplyr', 'tidyverse', 'cluster', 'factoextra', 'dendextend', 'ggplot2'))
  
  library(stats)
  library(dplyr)
  
  library(ggplot2)
  library(tidyverse)  # data manipulation
  library(cluster)    # clustering algorithms
  library(factoextra) # clustering visualization
  library(dendextend) # for comparing two dendrograms
  
  data_k <- Data
  
  str(data_k)
  
  # Scaking the dataset
  data_k %>% scale()
  
  set.seed(10)
  data_k1 <- sample_n(data_k, 20000)
  
  
  # Testing the data for first 20000 entries
  #data_k1 <- data_k[1:20000,]
  
  # Plotting the Elbow plot for first 20000 entries
  fviz_nbclust(data_k1, kmeans, method = 'wss')


# Applying k-meanns algorithm
set.seed(10)

data_kmean <- data_k1 %>% 
  kmeans(centers = 4, nstart = 25)

plot2 <- fviz_cluster(data_kmean, geom = "point",  data = data_k1) + ggtitle("k = 4")
plot2


data_kmean
# cluster: A vector of integers (from 1:k) indicating the cluster to which each point is allocated.
# centers: A matrix of cluster centers.
# totss: The total sum of squares.
# withinss: Vector of within-cluster sum of squares, one component per cluster.
# tot.withinss: Total within-cluster sum of squares, i.e. sum(withinss).
# betweenss: The between-cluster sum of squares, i.e. $totss-tot.withinss$.
# size: The number of points in each cluster.
# Within cluster sum of squares by cluster: 79.0 %
# The size of the clusters are 310, 11923, 7183, 584


#Cluster means:
#  Global_active_power Global_reactive_power  Voltage Global_intensity Sub_metering_1 Sub_metering_2 Sub_metering_3
#1           62.286667              3.199785 238.8796        15.771613      0.5806452     37.2838710     12.1838710
#2            7.506282              1.942872 242.3610         1.953669      0.0426906      0.4383125      0.8625346
#3           28.907416              2.352462 240.4693         7.229291      0.2799666      0.6146457     17.0066824
#4           63.806164              3.298801 238.0191        16.293151     30.0976027      1.3150685     13.1678082


class(data_kmean$cluster)

test <- data_kmean$cluster %>%
  as.data.frame()

test1 <- cbind(test, data_k1)
test1


# Dissimilarity matrix
dis_mat <- data_k1 %>%
  dist(method = "euclidean")

# Hierarchial clustering
h1 <- dis_mat %>% 
  hclust(method = "complete")

# Ward's Method
h2 <- dis_mat %>% 
  hclust(method = "ward.D2" )

# Single's method
h3 <- dis_mat %>% 
  hclust(method = "single" )

# Average method
h4 <- dis_mat %>% 
  hclust(method = "average" )

# Plotting
h1 %>% as.dendrogram ()
# 'dendrogram' with 2 branches and 20000 members total, at height 166.3572 

h2 %>% as.dendrogram ()
# 'dendrogram' with 2 branches and 20000 members total, at height 2934.847 

h3 %>% as.dendrogram ()
# 'dendrogram' with 2 branches and 20000 members total, at height 42.64391 

h4 %>% as.dendrogram ()
# 'dendrogram' with 2 branches and 20000 members total, at height 126.0513 



# Applying k-means algorithm on whole data
set.seed(10)

data_kmean2 <- data_k %>% 
  kmeans(centers = 4, nstart = 25)

data_kmean2
# K-means clustering with 3 clusters of sizes 107361, 110243, 239790
# Within cluster sum of squares by cluster: 78.5%

#fviz_cluster(data_kmean2, data_k)

plot <- fviz_cluster(data_kmean2, geom = "point",  data = data_k) + ggtitle("k = 4")
plot


data_kmean2
# Within cluster sum of squares by cluster: 78.4 % %
# The size of the clusters are 166070, 271597, 8884, 10843

#Cluster means:
#Global_active_power Global_reactive_power  Voltage Global_intensity Sub_metering_1 Sub_metering_2 Sub_metering_3
#1           28.864643              2.385851 240.5293         7.219866      0.3033661      0.6085205     16.9789426
#2            7.523723              1.935411 242.3497         1.955143      0.0459173      0.4340696      0.8857314
#3           63.415856              3.527172 238.6129        16.080099      0.9300991     30.2758892     13.4568888
#4           63.441243              3.084992 238.1025        16.188749     35.0984045      1.4745919     12.3182699

class(data_kmean2$cluster)

clustered <- data_kmean2$cluster %>%
  as.data.frame()

clustered_data <- cbind(clustered, data_k)
clustered_data

