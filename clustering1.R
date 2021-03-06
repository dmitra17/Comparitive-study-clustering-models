#setwd("C:/Users/Debarshi/Desktop/Fall 2017/Intro to Data Mining/project2_clustering")
source("clustering_methods.R")
data1 <- read.csv("dataset1.csv")

h_cluster <- h_cluster_method(data1)
print("Confusion Matrix for Hierarchical cluster")
print(h_cluster)
k_cluster <- k_cluster_method(data1)
print("Confusion Matrix for K-means cluster")
print(k_cluster)
d_cluster <- d_cluster_method(data1)
print("Confusion Matrix for Density-based cluster")
print(d_cluster)
g_cluster <- g_cluster_method(data1)
print("Confusion Matrix for Graph-based cluster")
print(g_cluster)