#setwd("C:/Users/Debarshi/Desktop/Fall 2017/Intro to Data Mining/project2_clustering")
data <- read.csv("dataset1.csv")
#install.packages("dbscan")
#install.packages("plotly")
library("plotly")

##########################Hierarchical Clustering###############################################
h_cluster_method <- function(data) {
  h_clusters <- hclust(dist(data[,1:3],method = "maximum"), method = "ward.D2")
  h_clusterCut <- cutree(h_clusters, 8)
  var1 <- table(h_clusterCut, data$cluster)
  print(paste("Accuracy for Hierarchical Clustering: ", sum(diag(var1))/10,"%", sep = ""))
  h_result = data.frame(data[,1:3],h_clusterCut)
  plot(h_clusters)
  p1 <- plot_ly(h_result, x = ~x, y = ~y, z = ~z, color = ~h_clusterCut) %>%
    add_markers() %>%
    layout(scene = list(xaxis = list(title = 'X'),
                        yaxis = list(title = 'Y'),
                        zaxis = list(title = 'Z')))
  print(p1)
  return(var1)
}

################################K-means Clustering##############################################

k_cluster_method <- function(data) {
  set.seed(178)
  kmeans_cluster <- kmeans(data[,1:3], 8, iter.max = 10, nstart = 2)
  var2 <- table(kmeans_cluster$cluster, data$cluster)
  print(paste("Accuracy for K-means Clustering: ", sum(diag(var2))/10, "%", sep = ""))
  k_result = data.frame(data[,1:3], kmeans_cluster$cluster)
  p2 <- plot_ly(k_result, x = ~x, y = ~y, z = ~z, color = ~kmeans_cluster.cluster) %>%
    add_markers() %>%
    layout(scene = list(xaxis = list(title = 'X'),
                        yaxis = list(title = 'Y'),
                        zaxis = list(title = 'Z')))
  print(p2)
  return(var2)
}

##########################Density-based Clustering##############################################

d_cluster_method <- function(data) {
  library("dbscan")
  density_clusters <- dbscan(data[,1:3], 1.313, minPts = 4)
  var3 <- table(density_clusters$cluster, data$cluster)[2:9,]
  print(paste("Accuracy for Density-based Clustering: ", sum(diag(var3))/10, "%", sep = ""))
  density_result = data.frame(data[,1:3], density_clusters$cluster)
  p3 <- plot_ly(density_result, x = ~x, y = ~y, z = ~z, color = ~density_clusters.cluster) %>%
    add_markers() %>%
    layout(scene = list(xaxis = list(title = 'X'),
                        yaxis = list(title = 'Y'),
                        zaxis = list(title = 'Z')))
  print(p3)
  kNNdistplot(data[, 1:3], k=4)
  abline(h=1.313, col="blue")
  return(var3)
}

#######################Graph-based Clustering###################################################

g_cluster_method <- function(data) {
  graph_clusters <- sNNclust(dist(data[,1:3], method = "euclidean"), k = 20, eps = 10, minPts = 16)
  var4 <- table(graph_clusters$cluster, data$cluster)[2:9,]
  print(paste("Accuracy for Graph-based Clustering: ", sum(diag(var4))/10, "%", sep = ""))
  graph_result = data.frame(data[,1:3], graph_clusters$cluster)
  p4 <- plot_ly(graph_result, x = ~x, y = ~y, z = ~z, color = ~graph_clusters.cluster) %>%
    add_markers() %>%
    layout(scene = list(xaxis = list(title = 'X'),
                        yaxis = list(title = 'Y'),
                        zaxis = list(title = 'Z')))
  print(p4)
  return(var4)
}

################K-means Clustering_dataset2#####################################################

k_cluster_method2 <- function(data) {  #k_cluster_method2 <- function(data, k) {
  set.seed(178)
  kmeans_cluster <- kmeans(data[,1:4], 20, iter.max = 10, nstart = 2) 
  #kmeans_cluster <- kmeans(data[,1:4], k, iter.max = 10, nstart = 2)
  return(kmeans_cluster)
}
