#setwd("C:/Users/Debarshi/Desktop/Fall 2017/Intro to Data Mining/project2_clustering")
source("clustering_methods.R")
data2 <- read.csv("dataset2.csv")

#list1 <- list()
#list2 <- list()
#for(i in 2:25) {
  k_cluster <- k_cluster_method2(data2) #k_cluster <- k_cluster_method2(data2, i)
  #list1 <- c(list1, k_cluster$tot.withinss)
  #print(list1)
  #list2 <- c(list2, k_cluster$betweenss)
  #print(list2)
#}

#l1 <- data.frame(list1)
#l2 <- data.frame(list2)
#plot(t(l1), type="o")
#plot(t(l2), type="o")

data2<-cbind(data2,k_cluster$cluster)
count(data2,'k_cluster$cluster')