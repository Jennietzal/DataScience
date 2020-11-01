View(iris)
kmod<-kmeans(as.matrix(iris[,1:4]), centers = 3)
#to check how many in each cluster (should be 50 in each from the data)
table(kmod$cluster)
#where is the center of each group
kmod$centers

#If the Groups are different
kmod$tot.withinss

plot(iris$Sepal.Length,iris$Sepal.Width, col=iris$Species)
plot(iris$Petal.Length,iris$Petal.Width, col=iris$Species)
plot(iris$Petal.Length,iris$Petal.Width, col=kmod$cluster)

#Elbow method to check the optimal k centers (iterations)
plotElbow<- function(data, kmax=8){
  set.seed(123)
  wss<-sapply(1:kmax,function(k){kmeans(data,k,nstart = 50,iter.max = 15)$tot.withinss})
  wss
  plot(1:kmax,wss,type = "b",pch=19,frame=FALSE,xlab = "Number of Clusters K",ylab = "Total within clusters sum of squares")
}

plotElbow(iris[,1:4],kmax = 8)

library(NbClust) #24 different algorithm to tell you best number of Clusters
res<- NbClust(iris[,1:4],distance = "euclidean",min.nc = 2,max.nc = 8,method = "complete")

#Hierarchical Clustering (Conectivity Based)
hcmod<- hclust(d=dist(iris[,1:4],method = "canberra"))
plot(hcmod)       

#To make groups from hclust
cluster_hc<-cutree(hcmod,3)
table(cluster_hc)
plot(iris$Petal.Length,iris$Petal.Width, col=cluster_hc)
