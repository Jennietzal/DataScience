library(clv)
data(iris)


# k-means
kmod <- kmeans(iris[,1:4],3) 
plot(iris$Petal.Length~iris$Petal.Width, col=iris$Species)
#hierarcical cluster
hcmod <- hclust(dist(iris[,1:4]))
hcl <- as.integer(cutree(hcmod,3)) 
plot(iris$Petal.Length~iris$Petal.Width, col=hcl)

#intraclust = c("complete","average","centroid")
#interclust = c("single", "complete", "average","centroid")

#######################################
####  Internal Validation #which model is better
#######################################
#Dunn measures the distance between cluster
#Davies Boudin measures homogenuos inside cluster
# compute intercluster distances and intracluster diameters
icd1 <- cls.scatt.data(iris[,1:4], kmod$cluster, dist="manhattan")
icd2 <- cls.scatt.data(iris[,1:4], hcl, dist="manhattan")

dunn1 <- as.numeric(clv.Dunn(icd1, "complete", "complete"))
davies1 <- as.numeric(clv.Davies.Bouldin(icd1, "complete", "complete"))

dunn2 <- as.numeric(clv.Dunn(icd2, "complete", "complete"))
davies2 <- as.numeric(clv.Davies.Bouldin(icd2, "complete", "complete"))

tab = cbind("Davies Boudin",kmeans=davies1,hclust=davies2)
tab = rbind(tab, cbind("Dunn",kmeans=dunn1,hclust=dunn2))

print(tab)

library(cluster)

silh1 <- silhouette(kmod$cluster,dist(iris[,1:4]))
plot(silh1,col=c(1,2,3))

silh2 <- silhouette(hcl,dist(iris[,1:4]))
plot(silh2,col=c(1,2,3))


  ########################################
###  External Validation
########################################

### Confusion matrix ( TP,TN,FP,FN )
# Jaccard index [ TP/(TP+FP+FN) ]
# Rand measure [ TP+TN/(TP+FP+FN+TN) ]
# Fowlkes- Mallows index [ TP/sqrt((TP+FP)(TP+FN)) ]

include(clv)

# use only once std.ext function
#compare the truth against the clusters
std <- std.ext(kmod$cluster, as.numeric(iris$Species))
table(cluster=kmod$cluster,species=(iris$Species))

# to compute three indicies based on std.ext result
rand1 <- clv.Rand(std)
jaccard1 <- clv.Jaccard(std)
folk.mal1 <- clv.Folkes.Mallows(std)

std2 <- std.ext(hcl, as.numeric(iris$Species))
table(cluster=hcl,species=(iris$Species))

# to compute three indicies based on std.ext result
rand2 <- clv.Rand(std2)
jaccard2 <- clv.Jaccard(std2)
folk.mal2 <- clv.Folkes.Mallows(std2)

res1 <- rbind(Rand=rand1,Jaccard=jaccard1,Fowlkes_Mallows=folk.mal1)
res2 <- rbind(Rand=rand2,Jaccard=jaccard2,Fowlkes_Mallows=folk.mal2)

cbind(kmeans=res1,hclust=res2)
#better the higher value

###  Matching based measures
# Purity (% of correctly classified objects)
# F-measure [ 2*precision*recall/(precision + recall) ]

#Pca
pca<-prcomp(iris[,1:4])
plot(pca)
pca$sdev
pca$sdev/sum(pca$sdev)

plot(pca$x)
#Pc1 has the most variability
plot(pca$x,col=iris$Species)
#To see graphically pca 1~3 and to compare against pc1~2
plot(pca$x[,3]~pca$x[,1],col=iris$Species)

kmod<- kmeans(pca$x[,1:2],3)#?????????? ???? ????????????
plot(pca$x,col=kmod$cluster)

table()