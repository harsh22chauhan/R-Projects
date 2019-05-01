rm(list=ls())
cust_data <- read.csv("E:/R/cluster.csv")
View(cust_data)
head(cust_data)
summary(cust_data)

#remove visit_id as this is not of our use.
cust_data <- cust_data[-c(1)]
head(cust_data)
summary(cust_data)

#scaling the data
cust_data_f <- scale(cust_data)
head(cust_data_f)
summary(cust_data_f)
apply(cust_data_f,2,sd)

dist.res = dist(cust_data_f,method = "euclidean")
head(dist.res)

hc <- hclust(dist.res,method = "complete")
hc2 <- hclust(dist.res,method = "centroid")
hc3 <- hclust(dist.res,method = "average")
hc4 <- hclust(dist.res,method = "ward.D")

plot(hc,hang = -1)
plot(hc2,hang = 0)
plot(hc3, hang = -1)
plot(hc4, hang = -1)

rect.hclust(hc,k=3,border = 2:3)

install.packages("vegan")
library(vegan)

fit <- cascadeKM(scale(cust_data,center = T,scale = T),1,10,iter = 1000)
plot(fit,sortg = T,grpmts.plot = T)
calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("calinski criterion optimal number of cluster:",calinski.best,"\n")
summary(fit)

#looking at the elbow chart
mydata <- cust_data

was <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for(i in 2:15) was[i] <- sum(kmeans(mydata,centers = i)$withinss)

plot(1:15,was,type = "b",xlab = "number of clusters",ylab = "within groups sum of squares",col = "mediumseagreen",pch=12)

k1 <-kmeans(cust_data_f,2)
k1
k2 <-kmeans(cust_data_f,3)
k2

install.packages("cluster")
library(cluster)
diss = daisy(cust_data_f)
sp = silhouette(k1$cluster,diss)
windows()
plot(sp)

aggregate(.~k1$cluster,data = cust_data,mean)
