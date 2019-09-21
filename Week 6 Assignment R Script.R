## Week 6 Assignment - K-Means and HCA's 

#Load Data
library(readr)
Wholesale_customers_data <- read_csv("Data Science School Documents/MSDS 680 Machine Learning/Week 6/Wholesale customers data.csv")
View(Wholesale_customers_data)
sales <- Wholesale_customers_data

#Explore and Prepare Data
str(sales)
table(sales$Channel)
table(sales$Region)

summary(sales)
plot(sales)

sum(is.na(sales))

#K-Means Analysis

#Libraries for Analysis
install.packages("tidyverse")
library(tidyverse)
install.packages("cluster")
library(cluster)
install.packages("factoextra")
library(factoextra)

#Computing K-means Clusters
k2 <- kmeans(sales, centers = 2, nstart = 25)
str(k2)
k2
fviz_cluster(k2, data = sales)

k3 <- kmeans(sales, centers = 3, nstart = 25)
k4 <- kmeans(sales, centers = 4, nstart = 25)
k5 <- kmeans(sales, centers = 5, nstart = 25)

#plot to compare
p1 <- fviz_cluster(k2, geom = "point", data = sales) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point", data = sales) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point", data = sales) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point", data = sales) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)



#Elbow Method
set.seed(123)

k.max <- 15

data <- sales

head(sales)

sales_scaled <- scale(sales[, -5])

fviz_nbclust(sales_scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

#Average Silhoutte for k-means
fviz_nbclust(sales_scaled, kmeans, method = "silhouette")

#When K=3 this is the ideal number of clusters according to the elbow method.
fviz_cluster(k3, data = sales)

#When k=4 this is the ideal number of clusters according to the silhouette method.
fviz_cluster(k4, data = sales)

#Look into individual variables and their clusters
new_sales <- sales
new_sales$Region <- NULL
(ns <- kmeans(new_sales, 3))

#compare the regions with the result of the new cluster
table(sales$Region, ns$cluster)

#Plot the clusters & centers of the data - Choosing the variables Fresh and Milk
plot(new_sales[c("Fresh", "Milk")], col=ns$cluster)
points(ns$centers[,c("Fresh","Milk")], col=1:3, pch=8, cex=2)

#2 new variables now, the new two are Grocery and Frozen
plot(new_sales[c("Grocery", "Frozen")], col=ns$cluster)
points(ns$centers[,c("Grocery","Frozen")], col=1:3, pch=8, cex=2)

#Gap Stat Method
gap_stat <- clusGap(sales, FUN = kmeans, nstart = 25,
                    K.max = 5, B = 50)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

#Extracting Results
final <- kmeans(sales, 3, nstart = 25) #Based on Elbow method for optimal k value = 3.
print(final)
fviz_cluster(final, data = sales)

final2 <- kmeans(sales, 4, nstart = 25) #Based on Silhouette method for optimal k value = 4
print(final2)
fviz_cluster(final2, data = sales)

## HCA
sales$Channel <- NULL
sales$Region <- NULL

#Normalize the data
normal <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
sales[1:6] <- as.data.frame(lapply(sales[1:6], normal))

#Install the package NbClust
install.packages("NbClust")
library(NbClust)

#Now let's analyze the data
nb <- NbClust(data, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "ward.D2")
fviz_nbclust(nb)

#Setting up for the dendrogram
hc = hclust(dist(sales, method = "euclidean"), method="ward.D2")
hc
plot(hc, hang = -0.01, cex = 0.7)

#Cut the dendrogram into the optimal 3 clusters
fit <- cutree(hc, k = 3)
table(fit)
plot(hc)
rect.hclust(hc, k = 4, border = "red")

#Create clusters with single linkages
nb2 <- NbClust(data, distance = "euclidean", min.nc = 2,
               max.nc = 10, method = "single")
fviz_nbclust(nb2)

#Now the new dendrogram
hc2 <- hclust(dist(data), method = "single")
plot(hc2, hang = -0.01, cex = 0.7)
hc2
fit2 <- cutree(hc2, k = 2)
table(fit2)

#dendrogram at h = 3
fit3 <- cutree(hc, h = 3)
table(fit3)

plot(hc)
rect.hclust(hc, h = 3, border = "red")

#Try at h = .5
fit.5 <- cutree(hc, h = 0.5)
table(fit.5)
rect.hclust(hc, h = .5, border = "red")

#Creating a sample of the data
samp <- sample(1:dim(sales)[1], 50)
sales_sample <- sales[samp,]
sales_sample$Channel <- NULL

hc_samp <- hclust(dist(sales_sample), method = "ave")
plot(hc_samp, hang = -1, labels=sales$Channel[samp])

#Now cut the sample up
fit_hc_samp <- cutree(hc_samp, h = 3)
table(fit_hc_samp)

rect.hclust(hc_samp, k = 3, border = "red")
