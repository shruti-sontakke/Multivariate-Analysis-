---
  title: "Cluster_Analysis"
author: "ss4230@scarletmail.rutgers.edu"
date: "03/02/2023"
output: html_document
---
  
  ```{r}

library(cluster)
library(readr)
library(factoextra)
library(magrittr)
library(NbClust)

# With made up data. 
diabetes <- read_csv("C:/Users/Shruti Sontakke/Downloads/archive (4)/diabetes.csv")
colnames(diabetes) <- rownames(diabetes)
diabetes
diabetes <- dist(diabetes,method = "euclidean")
diabetes

# Clustering
#Single
mat5.nn <- hclust(diabetes, method = "single")
plot(mat5.nn, hang=-1,xlab="Object",ylab="Distance",main="Dendrogram. Nearest neighbor linkage")

#Default - Complete
mat5.fn <- hclust(diabetes)
plot(mat5.fn,hang=-1,xlab="Object",ylab="Distance",
     main="Dendrogram. Farthest neighbor linkage")

#Average
mat5.avl <- hclust(diabetes,method="average")
plot(mat5.avl,hang=-1,xlab="Object",ylab="Distance",
     main="Dendrogram. Group average linkage")

# Lets use Canines

canines <- read_csv("Prehistoric dogs.csv")
canines
matstd.can <- scale(canines[-1])

# Creating a (Euclidean) distance matrix of the standardized data 
dist.canine <- dist(matstd.can, method="euclidean")

# Invoking hclust command (cluster analysis by single linkage method)      
cluscanine.nn <- hclust(dist.canine, method = "single") 

# Plotting vertical dendrogram      
# create extra margin room in the dendrogram, on the bottom (Canine species' labels)
#par(mar=c(6, 4, 4, 2) + 0.1)
plot(as.dendrogram(cluscanine.nn),ylab="Distance between Canine species",ylim=c(0,2.5),main="Dendrogram of seven canine species")


diabetes <- read_csv("C:/Users/Shruti Sontakke/Downloads/archive (4)/diabetes.csv")
attach(diabetes)
dim(diabetes)
str(diabetes)
diabetes$Outcome <- as.factor(diabetes$Outcome)
str(diabetes)
diabetes
# Hirerarchic cluster analysis, Nearest-neighbor

# Standardizing the data with scale()
matstd.diabetes <- scale(diabetes[,2:9])
matstd.diabetes
# Creating a (Euclidean) distance matrix of the standardized data
dist.diabetes <- dist(matstd.diabetes, method="euclidean")
# Invoking hclust command (cluster analysis by single linkage method)
clusdiabetes.nn <- hclust(dist.diabetes, method = "single")

plot(as.dendrogram(clusdiabetes.nn),ylab="Distance between Diabetic and Non-Diabetic",ylim=c(0,8),
     main="Dendrogram.People detected as Diabetic or Non-Diabetic \n depending on 8 parameter")

plot(as.dendrogram(clusdiabetes.nn),ylab="Distance between Diabetic and Non-Diabetic",ylim=c(0,6),
     horiz = TRUE,main="Dendrogram.People detected as Diabetic or \n Non-Diabetic depending on 8 parameter")

# We will use agnes function as it allows us to select option for data standardization, the distance measure and clustering algorithm in one single function

(agne.diabetes <- agnes(diabetes, metric="euclidean", stand=TRUE, method = "single"))
#View(agne.diabetes)

#  Description of cluster merging
agne.diabetes$merge

#Dendogram
plot(as.dendrogram(agne.diabetes), xlab= "Distance between Countries",xlim=c(8,0),
     horiz = TRUE,main="Dendrogram.People detected as Diabetic or \n Non-Diabetic depending on 8 parameter")

#Interactive Plots
#plot(agn.employ,ask=TRUE)
plot(agne.diabetes, which.plots=1)
plot(agne.diabetes, which.plots=2)
plot(agne.diabetes, which.plots=3)

# K-Means Clustering

matstd.diabetes <- scale(diabetes[,2:9])
# K-means, k=2, 3, 4, 5, 6
# Centers (k's) are numbers thus, 10 random sets are chosen

(kmeans2.diabetes <- kmeans(matstd.diabetes,2,nstart = 10))
# Computing the percentage of variation accounted for. Two clusters
perc.var.2 <- round(100*(1 - kmeans2.diabetes$betweenss/kmeans2.diabetes$totss),1)
names(perc.var.2) <- "Perc. 2 clus"
perc.var.2

# Computing the percentage of variation accounted for. Three clusters
(kmeans3.diabetes <- kmeans(matstd.diabetes,3,nstart = 10))
perc.var.3 <- round(100*(1 - kmeans3.diabetes$betweenss/kmeans3.diabetes$totss),1)
names(perc.var.3) <- "Perc. 3 clus"
perc.var.3

# Computing the percentage of variation accounted for. Four clusters
(kmeans4.diabetes <- kmeans(matstd.diabetes,4,nstart = 10))
perc.var.4 <- round(100*(1 - kmeans4.diabetes$betweenss/kmeans4.diabetes$totss),1)
names(perc.var.4) <- "Perc. 4 clus"
perc.var.4

# Computing the percentage of variation accounted for. Five clusters
(kmeans5.diabetes <- kmeans(matstd.diabetes,5,nstart = 10))
perc.var.5 <- round(100*(1 - kmeans5.diabetes$betweenss/kmeans5.diabetes$totss),1)
names(perc.var.5) <- "Perc. 5 clus"
perc.var.5

# Computing the percentage of variation accounted for. Six clusters
(kmeans6.diabetes <- kmeans(matstd.diabetes,6,nstart = 10))
perc.var.6 <- round(100*(1 - kmeans6.diabetes$betweenss/kmeans6.diabetes$totss),1)
names(perc.var.6) <- "Perc. 6 clus"
perc.var.6
attributes(perc.var.6)
Variance_List <- c(perc.var.2,perc.var.3,perc.var.4,perc.var.5,perc.var.6)

Variance_List
plot(Variance_List)
#
# Saving four k-means clusters in a list
clus1 <- matrix(names(kmeans4.diabetes$cluster[kmeans4.diabetes$cluster == 1]),
                ncol=1, nrow=length(kmeans4.diabetes$cluster[kmeans4.diabetes$cluster == 1]))
colnames(clus1) <- "Cluster 1"
clus2 <- matrix(names(kmeans4.diabetes$cluster[kmeans4.diabetes$cluster == 2]), 
                ncol=1, nrow=length(kmeans4.diabetes$cluster[kmeans4.diabetes$cluster == 2]))
colnames(clus2) <- "Cluster 2"
clus3 <- matrix(names(kmeans4.diabetes$cluster[kmeans4.diabetes$cluster == 3]), 
                ncol=1, nrow=length(kmeans4.diabetes$cluster[kmeans4.diabetes$cluster == 3]))
colnames(clus3) <- "Cluster 3"
clus4 <- matrix(names(kmeans4.diabetes$cluster[kmeans4.diabetes$cluster == 4]), 
                ncol=1, nrow=length(kmeans4.diabetes$cluster[kmeans4.diabetes$cluster == 4]))
colnames(clus4) <- "Cluster 4"
list(clus1,clus2,clus3,clus4)


# gg Visualizations with new Dataset

diabetes <- read_csv("diabetes.csv")

ndiabetes <- diabetes[-1] %>% na.omit() %>% scale()               

# View the firt 3 rows
head(diabetes, n = 3)

res.dist <- get_dist(diabetes, stand = TRUE, method = "pearson")

# Understand the Distance Between States
fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Lets Try to Find the Optimal Distance
fviz_nbclust(diabetes, kmeans, method = "gap_stat")

set.seed(123)
km.res <- kmeans(diabetes, 3, nstart = 25)
# Visualize
fviz_cluster(km.res, data = diabetes,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

# If your data has outliears , use PAM method
pam.res <- pam(diabetes, 3)
# Visualize
fviz_cluster(pam.res)

# Hierarchial Clusiering
res.hc <- diabetes %>% scale() %>% dist(method = "euclidean") %>%
  hclust(method = "ward.D2")

fviz_dend(res.hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)
# Lets see what the optimal numbers of clusers are
# Compute
res.nbclust <- diabetes %>% scale() %>% NbClust(distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index ="all") 

# Visualize
#fviz_nbclust(res.nbclust, ggtheme = theme_minimal())

# Quality of Clustering

set.seed(123)
# Enhanced hierarchical clustering, cut in 3 groups
res.hc <- diabetes[, -1] %>% scale() %>%
  eclust("hclust", k = 2, graph = FALSE)

# Visualize with factoextra
fviz_dend(res.hc, palette = "jco",
          rect = TRUE, show_labels = FALSE)

#Inspect the silhouette plot:
fviz_silhouette(res.hc)

# Silhouette width of observations
sil <- res.hc$silinfo$widths[, 1:3]

# Objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width'] < 0)
sil[neg_sil_index, , drop = FALSE]


# Reference https://www.datanovia.com/en/blog/types-of-clustering-methods-overview-and-quick-start-r-code/

```

