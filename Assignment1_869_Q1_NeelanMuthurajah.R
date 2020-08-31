# [Neelan,Muthurajah]
# [20195484]
# [Masters of Management Analytics]
# [Section 2]
# [MMA 869]
# [Aug 16th 2020]


# Submission to Question [1], Part [a,b,c]

#Imports R-packages that are needed to run code below. If user does not have package installed, pacman will install package for the user. 
if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("dplyr","tidyr","lubridate","tidyverse","dbscan","factoextra","ggplot2", "fpc","cluster") 

#Import jewellery customers dataset
jewellery_customer <- read.csv(file.choose(),header=T, sep=",")

#Explore data structure
head(jewellery_customer)

#Scaled the data as all features are numeric + have extremely different ranges. Also created a new copy of the dataset strictly for clustering purposes
jewellery_customer2<-scale(jewellery_customer)

##################################################################################################################
#KMEANS
#kmeans using different choice of clusters. Choice of distance metric was euclidean by default.  
set.seed(20)
km.res_2Clusters<-kmeans(jewellery_customer2,2,nstart=10)
km.res_3Clusters<-kmeans(jewellery_customer2,3,nstart=10)
km.res_4Clusters<-kmeans(jewellery_customer2,4,nstart=10)
km.res_5Clusters<-kmeans(jewellery_customer2,5,nstart=10)
km.res_6Clusters<-kmeans(jewellery_customer2,6,nstart=10)

#Extract WCSS from results above 
clusters<-c(2,3,4,5,6)
WCSS<-c(1020,384,189,66.4,61)

#Create dataframe of clusters + WSS 
WCSS_DataFrame<-data.frame(clusters,WCSS)

#Create ElbowPlot. From the elbowplot it seems that 5 clusters is optimal
ElbowPlot<-ggplot(WCSS_DataFrame,aes(x=clusters,y=WCSS))+geom_line()
ElbowPlot

#Calculate silhouette coefficient based on defined kmeans above (line 29 to 33)
ss_kmeans_1 <- silhouette(km.res_2Clusters$cluster, dist(jewellery_customer2))
mean(ss_kmeans_1[, 3])

ss_kmeans_2 <- silhouette(km.res_3Clusters$cluster, dist(jewellery_customer2))
mean(ss_kmeans_2[, 3])

ss_kmeans_3 <- silhouette(km.res_4Clusters$cluster, dist(jewellery_customer2))
mean(ss_kmeans_3[, 3])

ss_kmeans_4 <- silhouette(km.res_5Clusters$cluster, dist(jewellery_customer2))
mean(ss_kmeans_4[, 3])

ss_kmeans_5 <- silhouette(km.res_6Clusters$cluster, dist(jewellery_customer2))
mean(ss_kmeans_5[, 3])

#As an alternative, user can use package "factoextra" to create Silhoutette/WCSS Plot rather than running line 29 to 60
fviz_nbclust(jewellery_customer2, kmeans, method = "wss")
fviz_nbclust(jewellery_customer2, kmeans, method = "silhouette")

#Append kmeans clusters to original data (km.res_5Clusters had the highest silhouette coefficient so we will use that)
jewellery_customer<-cbind(jewellery_customer,cluster_KMeans=km.res_5Clusters$cluster)


##################################################################################################################
#DBSCANS
#DBScan considers any cluster with 0 as noise 
#To determine minpoints use # of features + 1 (In this case 4 features + 1)

#Optimal eps value is 0.4-0.5 based on plot from fpc package
dbscan_plot<-kNNdistplot(jewellery_customer2,k=5)

set.seed(21)
dbscan_1<-dbscan(jewellery_customer2,eps=0.3,MinPts=5)
dbscan_2<-dbscan(jewellery_customer2,eps=0.4,MinPts=5)
dbscan_3<-dbscan(jewellery_customer2,eps=0.5,MinPts=5)
dbscan_4<-dbscan(jewellery_customer2,eps=0.6,MinPts=5)
dbscan_5<-dbscan(jewellery_customer2,eps=0.7,MinPts=5)
dbscan_6<-dbscan(jewellery_customer2,eps=0.8,MinPts=5)

#Calculate silhouette coefficient based on defined dbscans above (line 79 to 84)
ss_dbscan_1 <- silhouette(dbscan_1$cluster, dist(jewellery_customer2))
mean(ss_dbscan_1[, 3])

ss_dbscan_2 <- silhouette(dbscan_2$cluster, dist(jewellery_customer2))
mean(ss_dbscan_2[, 3])

ss_dbscan_3 <- silhouette(dbscan_3$cluster, dist(jewellery_customer2))
mean(ss_dbscan_3[, 3])

ss_dbscan_4 <- silhouette(dbscan_4$cluster, dist(jewellery_customer2))
mean(ss_dbscan_4[, 3])

ss_dbscan_5 <- silhouette(dbscan_5$cluster, dist(jewellery_customer2))
mean(ss_dbscan_5[, 3])

ss_dbscan_6 <- silhouette(dbscan_6$cluster, dist(jewellery_customer2))
mean(ss_dbscan_6[, 3])

#Append dbscan clusters to original data (epi=0.5 and minpts=5 produced the highest silhouette coefficient so we will use that)
jewellery_customer<-cbind(jewellery_customer,cluster_DBSCAN=dbscan_3$cluster)

##################################################################################################################
#HIERARCHICAL
#Define distance metric. In this case the chosen metric was euclidean
dist_mat<-dist(jewellery_customer2,method="euclidean")

hclust_avg<-hclust(dist_mat,method='average')
plot(hclust_avg)

set.seed(22)
cut_avg_2<-cutree(hclust_avg,k=2)
cut_avg_3<-cutree(hclust_avg,k=3)
cut_avg_4<-cutree(hclust_avg,k=4)
cut_avg_5<-cutree(hclust_avg,k=5)
cut_avg_6<-cutree(hclust_avg,k=6)

#Calculate silhouette coefficient based on defined hierarchical cuts above (line 117 to 121)
ss_hierarchy_1 <- silhouette(cut_avg_2, dist(jewellery_customer2))
mean(ss_hierarchy_1[, 3])

ss_hierarchy_2 <- silhouette(cut_avg_3, dist(jewellery_customer2))
mean(ss_hierarchy_2[, 3])

ss_hierarchy_3 <- silhouette(cut_avg_4, dist(jewellery_customer2))
mean(ss_hierarchy_3[, 3])

ss_hierarchy_4 <- silhouette(cut_avg_5, dist(jewellery_customer2))
mean(ss_hierarchy_4[, 3])

ss_hierarchy_5 <- silhouette(cut_avg_6, dist(jewellery_customer2))
mean(ss_hierarchy_5[, 3])

#One alternative is to use package "factoextra" to create Silhoutette/WCSS Plot rather than running line 111 to 137
fviz_nbclust(jewellery_customer2, hcut, method = "wss")
fviz_nbclust(jewellery_customer2, hcut, method = "silhouette")

#Append hierarchical clusters to original data (cut_avg_5 had the highest silhouette coefficient so we will use that)
jewellery_customer<-cbind(jewellery_customer,cluster_Hierarchy=cut_avg_5)

##########Summary Statistics of clusters based on 3 different algorithms##############################

SummaryStatistics_Kmeans<-jewellery_customer%>%
  group_by(cluster_KMeans)%>%
  summarise(Size=n(),Avg_Age=mean(Age),Avg_Income=mean(Income),Avg_SpendingScore=mean(SpendingScore),Avg_Savings=mean(Savings))

SummaryStatistics_Hierarchy<-jewellery_customer%>%
  group_by(cluster_Hierarchy)%>%
  summarise(Size=n(),Avg_Age=mean(Age),Avg_Income=mean(Income),Avg_SpendingScore=mean(SpendingScore),Avg_Savings=mean(Savings))

SummaryStatistics_DBSCAN<-jewellery_customer%>%
  group_by(cluster_DBSCAN)%>%
  summarise(Size=n(),Avg_Age=mean(Age),Avg_Income=mean(Income),Avg_SpendingScore=mean(SpendingScore),Avg_Savings=mean(Savings))


