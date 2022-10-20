colnames(df3_arch)[69:70]<-c('Relationship_Spirituality','Relationship_Religion')
install.packages('factoextra')
install.packages('ClusterR')
install.packages('gmp')
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)
library(ClusterR)
colnames(df3_arch)[69:70]<-c('Relationship_Spirituality','Relationship_Religion')

kclust1<-df3_arch%>%select(`How_express_values-Grace`,`How_express_values-Volunteer`,`How_express_values-community_service`,`How_express_values-Community_organizing`,`How_express_values-Mediation_social_healing`)%>%na.omit(.)
kclust1<-kclust1%>%mutate(Age_Group=as.factor(Age_Group)
													kclust1	<-na.omit(kclust1)		
													scale(kclust1)

													# 

k1<-kmeans(kclust1,centers=3,nstart=20)
str(kclust1)


distance <- get_dist(kclust1)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
fviz_cluster(k1, data = kclust1)

# kclust1a<-df3_arch%>%select(respondent_id,Age_group,Gender,Relationship_Spirituality,Relationship_Religion)%>%na.omit(.)
# kclust1a<-na.omit(kclust1a)
# distance1a <- get_dist(kclust1a)
# fviz_dist(distance1a, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
# 
# kmeans.re<-kmeans()
# 
# Relationship_Spirituality


kclust1 %>%
	as_tibble() %>%
	mutate(cluster = k1$cluster,
				 state = row.names(kclust1)) %>%
	ggplot(aes(`How_express_values-Grace` , `How_express_values-Community_organizing` , color = factor(cluster), label = rownames(kclust1))) +
	geom_text()

k4<-kmeans(kclust1,centers=4,nstart=25)
p3 <- fviz_cluster(k4, geom = "point",  data = kclust1) + ggtitle("k = 4")
p3



set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
	kmeans(kclust1, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
		 type="b", pch = 19, frame = FALSE, 
		 xlab="Number of clusters K",
		 ylab="Total within-clusters sum of squares")

#above equivalent to#
#fviz_nbclust(df, kmeans, method = "wss")
fviz_nbclust(kclust1, kmeans, method = "wss")


#after seeing bend in elbpw method was at 6#
kclust1 %>%
as_tibble() %>%
	mutate(cluster = k1$cluster,
				 state = row.names(kclust1)) %>%
	ggplot(aes(`How_express_values-Grace` , `How_express_values-Community_organizing` , color = factor(cluster), label = rownames(kclust1))) +
	geom_text()

k6<-kmeans(kclust1,centers=6,nstart=25)
p6 <- fviz_cluster(k6, geom = "point",ellipse.type = "convex",data = kclust1) + ggtitle("k = 6")
p6

p6a<-fviz_cluster(k6, geom = "text",data = kclust1) + ggtitle("k = 6")
p6a
#gap statistic#
#set.seed(123)
##clus gap method shows 8#
gap_stat <- clusGap(kclust1, FUN = kmeans, nstart = 25,
										K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)
set.seed(123)
final_test <- kmeans(kclust1, 8, nstart = 25)
print(final_test)


fviz_cluster(final_test, data = kclust1)+ggtitle('gap statistic k-means 8 groups')

kclust2<-kclust1%>%mutate(row.names=respon)
complete <- hclust(get_dist(data_std, method = 'euclidean'), method = 'complete')

kclust2<-df3_arch%>%select(`How_express_values-Grace`,`How_express_values-Volunteer`,`How_express_values-community_service`,`How_express_values-Community_organizing`,`How_express_values-Mediation_social_healing`,Heavy_Magnetist,Heavy_Guardian,Heavy_Adventure,Gender)%>%na.omit(.)
kclust2<-kclust2%>%mutate(Gender=as.factor(Gender))
kclust2	<-na.omit(kclust2) %>% !is.na(kclust2)		
scale(kclust2)													


kclust2<-kclust2%>%mutate(Gender=ifelse(Gender=='Male',1,0))
scale(kclust2)

kclust2<-as.data.frame(kclust2)

complete <- hclust(get_dist(kclust2, method = 'euclidean'), method = 'complete')

plot(complete, main = 'Complete Linkage', xlab = "",
		 sub = "", cex = 0.8)

color in border
rect.hclust(complete, k = 8, border = 3:8)
complete_dend <- as.dendrogram(complete)

# Plot branches above the cut
plot(cut(complete_dend, h=2)$upper, 
		 main="Upper tree of cut at h=2")
# Plot selected branch below the cut
plot(cut(complete_dend, h=1)$lower[[2]], # Change value in 'lower[]' to view different branches/clusters
		 main="Second branch of lower tree with cut at h=1")

# Plot branches above the cut
plot(cut(complete_dend, h=8)$upper, 
		 main="Upper tree of cut at h=10")
# Plot selected branch below the cut
plot(cut(complete_dend, h=8)$lower[[2]], # Change value in 'lower[]' to view different branches/clusters
		 main="Second branch of lower tree with cut at h=8")
plot(complete_dend, main="Full Dendrogram")
rect.hclust(complete, k = 5, border = 3:6) 


gower.dist <- daisy(kclust2, metric = c("gower"))
divisive.clust <- diana(as.matrix(gower.dist), 
												diss = TRUE, keep.diss = TRUE)
plot(divisive.clust, main = "Divisive")

gap_stat_div2 <- clusGap(kclust2, FUN = kmeans, nstart = 25,
										K.max = 10, B = 50)
print(gap_stat_div2, method = "firstmax")
fviz_gap_stat(gap_stat_div2)
set.seed(123)
final_test2 <- kmeans(kclust2, 10, nstart = 25)
print(final_test2)
gower_k_8 <- kmeans(kclust2, , nstart = 25)
print(gower_k_8)
gower_k_6 <- kmeans(kclust2,6, nstart = 25)
gower_k_10 <- kmeans(kclust2,10, nstart = 25)
fviz_cluster(final_test2, data = kclust2)+ggtitle('gap statistic k-means 10 groups')
print(gower_k_10)
fviz_cluster(gower_k_6, data = kclust2)+ggtitle('gap statistic k-means 6 groups gower dissimilarity')

fviz_nbclust(kclust2, kmeans, method = "wss")

kclust2_df2<-kclust2 %>%
	mutate(Cluster = gower_k_10$cluster) %>%
	group_by(Cluster) %>%
	summarise_all("mean")
kclust2_df2
print(kclust2_df2)
View(kclust2_df2)
colnames(kclust2_df2)
ggplot(kclust2_df2,aes(Heavy_Magnetist, Heavy_Guardian, color=Cluster,label=Cluster))+geom_text()+facet_grid(~Gender)
ggplot(kclust2_df2,aes(Heavy_Magnetist, Heavy_Guardian, color=Cluster,label=Cluster))+facet_wrap(~Gender)+geom_text()+ggtitle('Magnetist Vs Guardian distribution with 10 cluster method-Seperated by Gender Group')

###CLuster attempt new, only used 3 with k means can reproduce###
combined_clust4_new<-as.data.frame(combined_clust4)%>%na.omit(.)
set.seed(123)
res.km_new <- kmeans(scale(combined_clust4_new[, -1]), 3, nstart = 10)
res.km_new$cluster

fviz_cluster(res.km_new, data = combined_clust4_new[, -1],
						 palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
						 geom = "point",
						 ellipse.type = "convex", 
						 ggtheme = theme_bw()
)


res.pca_new <- prcomp(combined_clust4_new[, -1],  scale = TRUE)
# Coordinates of individuals
ind.coord_new <- as.data.frame(get_pca_ind(res.pca_new)$coord)
# Add clusters obtained using the K-means algorithm
ind.coord_new$cluster <- factor(res.km_new$cluster)
# Add Species groups from the original data sett
ind.coord_new$respondent_id <- combined_clust4_new$respondent_id
# Data inspection
head(ind.coord_new)
eigenvalue <- round(get_eigenvalue(res.pca_new), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)
library(ggpubr)

# ggscatter(
# 	ind.coord_new, x = "Dim.1", y = "Dim.2", 
# 	color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
# 	shape = "respondent_id", size = 1.5,  legend = "right", ggtheme = theme_bw(),
# 	xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
# 	ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
# ) +
# 	stat_mean(aes(color = cluster), size = 4)


###using gowder distance and dissimilarity matrix for hclust#
gower.dist_new<-daisy(combined_clust4_new[ ,-1], metric = c("gower"))
class(gower.dist_new)

divisive.clust.new <- diana(as.matrix(gower.dist_new), 
												diss = TRUE, keep.diss = TRUE)
plot(divisive.clust.new, main = "Divisive")
###back to kmeans#

set.seed(123)
k6new <- kmeans(scale(combined_clust4_new[, -1]), 6, nstart = 20)
str(k6new)

fviz_cluster(k6new, data = combined_clust4_new[, -1])+ggtitle('Eucliean distance k-means 6 groups')
#silhoutte says 2 groups#
fviz_nbclust(combined_clust4_new[, -1], kmeans, method = "silhouette")
#gap sat#
set.seed(123)
gap_stat_new <- clusGap(combined_clust4_new[, -1], FUN = kmeans, nstart = 25,
										K.max = 10, B = 50)
fviz_gap_stat(gap_stat_new)
#gap stat didnt diverage in ten by k goes down at 6#

set.seed(123)
final_gap6 <- kmeans(combined_clust4_new[, -1], 6, nstart = 25)
print(final_gap6)
fviz_cluster(final_gap6, data = combined_clust4_new)
final_gap6_df<-combined_clust4_new %>%
	mutate(Cluster = final_gap6$cluster) %>%
	group_by(Cluster) %>%
	summarise_all("mean")
print(final_gap6_df)


set.seed(123)
#elbow method below shoes 2 is also optimal#
# function to compute total within-cluster sum of square 
wss <- function(k) {
	kmeans(combined_clust4_new[, -1], k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
		 type="b", pch = 19, frame = FALSE, 
		 xlab="Number of clusters K",
		 ylab="Total within-clusters sum of squares")
# try with 8#
set.seed(123)
final_gap8 <- kmeans(combined_clust4_new[, -1], 8, nstart = 25)
print(final_gap8)
fviz_cluster(final_gap8, data = combined_clust4_new)
final_gap8_df<-combined_clust4_new %>%
	mutate(Cluster = final_gap8$cluster) %>%
	group_by(Cluster) %>%
	summarise_all("mean")
print(final_gap8_df)


set.seed(123)
final_gap2 <- kmeans(combined_clust4_new[, -1], 2, nstart = 25)
print(final_gap2)
fviz_cluster(final_gap2, data = combined_clust4_new)
final_gap2_df<-combined_clust4_new %>%
	mutate(Cluster = final_gap2$cluster) %>%
	group_by(Cluster) %>%
	summarise_all("mean")
print(final_gap2)

set.seed(123)
final_gap3 <- kmeans(combined_clust4_new[, -1], 3, nstart = 25)
print(final_gap3)
fviz_cluster(final_gap3, data = combined_clust4_new)
final_gap3_df<-combined_clust4_new %>%
	mutate(Cluster = final_gap3$cluster) %>%
	group_by(Cluster) %>%
	summarise_all("mean")
print(final_gap3)
##using final gap 6#
final_gap6$cluster


combined_clust4_new2<-cbind(combined_clust4_new,final_gap6$cluster)
clus_res<-combined_clust4_new2%>%select(respondent_id,`final_gap6$cluster`)
clus_res<-clus_res%>%rename(cluster_group=`final_gap6$cluster`)
