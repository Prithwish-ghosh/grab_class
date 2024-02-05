grb_dataset = read.csv(file.choose())

grb_dataset = log(grb_dataset)

grb_dataset$H32 = grb_dataset$F3/grb_dataset$F2
grb_dataset$FT = grb_dataset$F1+ grb_dataset$F2 + grb_dataset$F3 + grb_dataset$F4
grb_dataset$H321 = grb_dataset$F3/(grb_dataset$F1 + grb_dataset$F2)

head(grb_dataset)
sum(is.na(grb_dataset))

grb_dataset = na.omit(grb_dataset)

df = grb_dataset[,-c(1,2, 5:9,11)]


# Replace Inf in data by N
df <- do.call(data.frame,                    A
                   lapply(df,
                          function(x) replace(x, is.infinite(x), NA)))

head(df)
df = na.omit(df)
dim(df)

sum(is.na(df))

library(cluster)
library(factoextra)
library(FactoMineR)
library(NbClust)
library(cluster)
library(ClusterR)
library(mclust)


# Elbow method
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

set.seed(123)

nb <- NbClust(df, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans", index ="all")

nb$Best.partition

final.kmeans <- kmeans(df, 3)
fviz_cluster(final.kmeans, data = df) + theme_minimal() + ggtitle("k = 3")

final.kmeans$size

with(df, pairs(df, col = c(1:5)[final.kmeans$cluster]))

library(MASS)

set.seed(124)
group = c(rep (1, 409), rep (2, 452), rep(3, 738))
head(group) 

head(df)

tab1 = table(predict(discr)$class, group,dnn = c('Actual Group','Predicted Group'))
tab1

#Maha distance

mahalanobis_distance <- function(x, center, cov_matrix) {
  diff_vector <- x - center
  inv_cov_matrix <- solve(cov_matrix)
  mahalanobis_dist <- sqrt(t(diff_vector) %*% inv_cov_matrix %*% diff_vector)
  return(mahalanobis_dist)
}

# K-means algorithm with Mahalanobis distance
kmeans_mahalanobis <- function(data, k, max_iter = 100, tol = 1e-4) {
  n <- nrow(data)
  m <- ncol(data)
  
  # Initialize cluster centroids randomly
  centroids <- data[sample(1:n, k), ]
  
  for (iter in 1:max_iter) {
    # Assign each data point to the nearest centroid based on Mahalanobis distance
    distances <- matrix(0, n, k)
    for (i in 1:k) {
      distances[, i] <- apply(data, 1, mahalanobis_distance, center = centroids[i, ], cov_matrix = cov(data))
    }
    cluster_assignments <- apply(distances, 1, which.min)
    
    # Update centroids
    new_centroids <- matrix(0, k, m)
    for (i in 1:k) {
      if (sum(cluster_assignments == i) > 0) {
        new_centroids[i, ] <- colMeans(data[cluster_assignments == i, ])
      } else {
        # If a cluster has no points, keep the centroid unchanged
        new_centroids[i, ] <- centroids[i, ]
      }
    }
    
    # Check for convergence
    if (sum(sqrt(rowSums((new_centroids - centroids)^2))) < tol) {
      break
    }
    
    centroids <- new_centroids
  }
  
  # Return cluster assignments and centroids
  return(list(cluster_assignments = cluster_assignments, centroids = centroids))
}

set.seed(123)
df_mat = as.matrix(df)
df_mat

result <- kmeans_mahalanobis(df_mat, k)
summary(result$cluster_assignments)

result$cluster_assignments

r <- table(result$cluster_assignments)
r

group = c(rep (1, 654), rep (2, 535), rep(3, 410))
head(group) 

discr = lda(df,group)
tab1 = table(predict(discr)$class, group,dnn = c('Actual Group','Predicted Group'))
tab1

set.seed(122)
fcm = cmeans(df, 3)
fcm$size

group = c(rep (1, 442), rep (2, 715), rep(3, 442))
head(group) 

discr = lda(df,group)
tab1 = table(predict(discr)$class, group,dnn = c('Actual Group','Predicted Group'))
tab1

head(df)
df_optimal_mc <- Mclust(df)
fviz_mclust_bic(df_optimal_mc, legend = "bottom right")

fit_gm = Mclust(df, 5)
summary(df_optimal_mc)

gmm = GMM(df, 5, dist_mode = "maha_dist", seed_mode = "random_subset", 
          km_iter = 10,em_iter = 10, verbose = F)

predicted_clusters = predict(gmm, newdata = df)
predicted_clusters
result <- table(predicted_clusters)

# Print the result
print(result)
summary(fit_gm)
summary(gmm)

discr = lda(df,group)
tab1 = table(predict(discr)$class, group,dnn = c('Actual Group','Predicted Group'))
tab1
mda = mclust::MclustDA(data = df, fit_gm$classification)
summary(mda)

mdm = mclust::MclustDA(data = df, predicted_clusters)
summary(mdm)

fit_gm$classification
plot(fit_gm, what = "classification")

