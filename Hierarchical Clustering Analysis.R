# Install and load necessary packages
# install.packages("factoextra")
# install.packages("cluster")
library(factoextra)
library(cluster)

# Load dataset
data <- read.csv("Fruit_Prices_2022.csv")

# Convert non-numeric columns to numeric
data <- data.frame(lapply(data, function(x) {
  if(is.factor(x) || is.character(x)) {
    as.numeric(as.factor(x))
  } else {
    x
  }
}))

# Alternatively, remove non-numeric columns
# data <- data[, sapply(data, is.numeric)]

# Scale the data
data_scaled <- scale(data)

# Compute the distance matrix
dist_matrix <- dist(data_scaled, method = "euclidean")

# Perform hierarchical clustering
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot the dendrogram
plot(hc, cex = 0.6, hang = -1)

# Cut the dendrogram into 3 clusters
clusters <- cutree(hc, k = 3)

# Add cluster membership to the original data
data$cluster <- clusters

# Visualize the clusters
fviz_cluster(list(data = data_scaled, cluster = clusters))