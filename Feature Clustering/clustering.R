# All the required libraries
require(readr)
require(factoextra)
require(NbClust)
require(cluster)

# Load the csv data
imputed_responses <- read_csv("../Data/imputed_responses.csv")

# Preparing the Data
music.group <- imputed_responses[,2:19]
# Hierarchial clustering
hclust.music <- hclust(dist(music.group, method = "euclidean"), method = "complete")
plot(hclust.music, hang = -0.01, cex = 0.7)

# Hierarchial Clustering to find optimal cluster value
nc <- NbClust(music.group, distance = "euclidean", min.nc = 2, max.nc = 15, method = "complete")

# Cutting the dendrogram at optimal cluster number
fit_music <- cutree(hclust.music, k = 2)
table(fit_music)

# Plotting the Dendrogram
plot(hclust.music)
rect.hclust(hclust.music, k = 2, border = 'blue')

# Plotting the clusters data points
clusplot(music.group, fit_music, color = T, shade = T, labels = 4, main = 'Music cluster', lines = 0)

# Understanding the Cluster Data segregation based on mean
aggregate(music.group, by=list(cluster = fit_music), mean)
# Understanding the Cluster Data segregation based on median
aggregate(music.group, by=list(cluster = fit_music), median)
