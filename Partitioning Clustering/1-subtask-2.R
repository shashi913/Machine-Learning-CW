
library(readxl) #for read xlsx file
library(factoextra)  #for determining the relevant number of clusters
library(NbClust)
library(cluster)


# Load the dataset
wine_data <- read_excel("Whitewine_v6.xlsx")
head(wine_data)

# Extract the first 11 attributes
wine_features <- wine_data[, 1:11]
head(wine_features)

#The structure of data set
str(wine_data)

#Checking missing values
sum(is.na(wine_data))


#checking number of rows and columns (before handling outliers)
dim(wine_data)






#boxplot


#View the outliers using using boxplot
boxplot(wine_data[1:11])
boxplot(wine_features)

#Generating box plot for each variable to identify outliers in the data set and remove them 
for (vari in 1:11) {
  
  #Detect outliers using boxplot
  outliers <- boxplot(wine_data[[vari]], plot = FALSE)$out
  
  
  #Remove outliers
  wine_data <- wine_data[!wine_data[[vari]] %in% outliers, , drop = FALSE]
}

#Checking outliers removal
boxplot(wine_data[1:11])

dim(wine_data)

# Scale the features
scaled_features <- scale(wine_features)


#Scaling data
cleaned_data_boxplot <- scale(wine_data[, -c(12)])
dim(cleaned_data_boxplot)



# Scale the features
scaled_features <- scale(wine_features)


#Scaling data
cleaned_data_boxplot <- scale(wine_data[, -c(12)])




#PCA
pca_result <- prcomp(cleaned_data_boxplot, scale = FALSE)
summary(pca_result)

# Extract eigenvalues
eigenvalues <- pca_result$sdev^2
print(eigenvalues)

#Extract eigenvectors
eigenvectors   <- -pca_result$rotation
print(eigenvectors)

# obtain the principal components scores 
pca_result$x <- - pca_result$x
head(pca_result$x) #print first six rows of the PC

# Calculate the cumulative variance explained by each principal component 
cumulative_value <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2)) 
print(cumulative_value) 

# Choose the number of PCs that provide at least cumulative score > 85%
max_pc <- min(which(cumulative_value > 0.85)) 
print(max_pc)

# since cumulative score > 85%, new transformed dataset have 7 PCs as attributes
transformed_data <- pca_result$x[,1:max_pc]
head(transformed_data)









#Determine the number of cluster centres
set.seed(26)

# NbClust method
clusterNo_method1=NbClust(transformed_data,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")
print(clusterNo_method1)

# Elbow Method
fviz_nbclust(transformed_data, kmeans, method = 'wss')

# Gap Statistic method
fviz_nbclust(transformed_data, kmeans, method = 'gap_stat')

# Silhouette Method
fviz_nbclust(transformed_data, kmeans, method = 'silhouette')



# kmeans clustering K=2

k = 2
kmeans_wines = kmeans(transformed_data, centers = k, nstart = 10)
kmeans_wines


#Illustration of the clusters
fviz_cluster(kmeans_wines, data = transformed_data)


# Internal evaluation 
wss = kmeans_wines$tot.withinss 
bss = kmeans_wines$betweenss

wss
bss




# The silhouette analysis 

sil <- silhouette(kmeans_wines$cluster, dist(cleaned_data_boxplot))
fviz_silhouette(sil)




# Calinski-Harabasz Index
ch_values <- NULL
for (i in 1:10) {
  kmeans_Wines_cslinski = kmeans(transformed_data, centers = i, nstart = 10)
  ch <- calinhara(transformed_data,kmeans_Wines_cslinski$cluster)
  ch_values <- c(ch_values,ch)
  
}
# plot the Calinski-Harabasz Index values for different k values.
plot(1:10, ch_values, type = "b", xlab = "Number of Clusters", ylab = "Calinski-Harabasz Index")

max(ch_values[2])

