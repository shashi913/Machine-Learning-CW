library(readxl) #for read xlsx file
library(factoextra)  #for determining the relevant number of clusters
library(NbClust)


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





#####################additional try#####################################

# Detect outliers using Z-score
z_scores <- scale(wine_features)

# Define a threshold for outliers
threshold <- 3

# Find indices of outliers
outlier_indices <- which(abs(z_scores) > threshold, arr.ind = TRUE)

# Print outlier rows
outlier_rows <- wine_data[outlier_indices[,1], ]
print(outlier_rows)

# Remove outliers
cleaned_data <- wine_features[-outlier_indices[,1], ]

# Check dimensions of original dataset
dim(wine_data)

# Check dimensions of scaled features
dim(scaled_features)


# Check dimensions of original dataset
dim(wine_data)

# Check dimensions of cleaned data
dim(cleaned_data)



#############################################












#Determine the number of cluster centres
set.seed(26)

# NbClust method
clusterNo_method1=NbClust(cleaned_data_boxplot,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")
print(clusterNo_method1)

# Elbow Method
fviz_nbclust(cleaned_data_boxplot, kmeans, method = 'wss')

# Gap Statistic method
fviz_nbclust(cleaned_data_boxplot, kmeans, method = 'gap_stat')

# Silhouette Method
fviz_nbclust(cleaned_data_boxplot, kmeans, method = 'silhouette')



# kmeans clustering K=2
install.packages("cluster")
library(cluster)


k = 2
kmeans_wine_2 = kmeans(cleaned_data_boxplot, centers = k, nstart = 10)
kmeans_wine_2


#Illustration of the clusters
fviz_cluster(kmeans_wine_2, data = cleaned_data_boxplot)


# Internal evaluation 
wss_2 = kmeans_wine_2$tot.withinss 
bss_2 = kmeans_wine_2$betweenss

wss_2
bss_2


# kmeans clustering K = 3

k = 3
kmeans_wines_3 = kmeans(cleaned_data_boxplot, centers = k, nstart = 10)
kmeans_wines_3


#Illustration of the clusters
fviz_cluster(kmeans_wines_3, data = cleaned_data_boxplot)


# Internal evaluation 
wss_3 = kmeans_wines_3$tot.withinss 
bss_3 = kmeans_wines_3$betweenss

wss_3
bss_3




# The silhouette analysis 

sil <- silhouette(kmeans_wine_2$cluster, dist(cleaned_data_boxplot))
fviz_silhouette(sil)



# The silhouette analysis 

sil <- silhouette(kmeans_wines_3$cluster, dist(cleaned_data_boxplot))
fviz_silhouette(sil)


