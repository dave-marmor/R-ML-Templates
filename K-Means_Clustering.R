# Import the dataset
data <- read.csv("./Data Files/Mall_Customers.csv")

# Select only the numeric variables of interest
library(dplyr)
X <- select(data, Annual.Income, Spending.Score)

# Scale the data
library(caret)
scale.values <- preProcess(X, method = c("center", "scale"))
X.scaled <- predict(scale.values, X)

# Create the K-Means Model & determine the optimal # of clusters
## Skip this step if the # of clusters is already known
set.seed(1111)
wcss <- NULL
for (i in 1:10) {
  wcss[i] <- sum(kmeans(X.scaled, i)$withinss)
}
plot(x = 1:10, 
     y = wcss,
     type = "b",
     xlab = "# of Clusters",
     ylab = "WCSS")
## Now determine the optimal number of clusters from the WCSS plot

# Create the K-Means Model (Use the # of clusters found above)
set.seed(2222)
kmeans.model <- kmeans(X.scaled, 
                       centers = 5,
                       iter.max = 300,
                       nstart = 10)

# Assign clusters to the original dataset
X$cluster <- kmeans.model$cluster
head(X)

# Visualize the clusters
library(cluster)
clusplot(X.scaled,
         kmeans.model$cluster,
         lines = 0,
         color = TRUE,
         labels = 4,
         plotchar = FALSE,
         span = FALSE,
         main = "K-Means Cluster Plot",
         xlab = "Annual Income",
         ylab = "Spending Score")

