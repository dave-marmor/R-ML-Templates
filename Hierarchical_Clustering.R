# Import the dataset
data <- read.csv("./Data Files/Mall_Customers.csv")

# Select only the numeric variables of interest
library(dplyr)
X <- select(data, Annual.Income, Spending.Score)

# Scale the data
library(caret)
scale.values <- preProcess(X, method = c("center", "scale"))
X.scaled <- predict(scale.values, X)

# Create the full hierarchical cluster model
hc.model <- hclust(dist(X.scaled, method = "euclidean"), 
                   method = "ward.D")

# Create the dendrogram to find optimal # of clusters
plot(hc.model)

# Fit hierarchiacal cluster model to dataset
clusters <- cutree(hc.model, 
                  k = 5)  # Desired # of groups

# Assign clusters to the original dataset
data$cluster <- clusters
head(data)

# Visualize the clusters
library(cluster)
clusplot(X.scaled,
         clusters,
         lines = 0,
         color = TRUE,
         labels = 4,
         plotchar = FALSE,
         span = FALSE,
         main = "HC Cluster Plot",
         xlab = "Annual Income",
         ylab = "Spending Score")
