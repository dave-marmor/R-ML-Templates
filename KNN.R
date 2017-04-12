# Import the dataset
data <- read.csv("./Data Files/Social_Network_Ads.csv")

# Set the seed
set.seed(1111)

# Convert the dependent variable into a factor
data$Purchased <- as.factor(as.character(data$Purchased))

# Partition the data into train and test sets
split <- sample(1:nrow(data), .75 * nrow(data))
train.set <- data[split, ]
test.set <- data[-split, ]

# Scale the independent variables of both the train and test sets
library(caret)
scale.values <- preProcess(train.set, method = c("center", "scale"))
train.set.scaled <- predict(scale.values, train.set)
test.set.scaled <- predict(scale.values, test.set)

# Create the KNN model and predict on the test data
library(class)
y.pred <- knn(train = train.set.scaled,
              test = test.set.scaled,
              cl = train.set$Purchased,  # Vector of the dependent variable
              k = 5)  # Set to whatever seems appropriate
pred.summary <- test.set
pred.summary$y.pred <- y.pred
head(pred.summary)

# Create Confusion Matrix
conf.mat <- table(Actual = test.set$Purchased, Predicted = y.pred)
conf.mat

# Accuracy, Precision, Recall, & F1 Score
Accuracy <- sum(diag(conf.mat)) / sum(conf.mat)
Precision <- conf.mat[1] / (conf.mat[1] + conf.mat[3])
Recall <- conf.mat[1] / (conf.mat[1] + conf.mat[2])
F1_Score <- (2 * Precision * Recall) / (Precision + Recall)
round(data.frame(Accuracy, Precision, Recall, F1_Score), 2)
