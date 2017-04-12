# Import the dataset
data <- read.csv("./Data Files/Social_Network_Ads.csv")

# Convert the dependent variable into a factor
data$Purchased <- as.factor(as.character(data$Purchased))

# Partition the data into train and test sets
set.seed(1111)
split <- sample(1:nrow(data), .75 * nrow(data))  # 80% partition
train.set <- data[split, ]
test.set <- data[-split, ]

# Scale the independent variables of both the train and test sets
library(caret)
scale.values <- preProcess(train.set, method = c("center", "scale"))
train.set.scaled <- predict(scale.values, train.set)
test.set.scaled <- predict(scale.values, test.set)

# Apply Kernel PCA
library(kernlab)
kernel.pca <- kpca(~ ., data = train.set.scaled,
                   kernel = "rbfdot",
                   features = 2)  # Number of features to retain
kpca.train <- predict(kernel.pca, train.set.scaled)
kpca.test <- predict(kernel.pca, test.set.scaled)
train.set.kpca <- data.frame(PC1 = kpca.train[, 1],
                            PC2 = kpca.train[, 2],
                            Class = train.set$Purchased)  # Add dependent variable
test.set.kpca <- data.frame(PC1 = kpca.test[, 1],
                           PC2 = kpca.test[, 2],
                           Class = test.set$Purchased)  # Add dependent variable




#### Now create the classification model of your choice ####



### Using a Logistic Regression model:

# Fit Logistic Regression using selected variables
log.model <- glm(formula = Class ~ ., 
                 data = train.set.kpca,
                 family = binomial)

# Predict on the test data
y.prob <- predict(log.model, 
                  newdata = test.set.kpca,
                  type = "response")
y.pred <- ifelse(y.prob >= .5, 1, 0)
pred.summary <- test.set
pred.summary$y.pred <- y.pred
pred.summary$y.prob <- round(y.prob, 2)
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
