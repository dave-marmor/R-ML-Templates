# Import the dataset
data <- read.csv("./Data Files/Social_Network_Ads.csv")

# Convert the dependent variable into a factor
data$Purchased <- as.factor(as.character(data$Purchased))

# Set the seed
set.seed(1111)

# Partition the data into train and test sets
split <- sample(1:nrow(data), .75 * nrow(data))
train.set <- data[split, ]
test.set <- data[-split, ]

# Create the Naive Bayes model (do not need to scale data)
library(e1071)
bayes.model <- naiveBayes(formula = Purchased ~ Age + EstimatedSalary,
                          data = train.set)

# Predict on the test data
y.pred <- predict(bayes.model, newdata = test.set)
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
