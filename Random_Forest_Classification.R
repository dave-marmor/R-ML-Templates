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

# Create the Random Forest model (do not need to scale data)
## Method 1
library(randomForest)
set.seed(2222)
rf.tree <- randomForest(formula = Purchased ~ Age + EstimatedSalary,
                        data = train.set,
                        ntree = 100) # Tune this number
## Method 2 (sometimes this works better with NLP)
rf.tree <- randomForest(x = train.set[-5],
                        y = train.set$Purchased,
                        ntree = 100) # Tune this number

# Predict on the test data
y.pred <- predict(rf.tree, newdata = test.set)
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

# Plotting the Random Forest to trim
plot(rf.tree)



# Tune the tree adjusting for the ntree argument
test.range <- seq(from = 10, to = 2000, by = 10)
analysis <- data.frame(ntree = NULL,
                       accuracy = NULL)
for (i in test.range) {
  set.seed(2222)
  tmp.rf.tree <- randomForest(formula = Purchased ~ Age + EstimatedSalary,
                              data = train.set,
                              ntree = i)
  tmp.y.pred <- predict(tmp.rf.tree, newdata = test.set)
  tmp.conf.mat <- table(test.set$Purchased, tmp.y.pred)
  tmp.accuracy <- sum(diag(tmp.conf.mat)) / nrow(test.set)
  tmp.analysis <- data.frame(ntree = i,
                             accuracy = tmp.accuracy)
  analysis <- rbind(analysis, tmp.analysis)
}
head(analysis)
analysis[which.max(analysis$accuracy), ]

