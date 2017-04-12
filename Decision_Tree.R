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

# Create the Decision Tree model (do not need to scale data)
library(rpart)
tree <- rpart(formula = Purchased ~ Age + EstimatedSalary,
              data = train.set)

# Predict on the test data
y.prob <- predict(tree, newdata = test.set)  # This gives the probabilities
y.pred <- predict(tree, newdata = test.set, type = "class")
pred.summary <- test.set
pred.summary$y.prob <- round(y.prob[, 2], 2)
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

# Plotting the Decision Tree
library(rpart.plot)
prp(tree, varlen = 8)  # Varlen controls length of variable names

# To snip the tree down (experiment with this)
new.tree <- prp(tree, snip = TRUE)$obj  # Interactively trim on plot
prp(new.tree)
