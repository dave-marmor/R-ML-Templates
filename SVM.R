# Import the dataset
data <- read.csv("./Data Files/Social_Network_Ads.csv")

# Convert the dependent variable into a factor
data$Purchased <- as.factor(as.character(data$Purchased))

# Partition the data into train and test sets
set.seed(1111)
split <- sample(1:nrow(data), .75 * nrow(data))
train.set <- data[split, ]
test.set <- data[-split, ]

# Create the SVM model
## Method 1: Data will be scaled during the process
library(e1071)
svm.model <- svm(formula = Purchased ~ Age + EstimatedSalary,
                 data = train.set,
                 scale = TRUE,  # Assumes the data was not scaled ahead of time
                 type = "C-classification",  # For regression problems (see SVR)
                 kernel = "linear")  # Can change to "polynomial", "radial", or "sigmoid"
## Method 2: Use a Matrix to reduce load
library(e1071)
X.train <- model.matrix(Purchased ~ . -1, data = train.set)
y.train <- train.set$Purchased
X.test <- model.matrix(Purchased ~ . -1, data = test.set)
y.test <- test.set$Purchased
svm.model <- svm(x = X.train,
                 y = y.train,
                 scale = TRUE,  # Assumes the data was not scaled ahead of time
                 type = "C-classification",  # For regression problems (see SVR)
                 kernel = "linear")  # Can change to "polynomial", "radial", or "sigmoid"

# Predict on the test data
## Using Method 1 (from above)
y.pred <- predict(svm.model, newdata = test.set)  # Do not pre-scale test data
pred.summary <- test.set
pred.summary$y.pred <- y.pred
head(pred.summary)
## Using Method 2 (from above)
y.pred <- predict(svm.model, newdata = X.test)  # Do not pre-scale test data
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
