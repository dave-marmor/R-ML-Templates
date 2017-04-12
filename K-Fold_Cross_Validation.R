# Import the dataset
library(dplyr)
data <- read.csv("./Data Files/Social_Network_Ads.csv")
data$Purchased <- as.factor(as.character(data$Purchased))
data <- select(data, Age, EstimatedSalary, Purchased)

##### Create the model of your choice first #####

# Using Kernel SVM:
set.seed(1111)
split <- sample(1:nrow(data), .75 * nrow(data))
train.set <- data[split, ]
test.set <- data[-split, ]
library(e1071)
svm.model <- svm(formula = Purchased ~ .,
                 data = train.set,
                 scale = TRUE,  # Assumes the data was not scaled ahead of time
                 type = "C-classification",  # For regression problems (see SVR)
                 kernel = "radial")  # Can change to "polynomial", "radial", or "sigmoid"
y.pred <- predict(svm.model, newdata = test.set)  # Do not pre-scale test data
pred.summary <- test.set
pred.summary$y.pred <- y.pred
head(pred.summary)
conf.mat <- table(Actual = test.set$Purchased, Predicted = y.pred)
conf.mat
Accuracy <- sum(diag(conf.mat)) / sum(conf.mat)
Precision <- conf.mat[1] / (conf.mat[1] + conf.mat[3])
Recall <- conf.mat[1] / (conf.mat[1] + conf.mat[2])
F1_Score <- (2 * Precision * Recall) / (Precision + Recall)
test.set.results <- round(data.frame(Accuracy, Precision, Recall, F1_Score), 2)



##### Now apply k-Fold Cross Validation #####

# Create the k number of folds
library(caret)
set.seed(2222)
folds <- createFolds(y = train.set[[1]], 
                     k = 10)  # For LOOCV use nrow(train.set)
cv.results <- lapply(folds, function(fold) {
  train.fold <- train.set[-fold, ]
  test.fold <- train.set[fold, ]
  svm.model <- svm(formula = Purchased ~ .,
                   data = train.fold,
                   scale = TRUE,
                   type = "C-classification",
                   kernel = "radial")
  y.pred <- predict(svm.model, newdata = test.fold)
  conf.mat <- table(Actual = test.fold$Purchased,
                    Predicted = y.pred)
  Accuracy <- sum(diag(conf.mat)) / sum(conf.mat)
  Precision <- conf.mat[1] / (conf.mat[1] + conf.mat[3])
  Recall <- conf.mat[1] / (conf.mat[1] + conf.mat[2])
  F1_Score <- (2 * Precision * Recall) / (Precision + Recall)
  round(data.frame(Accuracy, Precision, Recall, F1_Score), 2)
})
cv.results <- data.frame(matrix(unlist(cv.results), length(folds), 4))
names(cv.results) <- c("Accuracy", "Precision", "Recall", "F1_Score")
apply(cv.results, 2, mean)
test.set.results  # For comparison
