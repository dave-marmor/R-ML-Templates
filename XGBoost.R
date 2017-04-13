# Import the dataset
data <- read.csv("./Data Files/Churn_Modelling.csv")
data$Exited <- as.factor(as.character(data$Exited))  # Pretend the dep variable was a factor

# Select only the ind & dep variables from the dataset
library(dplyr)
data <- select(data, 4:14)

# Create Model Matrix (xgboost must be matrix/numeric vectors)
X <- model.matrix(Exited ~ . -1, data = data)
y <- as.numeric(data$Exited) - 1  # To convert to 0/1 format

# Partition the data into train and test sets
set.seed(1111)
split <- sample(1:nrow(x), .80 * nrow(x))
X.train <- X[split, ]
X.test <- X[-split, ]
y.train <- y[split]
y.test <- y[-split]

#### No need to scale data ahead of time

# Create XGBoost Model
library(xgboost)
set.seed(2222)
xgb.model <- xgboost(data = X.train, 
                     label = y.train,
                     nrounds = 10,
                     objective = "binary:logistic")

# Predict on the test data
y.prob <- predict(xgb.model, 
                  newdata = X.test)
y.pred <- ifelse(y.prob >= .5, 1, 0)
pred.summary <- data[-split, ]
pred.summary$y.pred <- y.pred
pred.summary$y.prob <- round(y.prob, 2)
head(pred.summary, 10)

# Create Confusion Matrix
conf.mat <- table(Actual = test.y, Predicted = y.pred)
conf.mat

# Accuracy, Precision, Recall, & F1 Score
Accuracy <- sum(diag(conf.mat)) / sum(conf.mat)
Precision <- conf.mat[1] / (conf.mat[1] + conf.mat[3])
Recall <- conf.mat[1] / (conf.mat[1] + conf.mat[2])
F1_Score <- (2 * Precision * Recall) / (Precision + Recall)
round(data.frame(Accuracy, Precision, Recall, F1_Score), 2)
