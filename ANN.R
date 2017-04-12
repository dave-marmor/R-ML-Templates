# Import the dataset
data <- read.csv("./Data Files/Churn_Modelling.csv")

# Select only the ind & dep variables from the dataset
library(dplyr)
data <- select(data, 4:14)

# Convert the categorical variables to factors
data <- mutate(data, 
               Geography = as.factor(as.character(Geography)),
               Gender = as.factor(as.character(Gender)),
               HasCrCard = as.factor(as.character(HasCrCard)),
               IsActiveMember = as.factor(as.character(IsActiveMember)),
               Exited = as.factor(as.character(Exited)))

# Partition the data into train and test sets
set.seed(1111)
split <- sample(1:nrow(data), .80 * nrow(data))
train.set <- data[split, ]
test.set <- data[-split, ]

# Scale the train and test sets
library(caret)
scale.values <- preProcess(train.set, method = c("center", "scale"))
train.set.scaled <- predict(scale.values, train.set)
test.set.scaled <- predict(scale.values, test.set)
train.set.scaled$Exited <- train.set$Exited
test.set.scaled$Exited <- test.set$Exited

# Connect to the h2o instance for better computing power
library(h2o)
h2o.init(nthreads = -1)  # Just always use nthreads = -1

# Convert data.frame into an h2o.frame dataset
h2o.train.set.scaled <- as.h2o(train.set.scaled)
h2o.test.set.scaled <- as.h2o(test.set.scaled)

# Create the ANN model
ann.model <- h2o.deeplearning(y = "Exited",
                              training_frame = h2o.train.set.scaled,
                              activation = "Rectifier",
                              hidden = c(6, 6),
                              epochs = 100,
                              train_samples_per_iteration = -2)

# Predict on the test data
y.prob <- h2o.predict(ann.model, newdata = h2o.test.set.scaled)
y.pred <- as.vector(ifelse(y.prob$p1 >= .5, 1, 0))
pred.summary <- test.set
pred.summary$y.pred <- y.pred
pred.summary$y.prob <- y.prob$p1
head(pred.summary)

# Create Confusion Matrix
conf.mat <- table(Actual = test.set$Exited, Predicted = y.pred)
conf.mat

# Accuracy, Precision, Recall, & F1 Score
Accuracy <- sum(diag(conf.mat)) / sum(conf.mat)
Precision <- conf.mat[1] / (conf.mat[1] + conf.mat[3])
Recall <- conf.mat[1] / (conf.mat[1] + conf.mat[2])
F1_Score <- (2 * Precision * Recall) / (Precision + Recall)
round(data.frame(Accuracy, Precision, Recall, F1_Score), 2)

# Shutdown the h2o instance
h2o.shutdown()
Y