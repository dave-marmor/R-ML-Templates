# Import the dataset
data <- read.csv("./Data Files/Wine.csv")

# Convert the dependent variable into a factor
data$Customer_Segment <- as.factor(as.character(data$Customer_Segment))

# Partition the data into train and test sets
set.seed(1111)
split <- sample(1:nrow(data), .80 * nrow(data))  # 80% partition
train.set <- data[split, ]
test.set <- data[-split, ]

# Scale the independent variables of both the train and test sets
library(caret)
scale.values <- preProcess(train.set, method = c("center", "scale"))
train.set.scaled <- predict(scale.values, train.set)
test.set.scaled <- predict(scale.values, test.set)

# Apply LDA
library(MASS)
lda <- lda(Customer_Segment ~ .,
           data = train.set.scaled)
lda.train <- predict(lda, train.set.scaled)
lda.test <- predict(lda, test.set.scaled)
train.set.lda <- data.frame(lda.train$x, class = lda.train$class)
test.set.lda <- data.frame(lda.test$x, class = lda.test$class)


#### Now create the classification model of your choice ####
# Keep in mind that the sample data set uses 3 categories



### Using an SVM model:

# Create the SVM model
library(e1071)
svm.model <- svm(formula = class ~ .,
                 data = train.set.lda,
                 type = "C-classification",  # For regression problems (see SVR)
                 kernel = "linear")  # Can change to "polynomial", "radial", or "sigmoid"

# Predict on the test data
y.pred <- predict(svm.model, newdata = test.set.lda)
pred.summary <- test.set
pred.summary$y.pred <- y.pred
head(pred.summary)

# Create Confusion Matrix
conf.mat <- table(Actual = test.set$Customer_Segment, Predicted = y.pred)
conf.mat
