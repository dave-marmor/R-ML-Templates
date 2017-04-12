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

# Apply PCA
library(caret)
library(e1071)
pca <- preProcess(x = train.set.scaled,
                  method = "pca",
                  pcaComp = 2)  # Number of features to retain
train.set.reduced <- predict(pca, train.set.scaled)
test.set.reduced <- predict(pca, test.set.scaled)



#### Now create the classification model of your choice ####
# Keep in mind that the sample data set uses 3 categories



### Using an SVM model:

# Create the SVM model
library(e1071)
svm.model <- svm(formula = Customer_Segment ~ .,
                 data = train.set.reduced,
                 type = "C-classification",  # For regression problems (see SVR)
                 kernel = "linear")  # Can change to "polynomial", "radial", or "sigmoid"

# Predict on the test data
y.pred <- predict(svm.model, newdata = test.set.reduced)
pred.summary <- test.set
pred.summary$y.pred <- y.pred
head(pred.summary)

# Create Confusion Matrix
conf.mat <- table(Actual = test.set$Customer_Segment, Predicted = y.pred)
conf.mat
