# Import the dataset & load the e1071 package
data <- read.csv("./Data Files/Position_Salaries.csv")

# Subset data so that only the dependent and independent variables remain
data <- data[, 2:3]

# Set the seed
set.seed(1111)

# Partition the data into train and test sets
split <- sample(1:nrow(data), .80 * nrow(data))  # Method 1
train.set <- data[split, ]
test.set <- data[-split, ]

# Create SVR model (Use "eps-regression")
library(e1071)
svr.fit <- svm(formula = Salary ~ ., 
               data = train.set,
               scale = TRUE,
               type = "eps-regression")

# Predict on the test data
y.pred <- predict(svr.fit, newdata = test.set)
pred.summary <- test.set
pred.summary$y.pred <- y.pred
head(pred.summary)
