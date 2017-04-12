# Import the dataset & load the rpart package
library(rpart)
data <- read.csv("./Data Files/Position_Salaries.csv")

# Subset data so that only the dependent and independent variables remain
data <- data[, 2:3]

# Set the seed
set.seed(1111)

# Partition the data into train and test sets
split <- sample(1:nrow(data), .80 * nrow(data))  # Method 1
train.set <- data[split, ]
test.set <- data[-split, ]

# Create regression tree model (Use "eps-regression")
tree.fit <- rpart(formula = Salary ~ ., 
                 data = train.set,
                 control = rpart.control(minsplit = 1))

# Predict on the test data
y.pred <- predict(tree.fit, newdata = test.set)
pred.summary <- test.set
pred.summary$y.pred <- y.pred
head(pred.summary)

# Visual representation (only works for 1 predictor)
x.pred <- seq(min(data$Level), max(data$Level), .01)
library(ggplot2)
ggplot() + 
  geom_point(data = data, aes(x = Level, y = Salary), color = "blue", shape = 1) +
  geom_line(aes(x = x.pred, 
                y = predict(rf.fit, data.frame(Level = x.pred))),
            color = "red")