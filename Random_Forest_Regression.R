# Import the dataset
data <- read.csv("./Data Files/Position_Salaries.csv")

# Subset data so that only the dependent and independent variables remain
data <- data[, 2:3]

# Set the seed
set.seed(1111)

# Partition the data into train and test sets
split <- sample(1:nrow(data), .80 * nrow(data))  # Method 1
train.set <- data[split, ]
test.set <- data[-split, ]

# Create decision tree regression model
## Method 1
library(randomForest)
set.seed(1)
reg.tree <- randomForest(formula = Salary ~ Level, 
                         data = train.set, 
                         ntree = 100)  # Pick a big number
## Method 2
X.train <- train.set[1]  # Split the data into an x matrix and y vector
y.train <- train.set$Salary
X.test <- test.set[1]
y.test <- test.set$Salary
set.seed(1111)
reg.tree <- randomForest(x = X.train,
                         y = y.train,
                         ntree = 100)  # Pick a big number

# Predict on the test data
y.pred <- predict(reg.tree, newdata = test.set)
pred.summary <- test.set
pred.summary$y.pred <- y.pred
head(pred.summary)



# Visual representation (only works for 1 predictor)
library(ggplot2)
x.pred <- seq(min(data$Level), max(data$Level), .01)
ggplot() + 
  geom_point(data = data, aes(x = Level, y = Salary), color = "blue", shape = 1) +
  geom_line(aes(x = x.pred, 
                y = predict(rf.fit, data.frame(Level = x.pred))),
            color = "red")
