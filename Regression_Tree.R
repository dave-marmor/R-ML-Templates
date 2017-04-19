# Import the dataset & load the rpart package
library(dplyr)
data <- read.csv("./Data Files/USA_Housing.csv")
data <- select(data, -Address)

# Partition the data into train and test sets
set.seed(1111)
split <- sample(1:nrow(data), .80 * nrow(data))
train.set <- data[split, ]
test.set <- data[-split, ]
y.test <- test.set$Price

# Create regression tree model (Use "eps-regression")
library(rpart)
tree.fit <- rpart(formula = Price ~ ., 
                 data = train.set,
                 control = rpart.control(minsplit = 1))
summary(tree.fit)

# Predict on the test data
y.pred <- predict(tree.fit, newdata = test.set)
pred.summary <- test.set
pred.summary$y.pred <- y.pred
head(pred.summary)

# Evaluate the model on the test data
mse <- mean((y.test - y.pred)^2)
rmse <- sqrt(mse)
var_explained = 1 - (sum((y.test - y.pred)^2) / 
                       (sum((y.test - mean(y.test))^2)))
data.frame(Score_Metric = c("MSE", "RMSE", "Var_Explained"),
           Measure = c(mse, rmse, var_explained))


# Visual representation (only works for 1 predictor)
x.pred <- seq(min(data$Level), max(data$Level), .01)
library(ggplot2)
ggplot() + 
  geom_point(data = data, aes(x = Level, y = Salary), color = "blue", shape = 1) +
  geom_line(aes(x = x.pred, 
                y = predict(rf.fit, data.frame(Level = x.pred))),
            color = "red")