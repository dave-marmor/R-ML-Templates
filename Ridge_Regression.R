
### INCOMPLETE ###


# Import the dataset
data <- read.csv("./Data Files/50_Startups.csv")
data <- data
head(data, 4)

# Partition the data into train and test sets
set.seed(1111)
split <- sample(1:nrow(data), .80 * nrow(data))
train.set <- data[split, ]
test.set <- data[-split, ]
y.test <- test.set$Profit  # Input y-variable

# Fit Base Ridge Regression using ALL variables
library(MASS)
ridge.fit <- linearRidge(Profit ~ ., 
                      data = train.set,
                      lambda = 1)

# Predict on the test data
y.pred <- as.matrix(cbind(const = 1, test.set[-4])) %*% coef(ridge.fit)
pred.summary <- test.set
pred.summary$y.pred <- y.pred
head(pred.summary)

# Evaluate the model on the test data
mse <- mean((y.test - y.pred)^2)
rmse <- sqrt(mse)
var_explained = 1 - (sum((y.test - y.pred)^2) / 
                       (sum((y.test - mean(y.test))^2)))
data.frame(Metric = c("MSE", "RMSE", "Var_Explained"),
           Measure = c(mse, rmse, var_explained))
