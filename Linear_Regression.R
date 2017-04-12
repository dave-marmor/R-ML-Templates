# Import the dataset
data <- read.csv("./Data Files/50_Startups.csv")
head(data, 4)

# Partition the data into train and test sets
set.seed(1111)
split <- sample(1:nrow(data), .80 * nrow(data))
train.set <- data[split, ]
test.set <- data[-split, ]
y.test <- test.set$Profit  # Input y-variable

# Fit Linear Regression using ALL variables
lm.fit <- lm(formula = Profit ~ ., 
             data = train.set)

# Analyze the model
summary(lm.fit)

# Predict on the test data
y.pred <- predict(lm.fit, newdata = test.set)
pred.summary <- test.set
pred.summary$y.pred <- y.pred
head(pred.summary, 4)

# Evaluate the model on the test data
mse <- mean((y.test - y.pred)^2)
rmse <- sqrt(mse)
var_explained = 1 - (sum((y.test - y.pred)^2) / 
                       (sum((y.test - mean(y.test))^2)))
data.frame(Score_Metric = c("MSE", "RMSE", "Var_Explained"),
           Measure = c(mse, rmse, var_explained))
