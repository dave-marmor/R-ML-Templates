# Import the dataset
data <- read.csv("./Data Files/Regression_Sample_File_2.csv")
data <- data
head(data, 4)

# Partition the data into train and test sets
library(dplyr)
X <- data %>%
  select(bedrooms, bathrooms, sqft_living, sqft_lot, floors, waterfront, condition)
y <- data$price
set.seed(1111)
split <- sample(1:nrow(data), .80 * nrow(data))
X.train <- model.matrix(~ .-1, X[split, ])
X.test <- model.matrix(~ .-1, X[-split, ])
y.train <- y[split]
y.test <- y[-split]

# Fit Lasso Model
## Do not need to scale data, it is done by default
library(glmnet)
cv.lasso.fit <- cv.glmnet(x = X.train,
                          y = y.train)  # Family = "binmoial" for Logistic Regression

plot(cv.lasso.fit)
cv.lasso.fit$lambda.min
coef(cv.lasso.fit, s = "lambda.min")
cv.lasso.fit$lambda.1se
coef(cv.lasso.fit, s = "lambda.1se")

# Predict on the test data
y.pred <- predict(cv.lasso.fit,
                  newx = X.test,
                  s = "lambda.min")  # Or use "lambda.1se"
pred.summary <- data.frame(X.test)
pred.summary$y.actual <- y.test
pred.summary$y.pred <- as.vector(y.pred)
head(pred.summary)

# Evaluate the model on the test data
mse <- mean((y.test - y.pred)^2)
rmse <- sqrt(mse)
var_explained = 1 - (sum((y.test - y.pred)^2) / 
                       (sum((y.test - mean(y.test))^2)))
data.frame(Metric = c("MSE", "RMSE", "Var_Explained"),
           Measure = c(mse, rmse, var_explained))
