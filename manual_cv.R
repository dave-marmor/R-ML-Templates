### CV Function ###
cv_folds <- function(X, splits = 10) {
  rand_order <- sample(1:nrow(X))
  j = 1
  folds <- lapply(1:splits, function(x) x <- NULL)
  for (i in 1:length(rand_order)) {
    index <- rand_order[i]
    if(j > splits) {
      j <- 1
      tmp <- folds[[j]]
      tmp <- c(tmp, index)
      folds[[j]] <- tmp
      j <- j + 1
    } else {
      tmp <- folds[[j]]
      tmp <- c(tmp, index)
      folds[[j]] <- tmp
      j <- j + 1
    }
  }
  cv_folds <- lapply(1:splits, function(x) x <- NULL)
  for (k in 1:splits) {
    cv_folds[[k]] <- X[folds[[k]], ]
  }
  return(cv_folds)
}
split_data <- cv_folds(data)

results <- NULL
for (i in 1:10) {
  train_folds <- split_data[-i]
  train <- do.call("rbind", train_folds)
  test <- split_data[[i]]
  fit <- lm(data = train, Spending.Score ~ Annual.Income)
  y_pred <- predict(fit, test)
  mse <- sum((y_pred - test$Spending.Score)^2) / nrow(data)
  rmse <- sqrt(mse)
  new_entry <- data.frame(Fold = i, MSE = mse, RMSE = rmse)
  results <- rbind(results, new_entry)
}
results





models <- lapply(cv_folds(data), 
                 function(x) lm(Spending.Score ~ Annual.Income, data = x))
models <- lapply(models, function(x) predict(x))




data <- read.csv("./Data Files/Mall_Customers.csv")
a <- cv_folds(data, 3)
a
df <- data.frame(x, y)
x = 1:5
y = 11:15
b <- lm(y ~ x, data = df)
b
names(b)
b$call
