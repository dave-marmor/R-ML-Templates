# Linear Regression Function

SimpleLinearRegression <- function(X, y) {
  if (!is.vector(X)) x <- X[[1]]
  n <- length(y)
  y_i <- sum(y)
  x_i <- sum(x)
  x_i_y_i <- sum(x*y)
  x_i_2 <- sum(x^2)
  
  b1 <- (x_i_y_i - x_i*y_i/n) / 
        (x_i_2 - x_i*x_i/n)
  b0 <- y_i/n - b1*x_i/n
  
  x_name <- ifelse(is.vector(X), "x", names(X))
  coefficients <- c(b0, b1)
  names(coefficients) <- c("Intercept", x_name)
  return(coefficients)
}

# Test it out
data <- read.csv("./Data Files/50_Startups.csv")
SimpleLinearRegression(X = data[1], y = data$Profit)
lm(y ~ x)


