# Linear Regression using Gradient Descent


b0 <- 0
b1 <- 0
step_size <- 10
tolerance <- 0.01

magnitude <- 1
end <- data.frame(b0 = b0,
                  b1 = b1,
                  Magnitude = magnitude)
while (magnitude > tolerance) {
  y_pred <- b0 + b1*x
  errors <- y_pred - y

  b0_deriv <- sum(errors)
  b0_adj <- step_size * b0_deriv
  b0 <- b0 - b0_adj

  b1_deriv <- sum(errors*x)
  b1_adj <- step_size*b1_deriv
  b1 <- b1 - b1_adj
  
  magnitude <- sqrt(b0_deriv^2 + b1_deriv^2)
  tmp <- data.frame(b0 = b0, b1 = b1, Magnitude = magnitude)
  end <- rbind(end, tmp)
  
}
end
b0
b1


data <- read.csv("./Data Files/50_Startups.csv")
x <- data$R.D.Spend
y <- data$Profit
SimpleLinearRegression(x, y)