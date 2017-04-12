# Import the dataset
data <- read.csv("./Data Files/Social_Network_Ads.csv")

# Convert the dependent variable into a factor
data$Purchased <- as.factor(as.character(data$Purchased))

# Partition the data into train and test sets
set.seed(1111)
split <- sample(1:nrow(data), .75 * nrow(data))
train.set <- data[split, ]
test.set <- data[-split, ]

# Fit Logistic Regression using selected variables
log.model <- glm(formula = Purchased ~ Age + EstimatedSalary, 
                 data = train.set,
                 family = binomial)

# Analyze the model
summary(log.model)

# Predict on the test data
y.prob <- predict(log.model, 
                  newdata = test.set,
                  type = "response")
y.pred <- ifelse(y.prob >= .5, 1, 0)
pred.summary <- test.set
pred.summary$y.pred <- y.pred
pred.summary$y.prob <- round(y.prob, 2)
head(pred.summary)

# Create Confusion Matrix
conf.mat <- table(Actual = test.set$Purchased, Predicted = y.pred)
conf.mat

# Accuracy, Precision, Recall, & F1 Score
Accuracy <- sum(diag(conf.mat)) / sum(conf.mat)
Precision <- conf.mat[1] / (conf.mat[1] + conf.mat[3])
Recall <- conf.mat[1] / (conf.mat[1] + conf.mat[2])
F1_Score <- (2 * Precision * Recall) / (Precision + Recall)
round(data.frame(Accuracy, Precision, Recall, F1_Score), 2)




# Visualize the logistic regression model  (Still a work in progress)

age <- NULL
for (i in 0:60) {
  tmp <- rep(i, 141)
  age <- c(age, tmp)
}
es <- seq(0, 140000, 1000)
x.pred <- data.frame(Age = age, EstimatedSalary = es)
x.pred$prob <- predict(log.fit, x.pred, type = "response")
x.pred$pred <- ifelse(x.pred$prob >= .5, "1", "0")
plot(x.pred$Age, x.pred$EstimatedSalary, col = x.pred$pred)

library(ggplot2)
ggplot() + 
  geom_point(data = x.pred, aes(x = Age, y = EstimatedSalary, color = pred)) +
  geom_point(data = train.set, aes(x = Age, y = EstimatedSalary, color = Purchased), color = "black")



