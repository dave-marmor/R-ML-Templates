# Import the dataset
data <- read.csv("./Data Files/Simple_Table.csv")

# Partition the data into train and test sets
set.seed(1111)
split <- sample(1:nrow(data), .8 * nrow(data))  # Method 1
train.set <- data[split, ]
test.set <- data[-split, ]

# Scale the train and test sets
library(caret)
scale.values <- preProcess(train.set, method = c("center", "scale"))
train.set.scaled <- predict(scale.values, train.set)
test.set.scaled <- predict(scale.values, test.set)

# Alternative method to scale data
ScaleData <- function(train, test) {
  num.cols <- which(lapply(train, class) %in% c("integer", "numeric"))
  col <- train[num.cols]
  means <- apply(col, 2, function(x) mean(x, na.rm = TRUE))
  sds <- apply(col, 2, function(x) sd(x, na.rm = TRUE))
  train[, num.cols] <- t((t(col)-means) / sds)
  test[, num.cols] <- t((t(test[, num.cols])-means) / sds)
  list(train = train, test = test)
}
scaled.data <- ScaleData(train.set, test.set)
train.scaled <- scaled.data$train
test.scaled <- scaled.data$test

# Identify Missing Data
apply(data, 2, function(x) sum(is.na(x)))  # Identify Columns
data[, which(apply(data, 2, function(x) sum(is.na(x))) > 0)]  # Subset Columns
which(apply(data, 1, function(x) sum(is.na(x))) > 0)  # Identify Rows
data[which(apply(data, 1, function(x) sum(is.na(x))) > 0), ]  # Subset Rows
which(is.na(data$Age))  # For a specific Column only

# Remove ALL missing data
data <- na.omit(data)
row.names(data) <- NULL  # Reset Index (Optional)

# Impute Missing Numeric Data Using Median or Mean
data$Age[is.na(data$Age)] <- median(data$Age, na.rm = TRUE)


# Impute Missing Categorical Data Using Most Frequent Category
data$Country[7] <- NaN  # Generate missing value
data$Country[is.na(data$Country)] <- names(which.max(table(data$Country)))

# Re-Code Baseline Category
data$Purchased <- factor(data$Purchased, levels = c("Yes", "No"))
contrasts(data$Purchased)

# Convert Factors to Numeric (0/1 binary)
data$Purchased <- as.numeric(data$Purchased)-1
