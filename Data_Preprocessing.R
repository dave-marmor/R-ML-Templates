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
### Alternative method
ScaleData <- function(train, test) {
  num_col <- which(lapply(train, class) %in% c("integer", "numeric"))
  col <- train[num_col]
  means <- apply(col, 2, function(x) mean(x, na.rm = TRUE))
  sds <- apply(col, 2, function(x) sd(x, na.rm = TRUE))
  for (i in 1:ncol(col)) {
    train[num_col[i]] <- (train[num_col[i]] - means[i]) / sds[i]
    test[num_col[i]] <- (test[num_col[i]] - means[i]) / sds[i]
  }
  list(scaled_train = train, scaled_test = test)
}
scaled.data <- ScaleData(train.set, test.set)
train.set.scaled1 <- scaled.data$scaled_train
test.set.scaled1 <- scaled.data$scaled_test

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

