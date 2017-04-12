# Import the dataset
original.data <- read.delim("./Data Files/Restaurant_Reviews.tsv",  # must be .tsv file
                            header = TRUE,
                            sep = "\t",
                            quote = "",
                            stringsAsFactors = FALSE)  

# Clean the text data
library(tm)
corpus <- VCorpus(VectorSource(original.data$Review))
as.character(corpus[[1]])  # To view the corpus (specify the row)
corpus <- tm_map(corpus, content_transformer(tolower))  # Make all lowercase
corpus <- tm_map(corpus, removeNumbers)  # Remove numbers
corpus <- tm_map(corpus, removePunctuation)  # Remove punctuation
library(SnowballC)
corpus <- tm_map(corpus, removeWords, stopwords())  # Remove non-relevant words
corpus <- tm_map(corpus, stemDocument)  # Stem document (get just root words)
corpus <- tm_map(corpus, stripWhitespace)  # Strip the white space
as.character(corpus[[1]])  # View corpus
original.data[1, ]  # Compare to original

# Create Document Term Matrix
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.999)  # Keep 99% of columns
data <- as.data.frame(as.matrix(dtm))
data$Liked <- original.data$Liked



#### Now create the classification model of your choice ####

# Convert the dependent variable into a factor
data$Liked <- as.factor(as.character(data$Liked))

# Partition the data into train and test sets
set.seed(1111)
split <- sample(1:nrow(data), .80 * nrow(data))
train.set <- data[split, ]
test.set <- data[-split, ]



### Using a Random Forest model:
library(randomForest)
set.seed(2222)
rf.tree <- randomForest(x = train.set[-692],
                        y = train.set$Liked,
                        ntree = 100) # Tune this number

# Predict on the test data
y.pred <- predict(rf.tree, newdata = test.set)
pred.summary <- test.set
pred.summary$y.pred <- y.pred
head(pred.summary)

# Create Confusion Matrix
conf.mat <- table(Actual = test.set$Liked, Predicted = y.pred)
conf.mat

# Accuracy, Precision, & Recall Rates
# Accuracy, Precision, Recall, & F1 Score
Accuracy <- sum(diag(conf.mat)) / nrow(test.set)
Precision <- conf.mat[1] / (conf.mat[1] + conf.mat[3])
Recall <- conf.mat[1] / (conf.mat[1] + conf.mat[2])
F1_Score <- (2 * Precision * Recall) / (Precision + Recall)
rf.stats <- round(data.frame(Accuracy, Precision, Recall, F1_Score), 2)

# Plotting the Random Forest to trim
plot(rf.tree)


### Using a Naive Bayes model:

# Create the Naive Bayes model (do not need to scale data)
library(e1071)
bayes.model <- naiveBayes(formula = Liked ~ .,
                          data = train.set)

# Predict on the test data
y.pred <- predict(bayes.model, newdata = test.set)

# Create Confusion Matrix
conf.mat <- table(Actual = test.set$Liked, Predicted = y.pred)
conf.mat

# Accuracy, Precision, Recall, & F1 Score
Accuracy <- sum(diag(conf.mat)) / nrow(test.set)
Precision <- conf.mat[1] / (conf.mat[1] + conf.mat[3])
Recall <- conf.mat[1] / (conf.mat[1] + conf.mat[2])
F1_Score <- (2 * Precision * Recall) / (Precision + Recall)
nb.stats <- round(data.frame(Accuracy, Precision, Recall, F1_Score), 2)


### Using a Naive Bayes model:

# Create the Decision Tree model (do not need to scale data)
library(rpart)
tree <- rpart(formula = Liked ~ .,
              data = train.set)

# Predict on the test data
y.prob <- predict(tree, newdata = test.set)  # This gives the probabilities
y.pred <- predict(tree, newdata = test.set, type = "class")
pred.summary <- test.set
pred.summary$y.prob <- round(y.prob[, 2], 2)
pred.summary$y.pred <- y.pred
head(pred.summary)

# Create Confusion Matrix
conf.mat <- table(Actual = test.set$Liked, Predicted = y.pred)
conf.mat

# Accuracy, Precision, Recall, & F1 Score
Accuracy <- sum(diag(conf.mat)) / nrow(test.set)
Precision <- conf.mat[1] / (conf.mat[1] + conf.mat[3])
Recall <- conf.mat[1] / (conf.mat[1] + conf.mat[2])
F1_Score <- (2 * Precision * Recall) / (Precision + Recall)
dt.stats <- round(data.frame(Accuracy, Precision, Recall, F1_Score), 2)


#### Compare the models ####
rbind(rf.stats, nb.stats, dt.stats)
