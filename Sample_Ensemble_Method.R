library(dplyr)
library(caret)

data<-read.csv(url('https://datahack-prod.s3.ap-south-1.amazonaws.com/train_file/train_u6lujuX_CVtuZ9i.csv'))
X <- data %>%
  select(Credit_History, LoanAmount, Loan_Amount_Term, 
         ApplicantIncome, CoapplicantIncome, Loan_Status)

# Identify and remove or impute missing data
apply(data, 2, function(x) sum(x == ""))
apply(data, 2, function(x) sum(is.na(x)))

clean.data <- na.omit(data)
clean.data <- mutate(clean.data, Credit_History = ifelse(is.na(Credit_History), 0, Credit_History))


apply(clean.data, 2, function(x) sum(x == ""))
apply(clean.data, 2, function(x) sum(is.na(x)))
dim(clean.data)

X <- select(clean.data, -Loan_Status)
y <- clean.data$Loan_Status
set.seed(1111)
split <- sample(1:nrow(clean.data), .75 * nrow(clean.data))
X.train <- X[split, ]
y.train <- y[split]
X.test <- X[-split, ]
y.test <- y[-split]

library(caret)
fitControl <- trainControl(method = "cv",
                           number = 5,
                           savePredictions = "final",
                           classProbs = TRUE)

rf.tree <- randomForest(x = X.train,
                        y = y.train,
                        ntree = 100)
