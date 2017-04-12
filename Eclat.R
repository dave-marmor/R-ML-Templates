# Import the dataset (for visualization purposes only)
data <- read.csv("./Data Files/Market_Basket_Optimization.csv",
                 header = FALSE)

# Create Sparse Matrix from dataset and summarize data
library(arules)
data <- read.transactions("./Data Files/Market_Basket_Optimization.csv",
                          sep = ",",
                          rm.duplicates = TRUE)
summary(data)

# Visualize the most frequency
itemFrequencyPlot(data, topN = 10)

# Train the Eclat model
rules <- eclat(data = data,
               parameter = list(support = 0.004, minlen = 2))  # Must specify
summary(rules)

# Visualize the resulting rules
inspect(rules)
inspect(sort(rules, by = "support")[1:10])  # List only the top 10 by support
