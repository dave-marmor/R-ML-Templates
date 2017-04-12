# Import the dataset (for visualization purposes only)
data <- read.csv("./Data Files/Market_Basket_Optimization.csv",
                 header = FALSE)

# Create Sparsity Matrix from dataset and summarize data
library(arules)
data <- read.transactions("./Data Files/Market_Basket_Optimization.csv",
                          sep = ",",
                          rm.duplicates = TRUE)
summary(data)

# Visualize the most frequency
itemFrequencyPlot(data, topN = 10)

# Train the Apriori model
rules <- apriori(data = data,
                 parameter = list(support = 0.003, confidence = 0.2))  # Must specify
summary(rules)

# Visualize the resulting rules
inspect(rules)
inspect(sort(rules, by = "lift")[1:10])  # List only the top 10 by lift
