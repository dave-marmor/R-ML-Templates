# Import the dataset
data <- read.csv("./Data Files/50_Startups.csv")

# Manual Method
## Fit Linear Regression using ALL variables (manually)
lm.fit <- lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State, 
             data = data)
## Step 1
summary(lm.fit)  # State (dummy variable) is the highest p-value, so remove it
lm.fit <- lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend, 
             data = data)
## Step 2
summary(lm.fit)  # Administration is the highest p-value, so remove it
lm.fit <- lm(formula = Profit ~ R.D.Spend + Marketing.Spend , 
             data = data)
## Step 3
summary(lm.fit)  # Marketing.Spend is the highest p-value, so remove it
lm.fit <- lm(formula = Profit ~ R.D.Spend, 
             data = data)
## Step 4
summary(lm.fit)  # All variables are significant, so this is final model

# Automated Method using step() function using AIC (not p-values)
step(lm.fit, direction = "backward")
