# Install and load the caTools package
install.packages("caTools")
library(caTools)

# Load the data
data <- read.csv("Admission_Predict.csv")

# Split the data: 70% training, 30% testing
split <- sample.split(data$Chance.of.Admit, SplitRatio = 0.7)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Fit the model on the training data
linear_model <- lm(Chance.of.Admit ~ GRE.Score + CGPA + University.Rating, data = train_data)

# Predict on the testing data
predictions <- predict(linear_model, newdata = test_data)

# Evaluate the model
actual <- test_data$Chance.of.Admit
MSE <- mean((predictions - actual)^2)
R2 <- 1 - (sum((predictions - actual)^2) / sum((actual - mean(actual))^2))

# Output the Mean Squared Error and R-squared
print(paste("Mean Squared Error: ", MSE))
print(paste("R-squared on Test Data: ", R2))
