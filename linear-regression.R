library(caTools)
library(ggplot2)

# Load the data
data <- read.csv("Admission_Predict.csv")

str(data)

# Split the data: 70% training, 30% testing
split <- sample.split(data$Chance.of.Admit, SplitRatio = 0.7)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

#covariance and correlation matrix
cov_matrix <- cov(customer_churn)
cor_matrix <- cor(customer_churn) 
print(cov_matrix)
print(cor_matrix)
corrplot(cor_matrix, method = "color", addCoef.col = "black")

#Find deviations between datasets (e.g., using mean difference)
mean_diff <- colMeans(train_data[, -1]) - colMeans(test_data[, -1])
print("Mean differences between training and testing sets:")
print(mean_diff)

# Fit the model on the training data
linear_model <- lm(Chance.of.Admit ~ GRE.Score + CGPA + University.Rating, data = train_data)

# Predict on the testing data
test_data$Predicted.Admit <- predict(linear_model, newdata = test_data)

# Plot the line of best fit against the actual values in the test set
ggplot(test_data, aes(x = Predicted.Admit, y = Chance.of.Admit)) +
  geom_point(color = 'blue') +
  geom_smooth(method = lm, color = 'red') +
  labs(title = "Actual vs Predicted Chance of Admit (Test Set)",
       x = "Predicted Chance of Admit",
       y = "Actual Chance of Admit") +
  theme_minimal()


# Calculate and print performance metrics
MSE <- mean((test_data$Predicted.Admit - test_data$Chance.of.Admit)^2)
R2 <- 1 - (sum((test_data$Predicted.Admit - test_data$Chance.of.Admit)^2) / sum((test_data$Chance.of.Admit - mean(test_data$Chance.of.Admit))^2))

print(paste("Mean Squared Error on Test Data: ", MSE))
print(paste("R-squared on Test Data: ", R2))
