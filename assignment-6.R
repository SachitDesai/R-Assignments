# Load necessary library
library(ggplot2)

# Load the dataset
student_data <- read.csv("Admission_Predict.csv")

# Fit the linear model
linear_model <- lm(Chance.of.Admit ~ GRE.Score + CGPA + University.Rating, data = student_data)

# Predict the Chance of Admit using the linear model
student_data$Predicted.Admit <- predict(linear_model, newdata = student_data)

# Plot the actual vs predicted values
ggplot(student_data, aes(x = Predicted.Admit, y = Chance.of.Admit)) +
  geom_point(color = 'blue') +
  geom_smooth(method = lm, color = 'red') +
  labs(title = "Actual vs Predicted Chance of Admit",
       x = "Predicted Chance of Admit",
       y = "Actual Chance of Admit") +
  theme_minimal()

# Residuals plot
ggplot(student_data, aes(x = Predicted.Admit, y = residuals(linear_model))) +
  geom_point(color = 'darkgreen') +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted",
       x = "Predicted Chance of Admit",
       y = "Residuals") +
  theme_minimal()

predictions <- predict(linear_model, newdata = test_data)

# Evaluate the model
actual <- test_data$Chance.of.Admit
MSE <- mean((predictions - actual)^2)
R2 <- 1 - (sum((predictions - actual)^2) / sum((actual - mean(actual))^2))

# Output the Mean Squared Error and R-squared
print(paste("Mean Squared Error: ", MSE))
print(paste("R-squared on Test Data: ", R2))