# Install necessary packages (if not installed already)
install.packages(c("tidyverse", "caret", "e1071", "pROC", "mlbench"))

# Load libraries
library(tidyverse)
library(caret)
library(e1071)
library(pROC)
library(mlbench)

# Load the Sonar dataset
data(Sonar)

# Convert the Class variable into a factor
Sonar$Class <- as.factor(Sonar$Class)

# Split the dataset into training and testing sets
set.seed(123)
splitIndex <- createDataPartition(Sonar$Class, p = 0.8, list = FALSE)
train_data <- Sonar[splitIndex, ]
test_data <- Sonar[-splitIndex, ]

# Build a classification model using Random Forest
model_rf <- train(
  Class ~ ., 
  data = train_data, 
  method = "rf"
)

# Make predictions on the test set
predictions_rf <- predict(model_rf, newdata = test_data)

# Evaluate the Random Forest model
confusion_matrix_rf <- confusionMatrix(predictions_rf, test_data$Class)
print(confusion_matrix_rf)

# Performance plot for Random Forest
roc_curve_rf <- roc(test_data$Class, as.numeric(predictions_rf))
plot(roc_curve_rf, main = "ROC Curve (Random Forest)", col = "blue", lwd = 2)
