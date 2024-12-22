library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)

# Load the data
data <- read_excel("E:/STAT515/cancer patient data sets.xlsx")

# View the first few rows of the dataset
head(data)
# Checking for missing values
sum(is.na(data))

# Checking for duplicate rows
sum(duplicated(data))

# You might also want to check data types and convert them if necessary
str(data)
# Age Distribution
ggplot(data, aes(x=Age)) + geom_histogram(bins=20) + ggtitle("Age Distribution")

# Gender Distribution
ggplot(data, aes(x=as.factor(Gender))) + geom_bar() + ggtitle("Gender Distribution")

# Cancer Level Distribution
ggplot(data, aes(x=Level)) + geom_bar() + ggtitle("Cancer Level Distribution")

# Encoding the 'Level' column for correlation
data$Level <- as.factor(data$Level)
levels(data$Level) <- c(1, 2, 3)
data$Level <- as.numeric(as.character(data$Level))

# Compute correlation
correlation_matrix <- cor(data[, sapply(data, is.numeric)])

# Visualization
corrplot(correlation_matrix, method="color")



data <- na.omit(data)  # Removes rows with any NA values
data$Level <- factor(data$Level)
str(data)
ggplot(data, aes(x=Level, y=Obesity)) + geom_boxplot() + ggtitle("Impact of Obesity on Cancer Level")


# Boxplots for different factors
ggplot(data, aes(x=Level, y=Obesity)) + geom_boxplot() + ggtitle("Impact of Obesity on Cancer Level")
# Repeat this for 'Coughing of Blood', 'Alcohol use', 'Dust Allergy', 'Balanced Diet'

# Age Distribution Across Cancer Levels
ggplot(data, aes(x=Level, y=Age)) + geom_boxplot() + ggtitle("Age Distribution Across Cancer Levels")

# Gender Distribution Across Cancer Levels
ggplot(data, aes(x=Level, fill=as.factor(Gender))) + geom_bar(position="dodge") + ggtitle("Gender Distribution Across Cancer Levels")



library(randomForest)
library(caret)

# Assuming 'data' is already loaded and cleaned
data$Level <- as.factor(data$Level)  # Ensure Level is a factor


# Check unique values in the Level column
unique(data$Level)
# Removing rows with NA in the Level column
data <- na.omit(data)
# Convert Level to factor if it's not already
data$Level <- as.factor(data$Level)

# Set seed for reproducibility
set.seed(123)

# Manually split the data
train_indices <- sample(1:nrow(data), size = 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Replace spaces with underscores in column names
colnames(train_data) <- gsub(" ", "_", colnames(train_data))
colnames(test_data) <- gsub(" ", "_", colnames(test_data))

# Fit the Random Forest model
# Assuming 'Level' is the dependent variable
rf_model <- randomForest(Level ~ ., data = train_data, ntree = 100)

# Print model summary
print(rf_model)

# Make predictions on the test dataset
rf_predictions <- predict(rf_model, newdata = test_data)


library(caret)

# Evaluate the model
confusionMatrix(rf_predictions, test_data$Level)


# Plot variable importance
varImpPlot(rf_model)

