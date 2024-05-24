library(tidyverse)
library(haven)
library(dplyr)
library(pastecs)
library(ggplot2)

# Load data set
murders_95df <- read.csv("murders.csv") 

# Filter by year 1995
filtered_murders_1995 <- murders_95df %>%
  filter(year == 1995)

# Descriptive statistics of dataset
summary(murders_95df)

summary(murders_95df$filtered_murders)

# Calculate the correlation matrix
correlation_matrix <- cor(filtered_murders_1995[, c('density', 'popul', 'perc1019', 'perc2029', 'percblack', 'percmale', 'rpcincmaint', 'rpcpersinc', 'rpcunemins')])

# Find the two variables with the highest correlation coefficient
max_corr <- max(correlation_matrix)
indices <- which(correlation_matrix == max_corr, arr.ind = TRUE)
var1 <- rownames(correlation_matrix)[indices[1, 1]]
var2 <- colnames(correlation_matrix)[indices[1, 2]]

upper_tri <- upper.tri(correlation_matrix, diag = FALSE)

correlation_df <- subset(correlation_df, Var1 != Var2)
highest_correlations <- correlation_df[order(-abs(correlation_df$Freq)),]
highest_correlations <- highest_correlations[1:2,]
sorted_correlations <- correlation_df[order(correlation_df$Freq),]
head(correlation_df)

# Select relevant variables for model
model_data <- filtered_murders_1995[, c("murders", "perc1019", "perc2029", "percblack", "percmale", "rpcpersinc")]

# Fit a multiple linear regression model
model <- lm(murders ~ perc1019 + perc2029 + percblack + percmale + rpcpersinc, data = model_data)

# Summary of the model
summary(model)

# Diagnostic plots
par(mfrow=c(2,2))
plot(model)

# Create a histogram of residuals using ggplot
ggplot(aes(x = residuals(model)), data = model$model) +
  geom_histogram(fill = "skyblue", color = "black", bins = 20) +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") +
  theme_minimal()

# Evaluate the model
predictions <- predict(model)
mse <- mean((predictions = model_data$murders)^2)
r_squared <- summary(model)$r.squared

# Print evaluation metrics
cat('Mean Squared Error:', mse, '\n')
cat('R-squared:', r_squared, '\n')


