#  MLR analysis for |Wood density| and |resin coating||resination level UF| affect the waterproofness of particleboard, Quell2h * Quell24h * WA2h * WA24h,
# Load required packages
library(tidyverse)
library(car)
library(ggplot2)
library(corrplot)

# Read the CSV file
data <- read.csv("C:/Users/zishu/Documents/MyGitHub/UFcombi.csv")
# view(data)

# Convert categorical variables to factors
data$LeimgradCode <- as.factor(data$LeimgradCode)
data$Primer <- as.factor(data$Primer)
####################################################
# Create correlation matrix for numerical variables
####################################################
numeric_vars <- data[, c(
  "Dens_mitt", "Dens_max", "Dens_min",
  "Quell2h", "Quell24h", "WA2h", "WA24h"
)]
cor_matrix <- cor(numeric_vars, use = "complete.obs")
print("The coorelation matrix")
print(cor_matrix)

# Create correlation plot
corrplot(cor_matrix,
  method = "color",
  type = "full",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  title = "Correlation Matrix of Variables"
)

################################################
# # Function to perform regression analysis
################################################
perform_regression <- function(dependent_var, data) {
  # Create formula
  formula <- as.formula(paste(dependent_var, "~ Dens_mitt + LeimgradCode + Primer"))
  # Fit model
  model <- lm(formula, data = data)

  # Print summary
  cat("\nRegression Analysis for", dependent_var, ":\n")
  cat("===================================\n")
  print(summary(model))

  # Print ANOVA
  cat("\nANOVA Results:\n")
  print(anova(model))

  # VIF analysis
  cat("\nVariance Inflation Factors:\n")
  print(vif(model))

  # Create diagnostic plots
  par(mfrow = c(2, 2))
  plot(model)

  # Return model
  return(model)
}

# Perform regression analysis for each water resistance measure
models <- list(
  Quell2h = perform_regression("Quell2h", data),
  Quell24h = perform_regression("Quell24h", data),
  WA2h = perform_regression("WA2h", data),
  WA24h = perform_regression("WA24h", data)
)

# Create prediction plots
create_prediction_plot <- function(model, dependent_var, data) {
  predictions <- predict(model)
  plot_data <- data.frame(
    Actual = data[[dependent_var]],
    Predicted = predictions,
    LeimgradCode = data$LeimgradCode,
    Primer = data$Primer
  )

  ggplot(plot_data, aes(
    x = Actual, y = Predicted,
    color = LeimgradCode, shape = Primer
  )) +
    geom_point(alpha = 0.6) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(
      title = paste("Actual vs Predicted Values -", dependent_var),
      x = "Actual Values",
      y = "Predicted Values"
    ) +
    theme_minimal()
}

# Create boxplots for categorical variables
create_boxplot <- function(dependent_var, data) {
  ggplot(data, aes(x = LeimgradCode, y = .data[[dependent_var]], fill = Primer)) +
    geom_boxplot() +
    labs(
      title = paste("Distribution of", dependent_var),
      y = dependent_var
    ) +
    theme_minimal()
}

# Create results directory
dir.create("analysis_results", showWarnings = FALSE)

# Save detailed results
sink("analysis_results/regression_results.txt")

# Print overall summary
cat("REGRESSION ANALYSIS SUMMARY\n")
cat("==========================\n\n")

for (var in names(models)) {
  model <- models[[var]]

  cat("\nResults for", var, "\n")
  cat("----------------\n")

  # Model summary
  print(summary(model))

  # Calculate metrics
  predictions <- predict(model)
  actual <- data[[var]]

  rmse <- sqrt(mean((actual - predictions)^2))
  mae <- mean(abs(actual - predictions))

  cat("\nPerformance Metrics:\n")
  cat("RMSE:", rmse, "\n")
  cat("MAE:", mae, "\n")

  # Effect sizes
  cat("\nStandardized Coefficients:\n")
  coef_summary <- summary(model)$coefficients
  std_coef <- coef_summary[, 1] * (sd(model.matrix(model)) / sd(actual))
  print(std_coef)
}

sink()

# Save plots
for (var in names(models)) {
  # Prediction plot
  pred_plot <- create_prediction_plot(models[[var]], var, data)
  ggsave(paste0("analysis_results/prediction_plot_", var, ".png"), pred_plot)

  # Boxplot
  box_plot <- create_boxplot(var, data)
  ggsave(paste0("analysis_results/boxplot_", var, ".png"), box_plot)
}

# Create comparison summary
results_summary <- data.frame(
  Measure = names(models),
  R_squared = sapply(models, function(x) summary(x)$r.squared),
  Adj_R_squared = sapply(models, function(x) summary(x)$adj.r.squared),
  F_statistic = sapply(models, function(x) summary(x)$fstatistic[1])
)

# Print comparison summary
print("Model Comparison Summary:")
print(results_summary)

# Additional visualization for density effect
ggplot(data, aes(x = Dens_mitt, y = WA24h, color = LeimgradCode, shape = Primer)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Density vs 24h Water Absorption",
    x = "Mean Density",
    y = "24h Water Absorption"
  ) +
  theme_minimal()

# Interaction plots
for (var in names(models)) {
  interaction_plot <- ggplot(data, aes(
    x = Dens_mitt, y = .data[[var]],
    color = LeimgradCode, linetype = Primer
  )) +
    geom_point() +
    geom_smooth(method = "lm") +
    labs(
      title = paste("Interaction Plot for", var),
      x = "Mean Density",
      y = var
    ) +
    theme_minimal()

  ggsave(paste0("analysis_results/interaction_", var, ".png"), interaction_plot)
}

cat("\nFinal Model Evaluation Metrics:\n")
for (var in names(models)) {
  cat("\nEvaluation Results for", var, ":\n")
  cat("-------------------\n")
  model <- models[[var]]

  # Print coefficient summary
  cat("Coefficient Summary:\n")
  print(summary(model)$coefficients)

  # Print model fit statistics
  cat("\nModel Fit Statistics:\n")
  cat("R-squared:", summary(model)$r.squared, "\n")
  cat("Adjusted R-squared:", summary(model)$adj.r.squared, "\n")
  cat("F-statistic:", summary(model)$fstatistic[1], "\n")

  # Add p-value for F-statistic
  f_stat <- summary(model)$fstatistic
  p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
  cat("p-value:", p_value, "\n")

  # Add additional metrics
  predictions <- predict(model)
  actual <- data[[var]]

  # Calculate RMSE
  rmse <- sqrt(mean((actual - predictions)^2))
  cat("RMSE:", rmse, "\n")

  # Calculate MAE
  mae <- mean(abs(actual - predictions))
  cat("MAE:", mae, "\n\n")
}
