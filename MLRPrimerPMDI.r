# Load required packages
library(tidyverse)
library(car)
library(ggplot2)
library(corrplot)

# Read the CSV file
data <- read.csv("C:/Users/zishu/Documents/MyGitHub/PMDIcombi.csv")

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
print("The correlation matrix")
print(cor_matrix)

# Create correlation plot
png("correlation_plot.png", width = 800, height = 800, res = 100)
corrplot(cor_matrix,
  method = "color",
  type = "full",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  title = "Correlation Matrix of Variables"
)

################################################
# Function to perform regression analysis
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
  png(paste0("diagnostic_", dependent_var, ".png"),
    width = 1200, height = 1200, res = 150
  )
  par(mfrow = c(2, 2))
  plot(model)
  dev.off()

  return(model)
}

# Perform regression analysis for each water resistance measure
models <- list(
  Quell2h = perform_regression("Quell2h", data),
  Quell24h = perform_regression("Quell24h", data),
  WA2h = perform_regression("WA2h", data),
  WA24h = perform_regression("WA24h", data)
)

# Create prediction plots function
create_prediction_plot <- function(model, dependent_var, data) {
  # Get predictions
  predictions <- predict(model)

  # Create data frame for plotting
  plot_data <- data.frame(
    Actual = data[[dependent_var]],
    Predicted = predictions,
    LeimgradCode = data$LeimgradCode,
    Primer = data$Primer
  )

  # Calculate R-squared
  r_squared <- summary(model)$r.squared

  # Calculate axis limits
  max_val <- max(c(plot_data$Actual, plot_data$Predicted), na.rm = TRUE)
  min_val <- min(c(plot_data$Actual, plot_data$Predicted), na.rm = TRUE)
  range_val <- max_val - min_val
  padding <- range_val * 0.05

  # Create plot
  ggplot(plot_data, aes(
    x = Actual,
    y = Predicted,
    color = LeimgradCode,
    shape = Primer
  )) +
    # Add points
    geom_point(size = 2) +
    # Add reference line
    geom_abline(
      intercept = 0, slope = 1,
      color = "red", linetype = "dashed", size = 0.3
    ) +
    # Add trend lines
    geom_smooth(
      aes(
        group = interaction(LeimgradCode, Primer),
        linetype = Primer
      ),
      method = "lm", se = TRUE, alpha = 0.1, size = 0.5
    ) +
    # Customize appearance
    scale_color_manual(values = c("high" = "#FF4B4B", "low" = "#4682B4")) +
    scale_shape_manual(values = c("melamine primer" = 16, "no primer" = 17)) +
    # Add labels
    labs(
      title = paste("Actual vs Predicted Values -", dependent_var),
      subtitle = paste("R² =", round(r_squared, 3)),
      x = "Actual Values",
      y = "Predicted Values"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 11),
      plot.subtitle = element_text(size = 9),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      legend.position = "right",
      legend.key.size = unit(0.8, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray92"),
      plot.margin = margin(t = 10, r = 20, b = 10, l = 10)
    ) +
    coord_cartesian(
      xlim = c(min_val - padding, max_val + padding),
      ylim = c(min_val - padding, max_val + padding),
      expand = FALSE
    )
}

# Create interaction plots function
create_interaction_plot <- function(dependent_var, data) {
  # Calculate accurate data ranges
  density_range <- range(data$Dens_mitt, na.rm = TRUE)
  density_padding <- diff(density_range) * 0.05
  dependent_range <- range(data[[dependent_var]], na.rm = TRUE)
  dependent_padding <- diff(dependent_range) * 0.1

  ggplot(data, aes(x = Dens_mitt, y = .data[[dependent_var]])) +
    facet_wrap(~Primer) +
    geom_point(aes(color = LeimgradCode), size = 2.5) +
    geom_smooth(aes(color = LeimgradCode, linetype = Primer),
      method = "lm",
      se = TRUE,
      alpha = 0.15,
      linewidth = 0.8
    ) +
    scale_color_manual(values = c("high" = "red", "low" = "blue")) +
    scale_linetype_manual(values = c(
      "melamine primer" = "solid",
      "no primer" = "dashed"
    )) +
    labs(
      title = paste("Interaction Effects -", dependent_var),
      x = "Mean Density",
      y = dependent_var
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 11),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      legend.position = "right",
      legend.key.size = unit(0.8, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray92"),
      strip.text = element_text(size = 10),
      plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
      panel.spacing = unit(1.5, "lines")
    ) +
    coord_cartesian(
      xlim = c(
        density_range[1] - density_padding,
        density_range[2] + density_padding
      ),
      ylim = c(
        max(0, dependent_range[1] - dependent_padding),
        dependent_range[2] + dependent_padding
      ),
      expand = FALSE
    )
}

# Create output directory
output_dir <- "analysis_results_new"
if (dir.exists(output_dir)) {
  unlink(output_dir, recursive = TRUE)
}
dir.create(output_dir, showWarnings = FALSE)

# Save detailed results
sink(file.path(output_dir, "regression_results.txt"))
cat("REGRESSION ANALYSIS SUMMARY\n")
cat("==========================\n\n")

for (var in names(models)) {
  model <- models[[var]]

  cat("\nResults for", var, "\n")
  cat("----------------\n")
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

# Save plots with optimized settings
for (var in names(models)) {
  # Prediction plot
  pred_plot <- create_prediction_plot(models[[var]], var, data)
  ggsave(
    file.path(output_dir, paste0("prediction_plot_", var, ".png")),
    pred_plot,
    width = 8,
    height = 6,
    dpi = 300,
    bg = "white"
  )

  # Interaction plot
  interaction_plot <- create_interaction_plot(var, data)
  ggsave(
    file.path(output_dir, paste0("interaction_", var, ".png")),
    interaction_plot,
    width = 10,
    height = 10,
    dpi = 300,
    bg = "white"
  )
}

# Create comparison summary
results_summary <- data.frame(
  Measure = names(models),
  R_squared = sapply(models, function(x) summary(x)$r.squared),
  Adj_R_squared = sapply(models, function(x) summary(x)$adj.r.squared),
  F_statistic = sapply(models, function(x) summary(x)$fstatistic[1])
)

print("Model Comparison Summary:")
print(results_summary)

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

  # Additional metrics
  predictions <- predict(model)
  actual <- data[[var]]
  rmse <- sqrt(mean((actual - predictions)^2))
  mae <- mean(abs(actual - predictions))
  cat("RMSE:", rmse, "\n")
  cat("MAE:", mae, "\n\n")
}

cat("\nAnalysis completed. Results saved in:", output_dir, "\n")
