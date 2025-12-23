# MLR analysis for |Wood density| and |resin coating|. The |types of Resination|, |resination level| affect the waterproofness of particleboard, Quell2h * Quell24h * WA2h * WA24h,
# Load required packages
library(tidyverse)
library(car)
library(ggplot2)
library(corrplot)
library(stats)
library(gridExtra)

# Read the CSV file
data <- read.csv("C:/Users/zishu/Documents/MyGitHub/AllcombiHolz.csv")
# view(data)

# Convert categorical variables to factors
data$LeimCode <- as.factor(data$LeimCode) # Resination Type
data$LeimgradCode <- as.factor(data$LeimgradCode) # Glue Application Density
data$Primer <- as.factor(data$Primer) # Primer Treatment

########################################################################
# Create correlation matrix (for waterproof indicators and density)
########################################################################
waterproof_vars <- data[, c(
  "Dens_mitt", "Dens_max", "Dens_min",
  "Quell2h", "Quell24h", "WA2h", "WA24h"
)]

cor_matrix <- cor(waterproof_vars, use = "complete.obs")
print("Correlation Matrix for Waterproof Properties:")
print(cor_matrix)

# Create correlation heatmap
corrplot(cor_matrix,
  method = "color",
  type = "full",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  title = "Correlation Matrix of Waterproof Properties"
)

###################################################################
# # Define multiple linear regression analysis function
###################################################################
analyze_waterproof <- function(dependent_var, data) {
  # Build regression model
  formula <- as.formula(paste(
    dependent_var,
    "~ Dens_mitt + LeimCode + LeimgradCode + Primer"
  ))

  model <- lm(formula, data = data)

  # Model summary
  summary_stats <- summary(model)

  # ANOVA analysis
  anova_result <- anova(model)

  # VIF analysis
  vif_result <- vif(model)

  # Residual analysis
  residuals <- resid(model)
  fitted_values <- fitted(model)

  # Create diagnostic plots
  par(mfrow = c(2, 2))
  plot(model)

  # Return results list
  return(list(
    model = model,
    summary = summary_stats,
    anova = anova_result,
    vif = vif_result,
    residuals = residuals,
    fitted = fitted_values
  ))
}

# Analyze each waterproof property
waterproof_measures <- c("Quell2h", "Quell24h", "WA2h", "WA24h")
analysis_results <- list()

# Create results directory
dir.create("waterproof_analysis", showWarnings = FALSE)

# Save analysis results
sink("waterproof_analysis/regression_results.txt")

cat("Multiple Linear Regression Analysis for Waterproof Properties\n")
cat("=========================================================\n\n")

for (measure in waterproof_measures) {
  cat(sprintf("\nAnalysis Results for %s:\n", measure))
  cat("----------------------------------\n")

  # Perform analysis
  results <- analyze_waterproof(measure, data)
  analysis_results[[measure]] <- results

  # Print main results
  print(results$summary)

  cat("\nANOVA Analysis:\n")
  print(results$anova)

  cat("\nVariance Inflation Factors (VIF):\n")
  print(results$vif)

  # Calculate and print additional statistics
  rmse <- sqrt(mean(results$residuals^2))
  mae <- mean(abs(results$residuals))

  cat(sprintf("\nRoot Mean Square Error (RMSE): %f\n", rmse))
  cat(sprintf("Mean Absolute Error (MAE): %f\n", mae))
}

sink()

# Create visualizations
for (measure in waterproof_measures) {
  results <- analysis_results[[measure]]

  # Actual vs Predicted plot
  pred_plot <- ggplot(data.frame(
    Actual = data[[measure]],
    Predicted = results$fitted,
    LeimCode = data$LeimCode
  ), aes(x = Actual, y = Predicted, color = LeimCode)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(
      title = sprintf("%s: Actual vs Predicted Values", measure),
      x = "Actual Values",
      y = "Predicted Values"
    ) +
    theme_minimal()
  # print(pred_plot)

  # Density effect plot
  density_plot <- ggplot(data, aes(
    x = Dens_mitt, y = .data[[measure]],
    color = LeimCode
  )) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    facet_grid(LeimgradCode ~ Primer) +
    labs(
      title = sprintf("Density Effect on %s", measure),
      x = "Mean Density",
      y = measure
    ) +
    theme_minimal()
  # print(density_plot)

  # Save plots
  ggsave(
    sprintf("waterproof_analysis/%s_prediction.png", measure),
    pred_plot
  )
  ggsave(
    sprintf("waterproof_analysis/%s_density_effect.png", measure),
    density_plot
  )
}

# Create comparison table
comparison_table <- data.frame(
  Measure = waterproof_measures,
  R_squared = sapply(analysis_results, function(x) summary(x$model)$r.squared),
  Adj_R_squared = sapply(analysis_results, function(x) summary(x$model)$adj.r.squared),
  F_statistic = sapply(analysis_results, function(x) summary(x$model)$fstatistic[1]),
  RMSE = sapply(analysis_results, function(x) sqrt(mean(x$residuals^2))),
  MAE = sapply(analysis_results, function(x) mean(abs(x$residuals)))
)

# Print comparison table
print("Model Comparison Summary:")
print(comparison_table)

# Save comparison table
write.csv(comparison_table, "waterproof_analysis/model_comparison.csv",
  row.names = FALSE
)

# Create interaction effect analysis
for (measure in waterproof_measures) {
  # Glue type and density interaction
  leim_density_plot <- ggplot(data, aes(
    x = Dens_mitt, y = .data[[measure]],
    color = LeimCode
  )) +
    geom_point() +
    geom_smooth(method = "lm") +
    labs(
      title = sprintf("%s: Glue Type and Density Interaction", measure),
      x = "Mean Density",
      y = measure
    ) +
    theme_minimal()

  # Glue density and primer interaction
  grade_primer_plot <- ggplot(data, aes(
    x = .data[[measure]],
    fill = LeimgradCode
  )) +
    geom_boxplot() +
    facet_wrap(~Primer) +
    labs(
      title = sprintf("%s: Glue Density and Primer Interaction", measure),
      y = measure
    ) +
    theme_minimal()

  # Save interaction plots
  ggsave(
    sprintf("waterproof_analysis/%s_leim_density_interaction.png", measure),
    leim_density_plot
  )
  ggsave(
    sprintf("waterproof_analysis/%s_grade_primer_interaction.png", measure),
    grade_primer_plot
  )
}

# Analyze factor importance
factor_importance <- data.frame()

for (measure in waterproof_measures) {
  model <- analysis_results[[measure]]$model
  coefficients <- abs(coef(model))[-1] # Remove intercept

  # Create factor importance dataframe
  importance <- data.frame(
    Measure = measure,
    Factor = names(coefficients),
    Importance = coefficients
  )

  factor_importance <- rbind(factor_importance, importance)
}

# Print factor importance
print("Factor Importance for Waterproof Properties:")
print(factor_importance)

# Save factor importance
write.csv(factor_importance, "waterproof_analysis/factor_importance.csv",
  row.names = FALSE
)

# Print final summary
cat("\nAnalysis Summary:\n")
cat("=================\n")
for (measure in waterproof_measures) {
  results <- analysis_results[[measure]]

  cat(sprintf("\n%s Results:\n", measure))
  cat("------------------------\n")

  # Model performance metrics
  r_squared <- summary(results$model)$r.squared
  adj_r_squared <- summary(results$model)$adj.r.squared
  f_stat <- summary(results$model)$fstatistic
  p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)

  cat(sprintf("R-squared: %.4f\n", r_squared))
  cat(sprintf("Adjusted R-squared: %.4f\n", adj_r_squared))
  cat(sprintf("F-statistic: %.4f (p-value: %.4e)\n", f_stat[1], p_value))

  # Effect significance
  coef_table <- summary(results$model)$coefficients
  cat("\nSignificant Factors (p < 0.05):\n")
  significant_factors <- rownames(coef_table)[coef_table[, 4] < 0.05]
  print(significant_factors)
}
