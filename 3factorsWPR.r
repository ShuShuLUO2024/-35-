# Load required packages
library(tidyverse) # Data processing and visualization
library(car) # For Levene's test
library(rstatix) # Statistical analysis
library(ggpubr) # Advanced plotting
library(effectsize) # Effect size calculation

# Create results directory if it doesn't exist
dir.create("analysis_results", showWarnings = FALSE)

# Start capturing all output to a single file
sink("analysis_results/water_resistance_analysis_results.txt")

cat("=================================================================\n")
cat("Three-way ANOVA: Effects of Resin Type, Content, and Wood Type on Water Resistance Properties\n")
cat("=================================================================\n\n")

# 1. Data Preparation
cat("DATA PREPARATION\n")
cat("=================================================================\n")
data <- read_csv("C:/Users/zishu/Documents/MyGitHub/combi_data_filtered.csv")
cat("Data loaded successfully\n\n")

data_analysis <- data %>%
  filter(Primer == "no primer") %>%
  mutate(
    WoodCategory = case_when(
      HolzartCode %in% c("Oak", "Beech", "Poplar") ~ "Hardwood",
      TRUE ~ "Softwood"
    )
  ) %>%
  pivot_longer(
    cols = c(Quell2h, Quell24h, WA2h, WA24h),
    names_to = "Property",
    values_to = "Value"
  )

# 2. Assumption Testing
cat("ASSUMPTION TESTING\n")
cat("=================================================================\n")

# 2.1 Normality test
cat("\n1. Normality Test Results (Shapiro-Wilk):\n")
cat("-------------------------------------------\n")
normality_test <- data_analysis %>%
  group_by(Property, LeimCode, LeimgradCode, WoodCategory) %>%
  shapiro_test(Value) %>%
  mutate(
    Normality = if_else(p > 0.05, "Normal", "Non-normal")
  )
print(normality_test)

# Create and save Q-Q plots
qq_plots <- data_analysis %>%
  ggplot(aes(sample = Value)) +
  geom_qq() +
  geom_qq_line() +
  facet_grid(Property ~ interaction(LeimCode, LeimgradCode, WoodCategory),
    scales = "free"
  ) +
  theme_bw() +
  labs(
    title = "Normal Q-Q Plots for Water Resistance Properties",
    subtitle = "By Adhesive Type, Content, and Wood Category"
  )
ggsave("analysis_results/water_resistance_qq_plots.png", qq_plots, width = 15, height = 8)

# 2.2 Homogeneity of variance test
cat("\n2. Homogeneity of Variance Test Results (Levene's Test):\n")
cat("-------------------------------------------\n")
levene_results <- data_analysis %>%
  group_by(Property) %>%
  summarise(
    levene_test = list(
      leveneTest(Value ~ LeimCode * LeimgradCode * WoodCategory)
    )
  ) %>%
  mutate(
    Homogeneity = if_else(
      map_dbl(levene_test, ~ .$`Pr(>F)`[1]) > 0.05,
      "Homogeneous",
      "Non-homogeneous"
    )
  )
print(levene_results)

# 3. Three-way ANOVA
cat("\nTHREE-WAY ANOVA RESULTS\n")
cat("=================================================================\n")

# Function to check normality
check_normality <- function(property, data) {
  norm_test <- data %>%
    group_by(LeimCode, LeimgradCode, WoodCategory) %>%
    filter(Property == property) %>%
    group_split() %>%
    map_lgl(~ shapiro.test(.$Value)$p.value > 0.05)

  all(norm_test)
}

anova_results <- data_analysis %>%
  group_by(Property) %>%
  summarise(
    analysis_result = list({
      current_property <- first(Property)
      data_subset <- filter(data_analysis, Property == current_property)

      # Use Type III SS for all properties
      model <- Anova(lm(Value ~ LeimCode * LeimgradCode * WoodCategory,
        data = data_subset
      ), type = 3)

      list(
        model_type = "Type III",
        result = model
      )
    })
  )

# Print ANOVA results
for (i in 1:nrow(anova_results)) {
  cat("\nResults for", anova_results$Property[i], ":\n")
  cat("-------------------------------------------\n")
  if (anova_results$analysis_result[[i]]$model_type == "Type III") {
    print(anova_results$analysis_result[[i]]$result)
  } else {
    print(summary(anova_results$analysis_result[[i]]$result))
  }
}

# Effect Sizes
cat("\nEFFECT SIZES\n")
cat("=================================================================\n")
effect_sizes <- lapply(unique(data_analysis$Property), function(prop) {
  data_subset <- subset(data_analysis, Property == prop)
  model <- aov(Value ~ LeimCode * LeimgradCode * WoodCategory,
    data = data_subset
  )
  eta <- effectsize::eta_squared(model, partial = TRUE)
  data.frame(
    Property = prop,
    Parameter = eta$Parameter,
    Partial_Eta_squared = eta$Eta2_partial,
    CI_low = eta$CI_low,
    CI_high = eta$CI_high,
    stringsAsFactors = FALSE
  )
}) %>% bind_rows()
print(effect_sizes)

# 4. Post-hoc Analysis
cat("\nPOST-HOC ANALYSIS RESULTS\n")
cat("=================================================================\n")
posthoc_results <- data_analysis %>%
  group_by(Property) %>%
  summarise(
    posthoc = list({
      current_property <- first(Property)
      data_subset <- filter(data_analysis, Property == current_property)

      # Use Games-Howell for all properties due to non-homogeneous variance
      data_subset$group <- interaction(
        data_subset$LeimCode,
        data_subset$LeimgradCode,
        data_subset$WoodCategory,
        sep = "_"
      )

      games_howell_result <- games_howell_test(
        data = data_subset,
        Value ~ group,
        conf.level = 0.95
      )

      list(
        test_type = "games_howell",
        result = games_howell_result
      )
    })
  )

# Print post-hoc results
for (i in 1:nrow(posthoc_results)) {
  cat("\nPost-hoc Results for", posthoc_results$Property[i], ":\n")
  cat("-------------------------------------------\n")
  print(posthoc_results$posthoc[[i]]$result)
}

# 5. Descriptive Statistics
cat("\nDESCRIPTIVE STATISTICS\n")
cat("=================================================================\n")
descriptive_stats <- data_analysis %>%
  group_by(Property, LeimCode, LeimgradCode, WoodCategory) %>%
  summarise(
    n = n(),
    mean = mean(Value),
    sd = sd(Value),
    se = sd / sqrt(n),
    ci_lower = mean - qt(0.975, n - 1) * se,
    ci_upper = mean + qt(0.975, n - 1) * se,
    .groups = "drop"
  )
print(descriptive_stats)

# 6. Visualization
# Interaction plots
interaction_plots <- data_analysis %>%
  ggplot(aes(x = LeimgradCode, y = Value, color = WoodCategory)) +
  geom_point(position = position_dodge(0.2)) +
  stat_summary(fun = mean, geom = "line", aes(group = WoodCategory)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  facet_grid(Property ~ LeimCode,
    labeller = labeller(
      Property = c(
        "Quell2h" = "2h Thickness Swelling",
        "Quell24h" = "24h Thickness Swelling",
        "WA2h" = "2h Water Absorption",
        "WA24h" = "24h Water Absorption"
      )
    )
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  labs(
    title = "Interaction Effects of Adhesive Type, Content, and Wood Category",
    x = "Adhesive Content",
    y = "Value (%)",
    color = "Wood Category"
  )
ggsave("analysis_results/water_resistance_interaction_plots.png", interaction_plots, width = 12, height = 8)

# Box plots
box_plots <- data_analysis %>%
  ggplot(aes(
    x = interaction(LeimCode, LeimgradCode, WoodCategory),
    y = Value,
    fill = WoodCategory
  )) +
  geom_boxplot(
    alpha = 0.8,
    color = "black",
    size = 0.5,
    outlier.shape = NA
  ) +
  geom_jitter(
    aes(color = WoodCategory),
    position = position_jitterdodge(jitter.width = 0.1),
    alpha = 0.5,
    size = 2
  ) +
  facet_wrap(
    ~Property,
    scales = "free_y",
    labeller = labeller(
      Property = c(
        "Quell2h" = "2h Thickness Swelling",
        "Quell24h" = "24h Thickness Swelling",
        "WA2h" = "2h Water Absorption",
        "WA24h" = "24h Water Absorption"
      )
    )
  ) +
  scale_fill_manual(
    values = c("Hardwood" = "#1f77b4", "Softwood" = "#ff7f0e")
  ) +
  scale_color_manual(
    values = c("Hardwood" = "#1f77b4", "Softwood" = "#ff7f0e")
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    strip.text = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.title = element_text(size = 11, face = "bold")
  ) +
  labs(
    title = "Distribution of Water Resistance Properties",
    subtitle = "By Adhesive Type, Content, and Wood Category",
    x = "Treatment Combination",
    y = "Value (%)",
    fill = "Wood Category",
    color = "Wood Category"
  )
ggsave("analysis_results/water_resistance_box_plots.png", box_plots, width = 12, height = 8, dpi = 300)

cat("\nPLOTS SAVED\n")
cat("=================================================================\n")
cat("The following plots have been saved to the 'analysis_results' directory:\n")
cat("1. water_resistance_qq_plots.png: Normal Q-Q plots for assumption checking\n")
cat("2. water_resistance_interaction_plots.png: Interaction plots\n")
cat("3. water_resistance_box_plots.png: Box plots of water resistance properties\n")

# Close the output file
sink()
