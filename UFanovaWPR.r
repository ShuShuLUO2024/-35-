# Wood Type & UF Level Influence on water resistance properties
# Load required packages
library(tidyverse) # Data processing and visualization
library(car) # For Levene's test
library(rstatix) # Statistical analysis
library(ggpubr) # Advanced plotting
library(effectsize) # Effect size calculation

# 1. Data Preparation ----
data <- read_csv("C:/Users/zishu/Documents/MyGitHub/uf_data_filtered.csv")
View(data)

##########################################
# Data preprocessing
##########################################
data_analysis <- data %>%
  # Filter UF boards without primer
  filter(LeimCode == "UF", Primer == "no primer") %>%
  # Classify wood types
  mutate(
    WoodCategory = case_when(
      HolzartCode %in% c("Oak", "Beech", "Poplar") ~ "Hardwood",
      TRUE ~ "Softwood"
    )
  ) %>%
  # Convert to long format for moisture properties
  pivot_longer(
    cols = c(Quell2h, Quell24h, WA2h, WA24h), # Moisture-related properties
    names_to = "Property",
    values_to = "Value"
  )

##########################################
# 2. Assumption Testing ----
# Normality test (Shapiro-Wilk) n<50
##########################################
normality_test <- data_analysis %>%
  group_by(Property, WoodCategory, LeimgradCode) %>%
  shapiro_test(Value) %>%
  mutate(
    Normality = if_else(p > 0.05, "Normal", "Non-normal")
  )

# Create Q-Q plots
qq_plots <- data_analysis %>%
  ggplot(aes(sample = Value)) +
  geom_qq() +
  geom_qq_line() +
  facet_grid(Property ~ interaction(WoodCategory, LeimgradCode),
    scales = "free"
  ) +
  theme_bw() +
  labs(
    title = "Normal Q-Q Plots for Moisture Properties",
    subtitle = "By Wood Category and Resin Content"
  )

cat("1. Normality Test Results (Shapiro-Wilk):\n")
print(normality_test)
# print(qq_plots)
# ggsave("moisture_QQ_plots.png", qq_plots, width = 12, height = 8)

################################################
# 3. Homogeneity of variance test (Levene's test)
################################################
levene_results <- data_analysis %>%
  group_by(Property) %>%
  summarise(
    levene_test = list(
      leveneTest(Value ~ WoodCategory * LeimgradCode)
    )
  ) %>%
  mutate(
    Homogeneity = if_else(
      map_dbl(levene_test, ~ .$`Pr(>F)`[1]) > 0.05,
      "Homogeneous",
      "Non-homogeneous"
    )
  )

cat("\n2. Homogeneity of Variance Test Results (Levene's Test):\n")
print(levene_results)

# ###############################################
# # 4. ANOVA Analysis
# # Based on Levene-test:
# - Quell24h: Welch-ANOVA
# - Quell2h: Welch-ANOVA
# - WA2h: Welch-ANOVA
# - WA24h: two-way ANOVA
# ###############################################
# Perform appropriate analysis based on assumptions
anova_results <- data_analysis %>%
  group_by(Property) %>%
  summarise(
    analysis_result = list({
      current_property <- first(Property)
      data_subset <- filter(data_analysis, Property == current_property)
      is_homogeneous <- current_property == "WA24h"

      if (!is_homogeneous) {
        # Use Welch's ANOVA for non-homogeneous variance
        welch_result <- oneway.test(
          Value ~ interaction(WoodCategory, LeimgradCode),
          data = data_subset,
          var.equal = FALSE
        )
        list(
          welch_test = welch_result,
          type = "welch"
        )
      } else {
        # Use regular two-way ANOVA
        aov_result <- aov(Value ~ WoodCategory * LeimgradCode,
          data = data_subset
        )
        list(
          aov_test = aov_result,
          type = "anova"
        )
      }
    })
  )

# Print ANOVA results
cat("\n3. Analysis Results:\n")
for (i in 1:nrow(anova_results)) {
  cat("\nResults for", anova_results$Property[i], ":\n")
  if (anova_results$analysis_result[[i]]$type == "welch") {
    print(anova_results$analysis_result[[i]]$welch_test)
  } else {
    print(summary(anova_results$analysis_result[[i]]$aov_test))
  }
}

# Calculate effect sizes
effect_sizes <- lapply(unique(data_analysis$Property), function(prop) {
  data_subset <- subset(data_analysis, Property == prop)
  model <- aov(Value ~ WoodCategory * LeimgradCode, data = data_subset)
  eta <- effectsize::eta_squared(model)
  data.frame(
    Property = prop,
    Parameter = eta$Parameter,
    Eta_squared = eta$Eta2,
    CI_low = eta$CI_low,
    CI_high = eta$CI_high,
    stringsAsFactors = FALSE
  )
}) %>% bind_rows()

cat("\n4. Effect Sizes Results:\n")
print(effect_sizes)

# ########################################
# # 5. Post-hoc Analysis
# ########################################
posthoc_results <- data_analysis %>%
  group_by(Property) %>%
  summarise(
    posthoc = list({
      current_property <- first(Property)
      data_subset <- filter(data_analysis, Property == current_property)
      is_homogeneous <- current_property == "WA24h"

      if (!is_homogeneous) {
        data_subset$group <- paste(
          data_subset$WoodCategory,
          data_subset$LeimgradCode,
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
      } else {
        aov_model <- aov(Value ~ WoodCategory * LeimgradCode,
          data = data_subset
        )
        tukey_result <- TukeyHSD(aov_model)

        list(
          test_type = "tukey",
          result = tukey_result
        )
      }
    })
  )

# Print post-hoc results
cat("\n5. Post-hoc Analysis Results:\n")
for (i in 1:nrow(posthoc_results)) {
  cat("\nPost-hoc Results for", posthoc_results$Property[i], ":\n")
  if (posthoc_results$posthoc[[i]]$test_type == "games_howell") {
    print(posthoc_results$posthoc[[i]]$result)
  } else {
    print(posthoc_results$posthoc[[i]]$result)
  }
}

# ##########################################
# # 6. Visualization
# ##########################################
# Interaction plots
interaction_plots <- data_analysis %>%
  ggplot(aes(x = LeimgradCode, y = Value, color = WoodCategory)) +
  geom_point(position = position_dodge(0.2)) +
  stat_summary(fun = mean, geom = "line", aes(group = WoodCategory)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  facet_wrap(~Property, scales = "free_y") +
  theme_bw() +
  labs(
    title = "Interaction Effects of Wood Category and Resin Content",
    subtitle = "On Moisture Properties",
    x = "UF Resin Content",
    y = "Value",
    color = "Wood Category"
  )

# print(interaction_plots)
# ggsave("moisture_interaction_plots.png", interaction_plots, width = 12, height = 6)

# Box plots with points
box_plots <- data_analysis %>%
  ggplot(aes(
    x = interaction(WoodCategory, LeimgradCode),
    y = Value,
    fill = WoodCategory
  )) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  facet_wrap(~Property, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Distribution of Moisture Properties",
    subtitle = "By Wood Category and UF Level",
    x = "Wood Category - UF Level",
    y = "Value",
    fill = "Wood Category"
  )

print(box_plots)
# ggsave("moisture_box_plots.png", box_plots, width = 12, height = 6)

# #########################################
# # 7. Descriptive Statistics
# #########################################
descriptive_stats <- data_analysis %>%
  group_by(Property, WoodCategory, LeimgradCode) %>%
  summarise(
    n = n(),
    mean = mean(Value),
    sd = sd(Value),
    se = sd / sqrt(n),
    ci_lower = mean - qt(0.975, n - 1) * se,
    ci_upper = mean + qt(0.975, n - 1) * se,
    .groups = "drop"
  )

cat("\n6. Descriptive Statistics:\n")
print(descriptive_stats)
