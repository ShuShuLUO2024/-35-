# Three-way ANOVA: Effects of Resine Type, resine level, and Wood Type on Mechanical Properties
# Load required packages
library(tidyverse) # Data processing and visualization
library(car) # For Levene's test
library(rstatix) # Statistical analysis
library(ggpubr) # Advanced plotting
library(effectsize) # Effect size calculation

# 1. Data Preparation ----
data <- read_csv("C:/Users/zishu/Documents/MyGitHub/combi_data_filtered.csv")
# View(data)

##########################################
# Data preprocessing
##########################################
data_analysis <- data %>%
  # Filter boards without primer
  filter(Primer == "no primer") %>%
  # Classify wood types
  mutate(
    WoodCategory = case_when(
      HolzartCode %in% c("Oak", "Beech", "Poplar") ~ "Hardwood",
      TRUE ~ "Softwood"
    )
  ) %>%
  # Convert to long format
  pivot_longer(
    cols = c(MOR, MOE, IB), # Mechanical properties
    names_to = "Property",
    values_to = "Value"
  )

##########################################
# 2. Assumption Testing
# 2.1 Normality test (Shapiro-Wilk test)
##########################################
normality_test <- data_analysis %>%
  group_by(Property, LeimCode, LeimgradCode, WoodCategory) %>%
  shapiro_test(Value) %>%
  mutate(
    Normality = if_else(p > 0.05, "Normal", "Non-normal")
  )

# Create Q-Q plots
qq_plots <- data_analysis %>%
  ggplot(aes(sample = Value)) +
  geom_qq() +
  geom_qq_line() +
  facet_grid(Property ~ interaction(LeimCode, LeimgradCode, WoodCategory),
    scales = "free"
  ) +
  theme_bw() +
  labs(
    title = "Normal Q-Q Plots",
    subtitle = "By Adhesive Type, Content, and Wood Category"
  )

cat("1. Normality Test Results (Shapiro-Wilk):\n")
print(normality_test)
print(qq_plots)
# ggsave("mechanical_QQ_plots.png", qq_plots, width = 15, height = 8)

################################################
# 2.2 Homogeneity of variance test (Levene's test)
################################################
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

cat("\n2. Homogeneity of Variance Test Results (Levene's Test):\n")
print(levene_results)

# ######################################################
# # 3. Three-way ANOVA
# Based on Levene's test results:
# - IB: Non-homogeneous variance, use Type III SS ANOVA
# - MOE: Homogeneous variance, use traditional ANOVA
# - MOR: Homogeneous variance, use traditional ANOVA
# ######################################################
anova_results <- data_analysis %>%
  group_by(Property) %>%
  summarise(
    analysis_result = list({
      current_property <- first(Property)
      data_subset <- filter(data_analysis, Property == current_property)

      # Select analysis method based on property
      if (current_property == "IB") {
        # For IB (non-homogeneous variance), use Type III SS
        model <- Anova(lm(Value ~ LeimCode * LeimgradCode * WoodCategory,
          data = data_subset
        ), type = 3)
        list(
          model_type = "Type III",
          result = model
        )
      } else {
        # For MOE and MOR (homogeneous variance), use traditional ANOVA
        model <- aov(Value ~ LeimCode * LeimgradCode * WoodCategory,
          data = data_subset
        )
        list(
          model_type = "Traditional",
          result = model
        )
      }
    })
  )

# Print ANOVA results
cat("\n3. Three-way ANOVA Results:\n")
for (i in 1:nrow(anova_results)) {
  cat("\nResults for", anova_results$Property[i], ":\n")
  if (anova_results$analysis_result[[i]]$model_type == "Type III") {
    print(anova_results$analysis_result[[i]]$result)
  } else {
    print(summary(anova_results$analysis_result[[i]]$result))
  }
}

# Calculate effect sizes
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

cat("\n4. Effect Size Results:\n")
print(effect_sizes)

# ########################################
# # 4. Post-hoc Analysis
# ########################################
posthoc_results <- data_analysis %>%
  group_by(Property) %>%
  summarise(
    posthoc = list({
      current_property <- first(Property)
      data_subset <- filter(data_analysis, Property == current_property)

      # Select post-hoc method based on property
      if (current_property == "IB") {
        # For IB (non-homogeneous variance), use Games-Howell
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
      } else {
        # For MOE and MOR (homogeneous variance), use Tukey HSD
        aov_model <- aov(Value ~ LeimCode * LeimgradCode * WoodCategory,
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
  print(posthoc_results$posthoc[[i]]$result)
}

##########################################
# 5. Visualization
##########################################
# Interaction plots
interaction_plots <- data_analysis %>%
  ggplot(aes(x = LeimgradCode, y = Value, color = WoodCategory)) +
  geom_point(position = position_dodge(0.2)) +
  stat_summary(fun = mean, geom = "line", aes(group = WoodCategory)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  facet_grid(Property ~ LeimCode) +
  theme_bw() +
  labs(
    title = "Interaction Effects of Adhesive Type, Content, and Wood Category",
    x = "Adhesive Content",
    y = "Mechanical Property Value",
    color = "Wood Category"
  )

# print(interaction_plots)
# ggsave("mechanical_interaction_plots.png", interaction_plots, width = 12, height = 8)

# Box plots
box_plots <- data_analysis %>%
  ggplot(aes(
    x = interaction(LeimCode, LeimgradCode, WoodCategory),
    y = Value,
    fill = WoodCategory
  )) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  facet_wrap(~Property, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Distribution of Mechanical Properties",
    subtitle = "By Adhesive Type, Content, and Wood Category",
    x = "Group Combination",
    y = "Mechanical Property Value",
    fill = "Wood Category"
  )

# print(box_plots)
# ggsave("mechanical_box_plots.png", box_plots, width = 12, height = 6)

#########################################
# 6. Descriptive Statistics
#########################################
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

cat("\n6. Descriptive Statistics:\n")
print(descriptive_stats)
