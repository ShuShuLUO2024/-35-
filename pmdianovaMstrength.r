# Woodtype&PMDI-level influence on the M-Strength
# Load required packages
library(tidyverse) # Data processing and visualization
library(car) # For Levene's test
library(rstatix) # Statistical analysis
library(ggpubr) # Advanced plotting
library(effectsize) # Effect size calculation

# 1. Data Preparation ----
data <- read_csv("C:/Users/zishu/Documents/MyGitHub/pmdi_data_filtered.csv")
View(data)

##########################################
# Data preprocessing
##########################################
data_analysis <- data %>%
  # Filter PMDI boards without primer
  filter(LeimCode == "PMDI", Primer == "no primer") %>%
  # Classify wood types
  mutate(
    WoodCategory = case_when(
      HolzartCode %in% c("Oak", "Beech", "Poplar") ~ "Hardwood",
      TRUE ~ "Softwood"
    )
  ) %>%
  # Convert to long format
  pivot_longer(
    cols = c(MOR, MOE, IB),
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
    title = "Normal Q-Q Plots for Mechanical Properties",
    subtitle = "By Wood Category and PMDI Content"
  )

cat("1. Normality Test Results (Shapiro-Wilk):\n")
print(normality_test)
print(qq_plots)
# ggsave("QQ_plots.png", qq_plots, width = 12, height = 8)

################################################
# 3.Homogeneity of variance test (Levene's test)
# A key prerequisite for ANOVA
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

###############################################
# 4. Modified ANOVA Analysis (IB non-homo)
###############################################
# Separate analysis for IB and other properties_Welch's ANOVA
# anova_results <- data_analysis %>%
#   group_by(Property) %>%
#   summarise(
#     analysis_result = list({
#       current_property <- first(Property)
#       if (current_property == "IB") {
#         # For IB: Use Welch's ANOVA
#         data_subset <- data_analysis %>%
#           filter(Property == "IB")
#         welch_result <- oneway.test(
#           Value ~ interaction(WoodCategory, LeimgradCode),
#           data = data_subset,
#           var.equal = FALSE
#         )
#         # Store the result in a format compatible with later processing
#         list(
#           welch_test = welch_result,
#           type = "welch"
#         )
#       } else {
#         # For other properties: Use regular two-way ANOVA
#         data_subset <- data_analysis %>%
#           filter(Property == current_property)
#         aov_result <- aov(Value ~ WoodCategory * LeimgradCode,
#           data = data_subset
#         )
#         list(
#           aov_test = aov_result,
#           type = "anova"
#         )
#       }
#     })
#   )

# # Print ANOVA results with appropriate handling
# cat("\n3. Analysis Results:\n")
# for (i in 1:nrow(anova_results)) {
#   cat("\nResults for", anova_results$Property[i], ":\n")
#   if (anova_results$analysis_result[[i]]$type == "welch") {
#     print(anova_results$analysis_result[[i]]$welch_test)
#   } else {
#     print(summary(anova_results$analysis_result[[i]]$aov_test))
#   }
# }

# Two-way ANOVA ----Perform ANOVA for each property
anova_results <- data_analysis %>%
  group_by(Property) %>%
  summarise(
    aov_result = list(
      aov(Value ~ WoodCategory * LeimgradCode)
    )
  )

cat("\n3. Two-way ANOVA Results:\n")
for (i in 1:nrow(anova_results)) {
  cat("\nANOVA Results for", anova_results$Property[i], ":\n")
  print(summary(anova_results$aov_result[[i]]))
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

# Print effect sizes results
cat("\n4. Effect Sizes Results:\n")
print(effect_sizes)

# ########################################
# 5. Post-hoc Analysis ----
# #########################################
# Separate post-hoc analysis for IB and other properties
posthoc_results <- data_analysis %>%
  group_by(Property) %>%
  summarise(
    posthoc = list({
      current_property <- first(Property)
      data_subset <- filter(data_analysis, Property == current_property)

      if (current_property == "IB") {
        # Create grouping factor for IB
        data_subset$group <- interaction(
          data_subset$WoodCategory,
          # data_subset$HolzartCode,
          data_subset$LeimgradCode
        )

        # Games-Howell post-hoc test for IB
        games_howell <- games_howell_test(
          data = data_subset,
          formula = Value ~ group
          # formula = HolzartCode ~ group
        )
        list(
          test_type = "games_howell",
          result = games_howell
        )
      } else {
        # Regular Tukey HSD for other properties
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

# Print results
cat("\n5. Post-hoc Analysis Results:\n")
for (i in 1:nrow(posthoc_results)) {
  cat("\nPost-hoc Results for", posthoc_results$Property[i], ":\n")
  if (posthoc_results$posthoc[[i]]$test_type == "games_howell") {
    print(posthoc_results$posthoc[[i]]$result)
  } else {
    print(posthoc_results$posthoc[[i]]$result)
  }
}

# # Tukey's HSD test
# tukey_results <- data_analysis %>%
#   group_by(Property) %>%
#   summarise(
#     tukey = list(
#       TukeyHSD(aov(Value ~ interaction(WoodCategory, LeimgradCode)))
#     )
#   )

# cat("\n5. Post-hoc Analysis (Tukey HSD):\n")
# for (i in 1:nrow(tukey_results)) {
#   cat("\nTukey HSD Results for", tukey_results$Property[i], ":\n")
#   print(tukey_results$tukey[[i]])
# }

# ##########################################
# # Visualization ----
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
    x = "PMDI Resin Content",
    y = "Value",
    color = "Wood Category"
  )

# print(interaction_plots)
# ggsave("Interaction_plots.png", interaction_plots, width = 12, height = 6)

# Create box plots with points
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
    title = "Distribution of Mechanical Properties",
    subtitle = "By Wood Category and PMDI level",
    x = "Wood Category - PMDI level",
    y = "Value",
    fill = "Wood Category"
  )

# Display the plot
# print(box_plots)
# ggsave("Box_plots.png", box_plots, width = 12, height = 6)

# Specific visualization for IB to show its non-homogeneous nature
ib_specific_plot <- data_analysis %>%
  filter(Property == "IB") %>%
  ggplot(aes(x = interaction(WoodCategory, LeimgradCode), y = Value, fill = WoodCategory)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "IB Distribution (Non-homogeneous Variance)",
    subtitle = "Analyzed using Welch's ANOVA",
    x = "Wood Category - Resin Content",
    y = "IB Value",
    fill = "Wood Category"
  )

# Display all plots
# print(interaction_plots)
# print(box_plots)
# print(ib_specific_plot)

# Save plots
# ggsave("IB_specific_plot.png", ib_specific_plot, width = 10, height = 6)

#########################################
# 6. Descriptive Statistics ----
#########################################
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
