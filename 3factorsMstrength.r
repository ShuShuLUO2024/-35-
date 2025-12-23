# Load required packages
library(tidyverse)
library(car)
library(rstatix)
library(ggpubr)
library(effectsize)

# Set output options
options(width = 10000)
options(max.print = 1000000)

# Create results directory
dir.create("analysis_results", showWarnings = FALSE)

# Create output file
fileConn <- file("analysis_results/complete_analysis_results.txt", "w", encoding = "UTF-8")

# Function for writing results
write_results <- function(text, fileConn) {
  writeLines(text, fileConn, useBytes = TRUE)
}

# Start analysis
write_results("=================================================================", fileConn)
write_results("THREE-WAY ANOVA ANALYSIS OF MECHANICAL PROPERTIES", fileConn)
write_results("=================================================================\n", fileConn)

# 1. Data Preparation
write_results("DATA PREPARATION", fileConn)
write_results("=================================================================", fileConn)
data <- read_csv("C:/Users/zishu/Documents/MyGitHub/combi_data_filtered.csv")
write_results("Data loaded successfully\n", fileConn)

data_analysis <- data %>%
  filter(Primer == "no primer") %>%
  mutate(
    WoodCategory = case_when(
      HolzartCode %in% c("Oak", "Beech", "Poplar") ~ "Hardwood",
      TRUE ~ "Softwood"
    )
  ) %>%
  pivot_longer(
    cols = c(MOR, MOE, IB),
    names_to = "Property",
    values_to = "Value"
  )

# 2. Assumption Testing
write_results("ASSUMPTION TESTING", fileConn)
write_results("=================================================================", fileConn)
write_results("\n1. Normality Test Results (Shapiro-Wilk):", fileConn)
write_results("-------------------------------------------", fileConn)

normality_test <- data_analysis %>%
  group_by(Property, LeimCode, LeimgradCode, WoodCategory) %>%
  shapiro_test(Value) %>%
  mutate(
    Normality = if_else(p > 0.05, "Normal", "Non-normal")
  ) %>%
  ungroup()

write_results(capture.output(as.data.frame(normality_test)), fileConn)

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
ggsave("analysis_results/qq_plots.png", qq_plots, width = 15, height = 8)

write_results("\n2. Homogeneity of Variance Test Results (Levene's Test):", fileConn)
write_results("-------------------------------------------", fileConn)

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

write_results(capture.output(levene_results), fileConn)

# 3. Three-way ANOVA
write_results("\nTHREE-WAY ANOVA RESULTS", fileConn)
write_results("=================================================================", fileConn)

# Perform ANOVA for each property
for (prop in unique(data_analysis$Property)) {
  write_results(paste("\nResults for", prop, ":"), fileConn)
  write_results("-------------------------------------------", fileConn)

  data_subset <- filter(data_analysis, Property == prop)

  if (prop == "IB") {
    model <- Anova(lm(Value ~ LeimCode * LeimgradCode * WoodCategory,
      data = data_subset
    ), type = 3)
    write_results(capture.output(model), fileConn)
  } else {
    model <- aov(Value ~ LeimCode * LeimgradCode * WoodCategory,
      data = data_subset
    )
    write_results(capture.output(summary(model)), fileConn)
  }
}

# Effect Sizes
write_results("\nEFFECT SIZES", fileConn)
write_results("=================================================================", fileConn)

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

write_results(capture.output(as.data.frame(effect_sizes)), fileConn)

# Post-hoc Analysis
write_results("\nPOST-HOC ANALYSIS RESULTS", fileConn)
write_results("=================================================================", fileConn)

for (prop in unique(data_analysis$Property)) {
  write_results(paste("\nPost-hoc Results for", prop, ":"), fileConn)
  write_results("-------------------------------------------", fileConn)

  data_subset <- filter(data_analysis, Property == prop)

  if (prop == "IB") {
    data_subset$group <- interaction(
      data_subset$LeimCode,
      data_subset$LeimgradCode,
      data_subset$WoodCategory,
      sep = "_"
    )
    results <- games_howell_test(
      data = data_subset,
      Value ~ group,
      conf.level = 0.95
    )
    write_results(capture.output(as.data.frame(results)), fileConn)
  } else {
    model <- aov(Value ~ LeimCode * LeimgradCode * WoodCategory,
      data = data_subset
    )
    results <- TukeyHSD(model)
    write_results(capture.output(results), fileConn)
  }
}

# Descriptive Statistics
write_results("\nDESCRIPTIVE STATISTICS", fileConn)
write_results("=================================================================", fileConn)

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

write_results(capture.output(as.data.frame(descriptive_stats)), fileConn)

# Create plots
# Interaction plots
interaction_plots <- data_analysis %>%
  ggplot(aes(x = LeimgradCode, y = Value, color = WoodCategory)) +
  geom_point(position = position_dodge(0.2)) +
  stat_summary(fun = mean, geom = "line", aes(group = WoodCategory)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  facet_grid(Property ~ LeimCode) +
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
    y = "Mechanical Property Value",
    color = "Wood Category"
  )
ggsave("analysis_results/interaction_plots.png", interaction_plots, width = 12, height = 8)

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
  facet_wrap(~Property, scales = "free_y") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    strip.text = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.title = element_text(size = 11, face = "bold")
  ) +
  labs(
    title = "Distribution of Mechanical Properties",
    subtitle = "By Adhesive Type, Content, and Wood Category",
    x = "Treatment Combination",
    y = "Mechanical Property Value",
    fill = "Wood Category"
  )
ggsave("analysis_results/box_plots.png", box_plots, width = 12, height = 6)

write_results("\nPLOTS SAVED", fileConn)
write_results("=================================================================", fileConn)
write_results("The following plots have been saved to the 'analysis_results' directory:", fileConn)
write_results("1. qq_plots.png: Normal Q-Q plots for assumption checking", fileConn)
write_results("2. interaction_plots.png: Interaction plots", fileConn)
write_results("3. box_plots.png: Box plots of mechanical properties", fileConn)

# Close the connection
close(fileConn)
