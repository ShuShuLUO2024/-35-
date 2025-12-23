# Load required packages
library(tidyverse) # For data processing and visualization
library(car) # For ANOVA analysis
library(ggpubr) # For combining plots
library(rstatix) # For statistical analysis

data <- read_csv("C:/Users/zishu/Documents/MyGitHub/uf_data_filtered.csv")
# View(data)

# Data preprocessing
# Categorize wood types into hardwood and softwood groups
data$WoodType <- ifelse(data$HolzartCode %in% c("Beech", "Oak", "Poplar"),
  "Hardwood", "Softwood"
)

# Convert factors and rename levels
data$UF_Level <- data$LeimgradCode
data$WoodType <- factor(data$WoodType)
data$UF_Level <- factor(data$UF_Level)

# Generate descriptive statistics
summary_stats <- data %>%
  group_by(WoodType, UF_Level) %>%
  summarise(
    MOE_mean = mean(MOE),
    MOE_sd = sd(MOE),
    MOR_mean = mean(MOR),
    MOR_sd = sd(MOR),
    IB_mean = mean(IB),
    IB_sd = sd(IB),
    .groups = "drop"
  )

# Print descriptive statistics
print("************Descriptive Statistics************")
print(summary_stats)
# write.csv(summary_stats, "mechanical_properties_summary.csv")

##########################################
# Perform Two-way ANOVA analysis
##########################################
# 1. MOE Analysis
moe_aov <- aov(MOE ~ WoodType * UF_Level, data = data)
print("\nTwo-way ANOVA results for MOE:")
print(summary(moe_aov))

# 2. MOR Analysis
mor_aov <- aov(MOR ~ WoodType * UF_Level, data = data)
print("\nTwo-way ANOVA results for MOR:")
print(summary(mor_aov))

# 3. IB Analysis
ib_aov <- aov(IB ~ WoodType * UF_Level, data = data)
print("\n***********Two-way ANOVA results for IB************")
print(summary(ib_aov))

##########################################
# Create visualizations
##########################################
# 1. MOE Box Plot
moe_plot <- ggplot(data, aes(x = WoodType, y = MOE, fill = UF_Level)) +
  geom_boxplot() +
  labs(
    title = "MOE: Wood Type × UF Level Interaction",
    x = "Wood Type",
    y = "MOE (MPa)"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "top")

# 2. MOR Box Plot
mor_plot <- ggplot(data, aes(x = WoodType, y = MOR, fill = UF_Level)) +
  geom_boxplot() +
  labs(
    title = "MOR: Wood Type × UF Level Interaction",
    x = "Wood Type",
    y = "MOR (MPa)"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "top")

# 3. IB Box Plot
ib_plot <- ggplot(data, aes(x = WoodType, y = IB, fill = UF_Level)) +
  geom_boxplot() +
  labs(
    title = "IB: Wood Type × UF Level Interaction",
    x = "Wood Type",
    y = "IB (MPa)"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "top")

# Combine all plots
combined_plots <- ggarrange(moe_plot, mor_plot, ib_plot,
  ncol = 2, nrow = 2,
  common.legend = TRUE,
  legend = "top"
)

# Add overall title
annotate_figure(combined_plots,
  top = text_grob("Mechanical Properties by Wood Type and UF Level",
    face = "bold", size = 14
  )
)

# ggsave("boxplots.pdf", combined_plots, width = 12, height = 8)


# Perform post-hoc tests (if ANOVA results are significant)
###################
# Tukey HSD tests
###################
# For MOE
tukey_moe <- TukeyHSD(moe_aov)
print("\nTukey HSD test results for MOE:")
print(tukey_moe)

# For MOR
tukey_mor <- TukeyHSD(mor_aov)
print("\nTukey HSD test results for MOR:")
print(tukey_mor)

# For IB
tukey_ib <- TukeyHSD(ib_aov)
print("\nTukey HSD test results for IB:")
print(tukey_ib)

# Check ANOVA assumptions
#######################
# 1. Normality tests
#######################
print("\n***************Normality Tests (Shapiro-Wilk)*******************")
print("*******MOE residuals********")
print(shapiro.test(residuals(moe_aov)))
print("*******MOR residuals********")
print(shapiro.test(residuals(mor_aov)))
print("*******IB residuals********")
print(shapiro.test(residuals(ib_aov)))

######################################
# 2. Homogeneity of variance tests
######################################
print("\n***************Levene's Test for Homogeneity of Variance****************")
print("*******MOE*******")
print(leveneTest(MOE ~ WoodType * UF_Level, data = data))
print("*******MOR*******")
print(leveneTest(MOR ~ WoodType * UF_Level, data = data))
print("*******IB*******")
print(leveneTest(IB ~ WoodType * UF_Level, data = data))

######################################
# Calculate effect sizes (Eta squared)
######################################
calculate_eta_squared <- function(aov_model) {
  anova_table <- summary(aov_model)[[1]]
  ss_effect <- anova_table[, "Sum Sq"]
  ss_total <- sum(ss_effect)
  eta_squared <- ss_effect / ss_total
  names(eta_squared) <- rownames(anova_table)
  return(eta_squared)
}
print("\n*************Effect Size Analysis (Eta Squared)******************")
print("*******MOE effect sizes*******")
print(calculate_eta_squared(moe_aov))
print("*******MOR effect sizes*******")
print(calculate_eta_squared(mor_aov))
print("*******IB effect sizes*******")
print(calculate_eta_squared(ib_aov))

#############################################################
# Additional interaction plots for better visualization
#############################################################
# Create interaction plots for each mechanical property
moe_interaction <- ggplot(summary_stats, aes(x = WoodType, y = MOE_mean, color = UF_Level, group = UF_Level)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = MOE_mean - MOE_sd, ymax = MOE_mean + MOE_sd), width = 0.2) +
  labs(
    title = "MOE: Interaction between Wood Type and UF Level",
    y = "Mean MOE (MPa)"
  ) +
  theme_minimal()

mor_interaction <- ggplot(summary_stats, aes(x = WoodType, y = MOR_mean, color = UF_Level, group = UF_Level)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = MOR_mean - MOR_sd, ymax = MOR_mean + MOR_sd), width = 0.2) +
  labs(
    title = "MOR: Interaction between Wood Type and UF Level",
    y = "Mean MOR (MPa)"
  ) +
  theme_minimal()

ib_interaction <- ggplot(summary_stats, aes(x = WoodType, y = IB_mean, color = UF_Level, group = UF_Level)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = IB_mean - IB_sd, ymax = IB_mean + IB_sd), width = 0.2) +
  labs(
    title = "IB: Interaction between Wood Type and UF Level",
    y = "Mean IB (MPa)"
  ) +
  theme_minimal()

# Combine interaction plots
interaction_plots <- ggarrange(moe_interaction, mor_interaction, ib_interaction,
  ncol = 2, nrow = 2,
  common.legend = TRUE,
  legend = "top"
)

# ggsave("interaction_plots.pdf", interaction_plots, width = 12, height = 8)
