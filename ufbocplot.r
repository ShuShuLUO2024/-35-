library(ggplot2)
library(tidyr)
library(dplyr)

data <- read_csv("C:/Users/zishu/Documents/MyGitHub/uf_data_filtered.csv")
# View(data)

############################################
# UF-MStrength
############################################
# Convert data to long format and add property labels with units
data_long <- data %>%
  select(LeimgradCode, HolzartCode, MOR, MOE, IB) %>%
  pivot_longer(
    cols = c(MOR, MOE, IB),
    names_to = "Property",
    values_to = "Value"
  ) %>%
  mutate(Property = factor(Property,
    levels = c("MOR", "MOE", "IB"),
    labels = c("MOR (MPa)", "MOE (MPa)", "IB (MPa)")
  ))

# Create boxplots with extended adjacent values
p1 <- ggplot(data_long, aes(x = LeimgradCode, y = Value)) +
  # Add boxplot with modified whiskers
  stat_boxplot(geom = "errorbar", width = 0.5) + # Add whisker ends
  geom_boxplot(
    aes(fill = LeimgradCode),
    outlier.shape = NA, # Hide default outliers
    coef = 1.5, # Whisker extent (1.5 * IQR)
    width = 0.5, # Box width
    alpha = 0.7 # Make boxes slightly transparent
  ) +
  # Add individual points with jitter, colored by wood type
  geom_point(
    aes(color = HolzartCode), # Color points by wood type
    position = position_jitter(width = 0.2, height = 0), # Add horizontal jitter only
    alpha = 0.6, # Point transparency
    size = 2 # Point size
  ) +
  # Facet by property with free y scales
  facet_wrap(~Property, scales = "free_y") +
  theme_bw() +
  labs(
    title = "Mechanical Properties Boxplots by UF Degree",
    x = "UF Degree",
    y = "Value",
    fill = "UF Degree",
    color = "Wood Type" # Add legend title for wood types
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(), # Remove minor grid lines
    panel.grid.major = element_line(color = "gray90"), # Lighten major grid lines
    legend.position = "right" # Place legends on the right
  ) +
  scale_fill_brewer(palette = "Set2") + # Colors for boxes
  scale_color_brewer(palette = "Set1") # Different colors for wood types

# Display the plot
print(p1)

# Save the plot with higher resolution
ggsave("mechanical_properties_boxplots.png", width = 14, height = 6, dpi = 300) # Increased width for legends

# # Convert data to long format and add property labels with units
# data_long <- data %>%
#   select(LeimgradCode, MOR, MOE, IB) %>%
#   pivot_longer(
#     cols = c(MOR, MOE, IB),
#     names_to = "Property",
#     values_to = "Value"
#   ) %>%
#   mutate(Property = factor(Property,
#     levels = c("MOR", "MOE", "IB"),
#     labels = c("MOR (MPa)", "MOE (MPa)", "IB (MPa)")
#   ))

# # Create boxplots with extended adjacent values
# p1 <- ggplot(data_long, aes(x = LeimgradCode, y = Value, fill = LeimgradCode)) +
#   # Add boxplot with modified whiskers
#   stat_boxplot(geom = "errorbar", width = 0.5) + # Add whisker ends
#   geom_boxplot(
#     outlier.shape = 21, # Outlier shape (filled circle)
#     outlier.size = 2, # Outlier size
#     outlier.fill = "white", # Outlier fill color
#     outlier.alpha = 0.7, # Outlier transparency
#     coef = 1.5, # Whisker extent (1.5 * IQR)
#     width = 0.5 # Box width
#   ) +
#   # Add individual points with jitter for better visualization
#   geom_jitter(
#     width = 0.2, # Jitter width
#     alpha = 0.4, # Point transparency
#     size = 1 # Point size
#   ) +

#   # Facet by property with free y scales
#   facet_wrap(~Property, scales = "free_y") +
#   theme_bw() +
#   labs(
#     title = "Mechanical Properties Boxplots by Gluing Degree",
#     x = "UF Degree",
#     y = "Value"
#   ) +
#   theme(
#     plot.title = element_text(hjust = 0.5, size = 14),
#     axis.title = element_text(size = 12),
#     strip.text = element_text(size = 12, face = "bold"),
#     legend.title = element_text(size = 10),
#     legend.text = element_text(size = 9),
#     panel.grid.minor = element_blank(), # Remove minor grid lines
#     panel.grid.major = element_line(color = "gray90") # Lighten major grid lines
#   ) +
#   scale_fill_brewer(palette = "Set2")

# # Display the plot
# print(p1)

# # Save the plot with higher resolution
# ggsave("mechanical_properties_boxplots.png", width = 12, height = 6, dpi = 300)

# # # Output detailed statistical summary including adjacent values
# # for (prop in c("MOR", "MOE", "IB")) {
# #   cat("\n", prop, "Summary Statistics:\n")
# #   # Calculate comprehensive statistics by group
# #   stats <- data %>%
# #     group_by(LeimgradCode) %>%
# #     summarise(
# #       n = n(),
# #       Mean = mean(!!sym(prop), na.rm = TRUE),
# #       SD = sd(!!sym(prop), na.rm = TRUE),
# #       Min = min(!!sym(prop), na.rm = TRUE),
# #       Lower_Adjacent = quantile(!!sym(prop), 0.25, na.rm = TRUE) -
# #         1.5 * IQR(!!sym(prop), na.rm = TRUE),
# #       Q1 = quantile(!!sym(prop), 0.25, na.rm = TRUE),
# #       Median = median(!!sym(prop), na.rm = TRUE),
# #       Q3 = quantile(!!sym(prop), 0.75, na.rm = TRUE),
# #       Upper_Adjacent = quantile(!!sym(prop), 0.75, na.rm = TRUE) +
# #         1.5 * IQR(!!sym(prop), na.rm = TRUE),
# #       Max = max(!!sym(prop), na.rm = TRUE),
# #       IQR = IQR(!!sym(prop), na.rm = TRUE)
# #     )
# #   print(stats)
# # }

############################################
# UFWaterR - performance
############################################
# Convert data to long format and add property labels with units
data_long <- data %>%
  select(LeimgradCode, HolzartCode, Quell2h, Quell24h, WA2h, WA24h) %>%
  pivot_longer(
    cols = c(Quell2h, Quell24h, WA2h, WA24h),
    names_to = "Property",
    values_to = "Value"
  ) %>%
  mutate(Property = factor(Property,
    levels = c("WA2h", "WA24h", "Quell2h", "Quell24h"),
    labels = c(
      "Water Absorption 2h (%)",
      "Water Absorption 24h (%)",
      "Thickness Swelling 2h (%)",
      "Thickness Swelling 24h (%)"
    )
  ))

# Create boxplots with extended adjacent values and colored points by wood type
p <- ggplot(data_long, aes(x = LeimgradCode, y = Value)) +
  # Add whisker ends (T-bars)
  stat_boxplot(geom = "errorbar", width = 0.5) +
  # Add boxplot
  geom_boxplot(
    aes(fill = LeimgradCode),
    outlier.shape = NA, # Hide default outliers
    coef = 1.5, # Whisker extent (1.5 * IQR)
    width = 0.3, # Box width
    alpha = 0.7 # Make boxes slightly transparent
  ) +
  # Add individual points, colored by wood type
  geom_point(
    aes(color = HolzartCode), # Color points by wood type
    position = position_jitter(width = 0.2, height = 0), # Add horizontal jitter only
    alpha = 0.6, # Point transparency
    size = 2 # Point size
  ) +
  # Facet by property with free y scales
  facet_wrap(~Property, scales = "free_y", ncol = 2) +
  theme_bw() +
  labs(
    title = "Water-related Properties Boxplots by UF Degree",
    x = "UF Degree",
    y = "Value (%)",
    fill = "UF Degree",
    color = "Wood Type" # Add legend title for wood types
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    legend.position = "right" # Place legends on the right
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set1") # Different color palette for wood types

# Display the plot
print(p)

# Save the plot with adjusted dimensions for 4 panels
ggsave("water_properties_boxplots.png", width = 14, height = 10, dpi = 300)

# # Output detailed statistical summary by gluing degree and wood type
# for (prop in c("WA2h", "WA24h", "Quell2h", "Quell24h")) {
#   cat("\n", prop, "Summary Statistics by Gluing Degree and Wood Type:\n")
#   # Calculate comprehensive statistics by group
#   stats <- data %>%
#     group_by(LeimgradCode, HolzartCode) %>%
#     summarise(
#       n = n(),
#       Mean = mean(!!sym(prop), na.rm = TRUE),
#       SD = sd(!!sym(prop), na.rm = TRUE),
#       Min = min(!!sym(prop), na.rm = TRUE),
#       Q1 = quantile(!!sym(prop), 0.25, na.rm = TRUE),
#       Median = median(!!sym(prop), na.rm = TRUE),
#       Q3 = quantile(!!sym(prop), 0.75, na.rm = TRUE),
#       Max = max(!!sym(prop), na.rm = TRUE),
#       IQR = IQR(!!sym(prop), na.rm = TRUE)
#     ) %>%
#     arrange(LeimgradCode, HolzartCode)
#   print(stats)
# }

############################################
# UFdensity
############################################
# Convert data to long format and add property labels with units
data_long <- data %>%
  select(
    LeimgradCode, HolzartCode,
    Dens_min, Dens_mitt, Dens_max
  ) %>%
  pivot_longer(
    cols = c(Dens_min, Dens_mitt, Dens_max),
    names_to = "Property",
    values_to = "Value"
  ) %>%
  mutate(Property = factor(Property,
    levels = c("Dens_min", "Dens_mitt", "Dens_max"),
    labels = c(
      "Density_min (kg/m³)",
      "Density_mitt (kg/m³)",
      "Density_max (kg/m³)"
    )
  ))

# Create boxplots with extended adjacent values
p2 <- ggplot(data_long, aes(x = LeimgradCode, y = Value)) +
  # Add whisker ends (T-bars)
  stat_boxplot(geom = "errorbar", width = 0.5) +
  # Add boxplot
  geom_boxplot(
    aes(fill = LeimgradCode),
    outlier.shape = NA, # Hide default outliers
    coef = 1.5, # Whisker extent (1.5 * IQR)
    width = 0.5, # Box width
    alpha = 0.7 # Make boxes slightly transparent
  ) +
  # Add individual points, colored by wood type
  geom_point(
    aes(color = HolzartCode), # Color points by wood type
    position = position_jitter(width = 0.2, height = 0), # Add horizontal jitter only
    alpha = 0.6, # Point transparency
    size = 2 # Point size
  ) +
  # Facet by property with free y scales
  facet_wrap(~Property, scales = "free_y", ncol = 3) +
  theme_bw() +
  labs(
    title = "Density Distribution Boxplots by UF Degree",
    x = "UF Degree",
    y = "Density (kg/m³)",
    fill = "UF Degree",
    color = "Wood Type" # Add legend title for wood types
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    legend.position = "right" # Place legends on the right
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set1") # Different color palette for wood types

# Display the plot
print(p2)

# Save the plot
ggsave("density_distribution_boxplots.png", width = 14, height = 6, dpi = 300)

# # Output detailed statistical summary by gluing degree and wood type
# properties <- c("Dens_min", "Dens_mitt", "Dens_max")
# for (prop in properties) {
#   cat("\n", prop, "Summary Statistics by Gluing Degree and Wood Type:\n")
#   # Calculate comprehensive statistics by group
#   stats <- data %>%
#     group_by(LeimgradCode, HolzartCode) %>%
#     summarise(
#       n = n(),
#       Mean = mean(!!sym(prop), na.rm = TRUE),
#       SD = sd(!!sym(prop), na.rm = TRUE),
#       Min = min(!!sym(prop), na.rm = TRUE),
#       Q1 = quantile(!!sym(prop), 0.25, na.rm = TRUE),
#       Median = median(!!sym(prop), na.rm = TRUE),
#       Q3 = quantile(!!sym(prop), 0.75, na.rm = TRUE),
#       Max = max(!!sym(prop), na.rm = TRUE),
#       IQR = IQR(!!sym(prop), na.rm = TRUE)
#     ) %>%
#     arrange(LeimgradCode, HolzartCode)
#   print(stats)
# }
