library(readr)
library(dplyr)
library(ggplot2)
library(moments)
library(gridExtra)
library(reshape2)
library(tidyverse)
data <- read_csv("C:/Users/zishu/Documents/MyGitHub/uf_data_filtered.csv")
View(data)

numeric_cols <- c(
  "Rohd.Biege", "MOE", "MOR", "Rohd.Querz", "IB",
  "Dens_mitt", "Dens_max", "Dens_min",
  "Quell2h", "Quell24h", "WA2h", "WA24h"
)

# numeric_cols <- c(
#   "Rohd.Biege", "MOE", "MOR", "Rohd.Querz", "IB"
# )

# numeric_cols <- c(
#   "Dens_mitt", "Dens_max", "Dens_min"
# )

# numeric_cols <- c(
#   "Quell2h", "Quell24h", "WA2h", "WA24h"
# )

calculate_statistics <- function(x) {
  c(
    Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    Range = diff(range(x, na.rm = TRUE)),
    CV = sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100,
    Skewness = skewness(x, na.rm = TRUE),
    Kurtosis = kurtosis(x, na.rm = TRUE)
  )
}

stats_by_group <- data %>%
  group_by(HolzartCode, LeimgradCode) %>%
  summarise(
    across(
      all_of(numeric_cols),
      list(
        Mean = ~ mean(., na.rm = TRUE),
        Median = ~ median(., na.rm = TRUE),
        SD = ~ sd(., na.rm = TRUE),
        Min = ~ min(., na.rm = TRUE),
        Max = ~ max(., na.rm = TRUE),
        Range = ~ diff(range(., na.rm = TRUE)),
        CV = ~ sd(., na.rm = TRUE) / mean(., na.rm = TRUE) * 100,
        Skewness = ~ skewness(., na.rm = TRUE),
        Kurtosis = ~ kurtosis(., na.rm = TRUE)
      )
    ),
    .groups = "drop"
  )

print("=== Table1:Statistical analysis by tree species and sizing concentration ===")
print(stats_by_group)
# write.csv(stats_by_group, "C:/Users/zishu/Documents/MyGitHub/ufgroup_highLow.csv")
