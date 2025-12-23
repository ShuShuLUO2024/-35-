library(tidyverse)
library(ggplot2)
library(dplyr)
library(reshape2)
library(gridExtra)
data <- read_csv("C:/Users/zishu/Documents/MyGitHub/uf_data_filtered.csv")
View(data)

# data$glue_group <- ifelse(data$LeimgradCode == "High", "High", "Low")
# print(unique(data$LeimgradCode))
# print(table(data$LeimgradCode))
# print(table(data$glue_group))
# print(head(data))

# M-stength
mech_props <- c("MOE", "MOR", "IB")

calc_stats <- function(x) {
  c(
    Mean = round(mean(x, na.rm = TRUE), 2),
    Median = round(median(x, na.rm = TRUE), 2),
    SD = round(sd(x, na.rm = TRUE), 2),
    Min = round(min(x, na.rm = TRUE), 2),
    Max = round(max(x, na.rm = TRUE), 2),
    Range = round(diff(range(x, na.rm = TRUE)), 2),
    CV = round(sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100, 2),
    Skewness = round(skewness(x, na.rm = TRUE), 2),
    Kurtosis = round(kurtosis(x, na.rm = TRUE), 2)
  )
}

for (prop in mech_props) {
  cat("\n=== DS-Analysis", prop, "===\n")
  cat("\nStatistical results grouped by uf level:\n")
  stats <- by(data[[prop]], data$LeimgradCode, calc_stats)
  print(stats)
}

stats_summary1 <- data %>%
  # group_by(HolzartCode, LeimgradCode) %>%
  group_by(LeimgradCode) %>%
  summarise(
    across(
      all_of(water_props),
      list(
        mean = ~ round(mean(., na.rm = TRUE), 2),
        median = ~ round(median(., na.rm = TRUE), 2),
        sd = ~ round(sd(., na.rm = TRUE), 2),
        min = ~ round(min(., na.rm = TRUE), 2),
        max = ~ round(max(., na.rm = TRUE), 2),
        range = ~ round(diff(range(., na.rm = TRUE)), 2),
        cv = ~ round(sd(., na.rm = TRUE) / mean(., na.rm = TRUE) * 100, 2),
        skew = ~ round(skewness(., na.rm = TRUE), 2),
        kurt = ~ round(kurtosis(., na.rm = TRUE), 2)
      )
    )
  )
# summarise(
#   across(
#     all_of(mech_props),
#     list(
#       mean = ~ mean(., na.rm = TRUE),
#       median = ~ median(., na.rm = TRUE),
#       sd = ~ sd(., na.rm = TRUE),
#       min = ~ min(., na.rm = TRUE),
#       max = ~ max(., na.rm = TRUE),
#       range = ~ diff(range(., na.rm = TRUE)),
#       cv = ~ sd(., na.rm = TRUE) / mean(., na.rm = TRUE) * 100,
#       skew = ~ skewness(., na.rm = TRUE),
#       kurt = ~ kurtosis(., na.rm = TRUE)
#     )
#   )
# )


cat("\n=== UFsummary_MStrength ===\n")
print(stats_summary1)

# WR-peformance
water_props <- c("Quell2h", "Quell24h", "WA2h", "WA24h")

calc_stats <- function(x) {
  c(
    Mean = round(mean(x, na.rm = TRUE), 2),
    Median = round(median(x, na.rm = TRUE), 2),
    SD = round(sd(x, na.rm = TRUE), 2),
    Min = round(min(x, na.rm = TRUE), 2),
    Max = round(max(x, na.rm = TRUE), 2),
    Range = round(diff(range(x, na.rm = TRUE)), 2),
    CV = round(sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100, 2),
    Skewness = round(skewness(x, na.rm = TRUE), 2),
    Kurtosis = round(kurtosis(x, na.rm = TRUE), 2)
  )
}

for (prop in water_props) {
  cat("\n=== DS-Analysis", prop, "===\n")
  cat("\nStatistical results grouped by uf level:\n")
  stats <- by(data[[prop]], data$LeimgradCode, calc_stats)
  print(stats)
}
stats_summary2 <- data %>%
  group_by(LeimgradCode) %>%
  # group_by(HolzartCode, LeimgradCode) %>%
  summarise(
    across(
      all_of(water_props),
      list(
        mean = ~ round(mean(., na.rm = TRUE), 2),
        median = ~ round(median(., na.rm = TRUE), 2),
        sd = ~ round(sd(., na.rm = TRUE), 2),
        min = ~ round(min(., na.rm = TRUE), 2),
        max = ~ round(max(., na.rm = TRUE), 2),
        range = ~ round(diff(range(., na.rm = TRUE)), 2),
        cv = ~ round(sd(., na.rm = TRUE) / mean(., na.rm = TRUE) * 100, 2),
        skew = ~ round(skewness(., na.rm = TRUE), 2),
        kurt = ~ round(kurtosis(., na.rm = TRUE), 2)
      )
    )
  )
# group_by(HolzartCode, LeimgradCode) %>%
# summarise(
#   across(
#     all_of(water_props),
#     list(
#       mean = ~ mean(., na.rm = TRUE),
#       median = ~ median(., na.rm = TRUE),
#       sd = ~ sd(., na.rm = TRUE),
#       min = ~ min(., na.rm = TRUE),
#       max = ~ max(., na.rm = TRUE),
#       range = ~ diff(range(., na.rm = TRUE)),
#       cv = ~ sd(., na.rm = TRUE) / mean(., na.rm = TRUE) * 100,
#       skew = ~ skewness(., na.rm = TRUE),
#       kurt = ~ kurtosis(., na.rm = TRUE)
#     )
#   )
# )

cat("\n=== UFsummary_WRP ===\n")
print(stats_summary2)
