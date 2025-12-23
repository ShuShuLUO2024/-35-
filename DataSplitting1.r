# T1_S1_Read data
library(readr)
library(dplyr)
data <- read_csv("C:/Users/zishu/Documents/MyGitHub/HolzartenSpan2024UTF8.csv")
# View(data)
uf_data <- data %>%
  filter(LeimCode == "UF" &
    Primer == "no primer")
# View(uf_data)
# write_csv(uf_data, "C:/Users/zishu/Documents/MyGitHub/uf_data_filtered.csv")
pmdi_data <- data %>%
  filter(LeimCode == "PMDI" &
    Primer == "no primer")
# View(pmdi_data)
# write_csv(pmdi_data, "C:/Users/zishu/Documents/MyGitHub/pmdi_data_filtered.csv")


uf_data1 <- data %>%
  filter(LeimCode == "UF" &
    Primer == "melamine primer")
View(uf_data1)
write_csv(uf_data1, "C:/Users/zishu/Documents/MyGitHub/uf_data_primer.csv")
pmdi_data1 <- data %>%
  filter(LeimCode == "PMDI" &
    Primer == "melamine primer")
# View(pmdi_data1)
write_csv(pmdi_data1, "C:/Users/zishu/Documents/MyGitHub/pmdi_data_primer.csv")
