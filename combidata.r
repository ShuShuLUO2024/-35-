library(readr)
library(dplyr)
# file1 <- read.csv("C:/Users/zishu/Documents/MyGitHub/uf_data_filtered.csv")
# file2 <- read.csv("C:/Users/zishu/Documents/MyGitHub/pmdi_data_filtered.csv")

# print(colnames(file1))
# print(colnames(file2))

# merged_data <- rbind(file1, file2)

# View(merged_data)
# write.csv(merged_data, "C:/Users/zishu/Documents/MyGitHub/combi_data_filtered.csv", row.names = FALSE)

# file3 <- read.csv("C:/Users/zishu/Documents/MyGitHub/uf_data_primer.csv")
# file4 <- read.csv("C:/Users/zishu/Documents/MyGitHub/pmdi_data_primer.csv")

# print(colnames(file3))
# print(colnames(file4))

# merged_data <- rbind(file3, file4)

# View(merged_data)
# write.csv(merged_data, "C:/Users/zishu/Documents/MyGitHub/combi_data_primer.csv", row.names = FALSE)

# file5 <- read.csv("C:/Users/zishu/Documents/MyGitHub/combi_data_filtered.csv")
# file6 <- read.csv("C:/Users/zishu/Documents/MyGitHub/combi_data_primer.csv")

# print(colnames(file5))
# print(colnames(file6))

# merged_data1 <- rbind(file5, file6)

# View(merged_data1)
# write.csv(merged_data1, "C:/Users/zishu/Documents/MyGitHub/AllcombiHolz.csv", row.names = FALSE)

# file7 <- read.csv("C:/Users/zishu/Documents/MyGitHub/uf_data_filtered.csv")
# file8 <- read.csv("C:/Users/zishu/Documents/MyGitHub/uf_data_primer.csv")

# print(colnames(file7))
# print(colnames(file8))

# merged_data2 <- rbind(file7, file8)

# View(merged_data2)
# write.csv(merged_data2, "C:/Users/zishu/Documents/MyGitHub/UFcombi.csv", row.names = FALSE)

file9 <- read.csv("C:/Users/zishu/Documents/MyGitHub/pmdi_data_filtered.csv")
file10 <- read.csv("C:/Users/zishu/Documents/MyGitHub/pmdi_data_primer.csv")

print(colnames(file9))
print(colnames(file10))

merged_data3 <- rbind(file9, file10)

View(merged_data3)
write.csv(merged_data3, "C:/Users/zishu/Documents/MyGitHub/PMDIcombi.csv", row.names = FALSE)
