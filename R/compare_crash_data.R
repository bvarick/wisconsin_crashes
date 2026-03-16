library(tidyverse)

data1 <- read_csv(file = "/home/ben/Downloads/crash-data-download.csv")
data2 <- read_csv(file = "data/TOPS/crash-data-download_2025.csv")

anti_join(data1, data2)
anti_join(data2, data1)
