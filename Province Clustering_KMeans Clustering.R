# Load packages
library(car)
library(psych)
library(cluster)
library(tidyverse)
library(factoextra)
library(readxl)

# Load data
data <- read_xlsx("C:/Users/HP/Downloads/data_asis_statmul.xlsx") 

# Hapus kolom pertama (ID atau Nama)
data_1 <- data[,-1] 
View(data_1)

# Normalisasi data (opsional tapi direkomendasikan)
data_1 <- scale(data_1)

# Menentukan jumlah cluster optimal dengan silhouette
set.seed(123)
silhouette <- fviz_nbclust(data_1, kmeans, method = "silhouette") 
print(silhouette)

# Lakukan clustering dengan jumlah cluster = 3
set.seed(123)
final <- kmeans(data_1, 3)
print(final)

# Visualisasi cluster
fviz_cluster(final, data = data_1)

# Gabungkan hasil cluster ke data
final_result <- data.frame(Provinsi = data[[1]], data_1, Cluster = final$cluster)
print(final_result)
table(final_result$Cluster)

# (Opsional) Evaluasi kualitas cluster
sil <- silhouette(final$cluster, dist(data_1))
fviz_silhouette(sil)
