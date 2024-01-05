#sintaks kmdeoid tugas statcon 2019-2021

Jabar=read.csv(file.choose(),header=TRUE,sep=",")
Jabar
#melakukan scalling data
jabar1 <- scale(Jabar[,2:5])#Standarisasi atau scalling data
jabar1
#package
library(fpc)
library(tidyverse) # data manipulation
library(cluster) # clustering algorithms
library(factoextra) # clustering algorithms & visualization
summary(is.na(jabar1)) #melihat missing value
summary(jabar1) #melihat deskriptif data
boxplot(jabar1) #melihat data outlier
#cek multiko
multikolinearitas<-cor(jabar1)
multikolinearitas
# pamk (penentuan jumlah cluster)
library(fpc)
pamk.result <- pamk(jabar1)
pamk.result
pamk.result$nc
#menampilkan grafik sillhouette
fviz_nbclust(jabar1, pam, method = "silhouette")
pam.hasil <- pam(jabar1, 3)
#menunjukkan penomoran atau label cluster dari masing-masing data
summary(pam.hasil)
#menunjukkan nilai rata-rata metode sihlouette dari masing-masing cluster,
#Metric menunjukkan metode jarak yang dipakai yaitu Eucledian Distance
pam.hasil$medoidss
pam.hasil$diss
#melihat label cluster jumlah penduduk
data.frame(Jabar$Kabupaten,pam.hasil$clustering)
data.frame(Jabar,pam.hasil$cluster)
#visualisasi kmedoidss dari dari masing-masing cluster
fviz_cluster(pam.hasil)
#menghitung nilai rata2
Jabar%>%
  mutate(Cluster = pam.hasil$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
#pengujian multiko
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(jabar1, histogram=TRUE, pch=19)

#melihat label berdasarkan pengelompokan 
label1<-data.frame(Jabar$Kabupaten,pam.hasil$clustering)
label1
kelompok1 <- subset(label1, pam.hasil.clustering==1)
kelompok1
kelompok2 <- subset(label1, pam.hasil.clustering==2)
kelompok2
kelompok3 <- subset(label1, pam.hasil.clustering==3)
kelompok3