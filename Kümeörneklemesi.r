library(datasets)

veri <- USArrests
veri

library(cluster)
library(factoextra)

str(veri)

summary(veri)
#verinin 4 deðiþkeni de numeric

#deðiþken isimlerini deðiþtirmek

names(veri)[1] <- 'Cinayet'
names(veri)[2] <- 'Saldiri'
names(veri)[3] <- 'Kentsel_yuzde'
names(veri)[4] <- 'Tecavuz'

veri

cinayet_veri <- table(veri$Cinayet)
barplot(cinayet_veri, main="Tutuklama", xlab="Cinayet Verisi" )

saldiri_veri <- table(veri$Cinayet)
barplot(saldiri_veri, main="Tutuklama", xlab="Saldiri Verisi")

tecavuz_veri <- table(veri$Tecavuz)
barplot(tecavuz_veri, main="tutuklama", xlab="Tecavuz verisi")

veri_norm <- scale(veri)
head(veri_norm)
head(veri)

heatmap(veri_norm, Colv = NA, Rowv = NA, scale = 'none')

#Hiyerarþik Kümeleme

#uzaklýklarýn hesaplanmasý
distance_hesaplama <- dist(veri_norm, method = 'euclidean')
distance_hesaplama.matrix <- as.matrix(distance_hesaplama)
heatmap(distance_hesaplama.matrix)

hc1 <- hclust(distance_hesaplama, method = "complete" ) #uygulama

plot(hc1, cex = 0.9, hang = -5) #gösterim

sub_grp <- cutree(hc1, k = 4)
table(sub_grp)

library(dplyr)
veri %>%
  mutate(cluster = sub_grp) %>%
  head

veri_dendongram_sonuclu <- veri %>%
  mutate(cluster = sub_grp)

plot(hc1, cex = 0.7)
rect.hclust(hc1, k = 4, border = 2:5)

fviz_cluster(list(data = veri_norm, cluster = sub_grp))

#k-means Kümeleme

df <- USArrests

tanimlayici_ist<-data.frame(
  Min=apply(df, 2, min), 
  Med=apply(df, 2, median), 
  Mean=apply(df, 2, mean),
  SD=apply(df, 2, sd),
  Max=apply(df, 2, max)
)

tanimlayici_ist<-round(tanimlayici_ist, 1)
head(tanimlayici_ist)

library(factoextra)

set.seed(123)

fviz_nbclust(df, FUN = hcut, method = "silhouette")


set.seed(123)
km.res<-kmeans(df, 2, nstart=25)
print(km.res)

fviz_cluster(km.res, data=df)  

km.res$cluster

veri_2_kmeans_2_sonuclu <- df %>%
  mutate(cluster = km.res$cluster)

