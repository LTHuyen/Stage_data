data <- read.csv("C:/Users/huyen/Dropbox/Stage_data/ESX01/ESX01_Mars2019.csv",
                 sep = "
                 ;")
summary(data)
plot(data$CPU.usage,type = 'l')
plot.ts(data$CPU.usage)
library(FactoMineR)
library(factoextra)
df <- data.frame(data)
cor(df[,-1])
res <- PCA(df[-1])
res$eig
boxplot(df$CPU.usage)
boxplot.stats(df$CPU.usage)$out
(a1 <- which(df$CPU.usage %in% boxplot.stats(df$CPU.usage)$out))
df[733,1] ## date 31/03/2019 time 13:00
(a2 <- which(df$Ntwk_bytes.receiv %in% 
               boxplot.stats(df$Ntwk_bytes.receiv)$out))
### rien 
(a3 <- which(df$Ntwk_bytes.transmitted %in% 
               boxplot.stats(df$Ntwk_bytes.transmitted)$out))
## 229 667 668
df[229,1] ## date 10/03/2019 time 00:00
df[667,1] ## date 28/3/2019 time 18:00
df[668,1] ## date 28/3/2019 time 19:00
(a4 <- which(df$Used.memory %in% 
               boxplot.stats(df$Used.memory)$out))
(a5 <- which(df$Aver.write.latency.LUN_080 %in% 
               boxplot.stats(df$Aver.write.latency.LUN_080)$out))
intersect(a4,a5) ###- 484 485 653
df[484,1] ### date 21/03/2019 time 03:00
############################## segmentation 
library(segclust2d)
shift_seg <- segmentation(data,lmin = 30, Kmax= 10,seg.var='Ntwk_bytes.transmitted',
                          scale.variable = FALSE)
plot(shift_seg)


